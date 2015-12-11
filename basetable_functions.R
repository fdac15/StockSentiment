setwd("~/Dropbox/1Analytics_2015_fall/Reinforcement_learning/Final_Project")
library(dplyr)

X_prob = read.csv('probabilities.csv')
X_pred = read.csv('predictions.csv')
closeprices = read.csv('close_prices')
closeprices_2prior = read.csv('closeprices_w_2prior')
price_dif = read.csv('price_dif.csv')
price_dif_2prior = read.csv('price_dif_2prior')

X_prob_1 <-  X_prob[,names(closeprices)[names(closeprices) %in% names(X_prob)]]
names(X_prob_1)[-1] <- paste(names(X_prob_1)[-1], 'prob', sep = '_')
X_pred_1 <-  X_pred[,names(closeprices)[names(closeprices) %in% names(X_pred)]]
names(X_pred_1)[-1] <- paste(names(X_pred_1)[-1], 'pred', sep = '_')
X_prob_pred <- inner_join(X_prob_1, X_pred_1, by = 'Date') %>%
  inner_join(closeprices, by = 'Date')

#base table creator
tweet_stock_data <- function(stock, price = 'prob'){
  if(price == ''){
    sp = ''
  }else{
    sp = '_'
  }
  s_clss <- paste(stock, price, sep = sp)
  BAC <- X_prob_pred[,c('Date',stock)]
  twt_BA <- X_prob_pred[,c('Date', s_clss)]
  
  ts <- seq.POSIXt(as.POSIXct("2014-10-07", origin = 'EST'), as.POSIXct("2015-10-29", origin = 'EST'), by="day")
  ts <- as.Date(round(ts, units =  "days"))
  df <- data.frame(Date=ts)
  BAC$Date <- as.Date(BAC$Date)
  twt_BA$Date <- as.Date(twt_BA$Date)
  BACC <- full_join(df,BAC, by = 'Date')
  twt_BAC <- full_join(df,twt_BA, by = 'Date')
  
  for(i in 2:length(BACC[,1])){
    if(is.na(BACC[i,2])){
      BACC[i,2] <- BACC[i-1,2]
      twt_BAC[i,2] <- twt_BAC[i-1,2]
    }
  }
  
  
  names(BACC) <- c('Date', 's_cls')
  twt_BAC <- BACC
  names(twt_BAC) <- c('Date', 'stock')
  
  BACC_UD <- BACC %>%
    inner_join(twt_BAC, by = 'Date') %>%
    mutate(five = lead(s_cls, 5), ten =  lead(s_cls, 10), fteen =  lead(s_cls, 15)) %>%
    mutate(bi_5 = ifelse(s_cls> five, 1, 0), bi_10 = ifelse(s_cls> ten, 1, 0), bi_15 = ifelse(s_cls> fteen, 1, 0)) %>%
    mutate(twt = stock, twt_1 = lag(stock, 1), twt_2= lag(stock, 2), twt_3= lag(stock, 3), twt_4= lag(stock, 4), twt_5= lag(stock, 5), twt_6= lag(stock, 6), 
           twt_7= lag(stock, 7), twt_8= lag(stock, 8), twt_9= lag(stock, 9), twt_10= lag(stock, 10),
           twt_11= lag(stock, 11), twt_12= lag(stock, 12), twt_13= lag(stock, 13), twt_14= lag(stock, 14), twt_15= lag(stock))%>%
    mutate(twt_3_avg = (twt+twt_1+twt_2+twt_3),
           twt_5_avg = (twt+twt_1+twt_2+twt_3+ twt_4+ twt_5),
           twt_7_avg = (twt+twt_1+twt_2+twt_3+ twt_4+ twt_5+ twt_6+ twt_7),
           twt_9_avg = (twt+twt_1+twt_2+twt_3+ twt_4+ twt_5+ twt_6+ twt_7+ twt_8+ twt_9),
           twt_14_avg = (twt+twt_1+twt_2+twt_3+ twt_4+ twt_5+ twt_6+ twt_7+ twt_8+ twt_9+
                           twt_10+twt_11+twt_12+twt_13+twt_14),
           twt_3_7_avg = (twt_3+ twt_4+ twt_5+ twt_6+ twt_7),
           twt_7_10_avg =  (twt_7+ twt_8+ twt_9+ twt_10),
           twt_10_14_avg = (twt_10+twt_11+twt_12+twt_13+twt_14)) %>%
    group_by(Date)%>%
    mutate(twt_3_sd = sd(c(twt, twt_1,twt_2,twt_3)),
           twt_5_sd = sd(c(twt, twt_1,twt_2,twt_3, twt_4, twt_5)),
           twt_7_sd = sd(c(twt, twt_1,twt_2,twt_3, twt_4, twt_5, twt_6, twt_7)),
           twt_9_sd = sd(c(twt, twt_1,twt_2,twt_3, twt_4, twt_5, twt_6, twt_7, twt_8, twt_9)),
           twt_14_sd = sd(c(twt, twt_1,twt_2,twt_3, twt_4, twt_5, twt_6, twt_7, twt_8, twt_9,
                            twt_10,twt_11,twt_12,twt_13,twt_14)),
           twt_3_7_sd = sd(c(twt_3, twt_4, twt_5, twt_6, twt_7)),
           twt_7_10_sd =  sd(c(twt_7, twt_8, twt_9, twt_10)),
           twt_10_14_sd = sd(c(twt_10,twt_11,twt_12,twt_13,twt_14)))
  
  
  ###
  BACC_train_test_10 <- BACC_UD[-c(1:16),-c(1,2,3,4,5,6,8,9)]
  BACC_train_test_10$bi_5 <- as.factor(BACC_train_test_10$bi_5) 
#   
  # BACC_train_test_10 <- BACC_UD
  return(BACC_train_test_10)
}

#create basetable for prob and pred
prob_pred_base <- function(stock){
  BACC_train_test_prob <- tweet_stock_data(stock, price = 'prob')
  names(BACC_train_test_prob) <- paste(names(BACC_train_test_prob), 'prob', sep = '_')
  
  BACC_train_test_pred <- tweet_stock_data(stock, price = 'pred')
  names(BACC_train_test_pred) <- paste(names(BACC_train_test_pred), 'pred', sep = '_')
  
  BACC_train_test_10 <- cbind( BACC_train_test_prob, BACC_train_test_pred[-1])
  names(BACC_train_test_10)[1] <-  'bi_5'
  return(BACC_train_test_10 <- BACC_train_test_10[-c(368:372),])
}
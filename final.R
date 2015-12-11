setwd("~/Dropbox/1Analytics_2015_fall/DigitalArch/final")

library(data.table)
library(dplyr)
library(randomForest)
library(ROCR)
tweet <- fread('all_tweet.csv')
adjclose <- fread('pricedf.csv')

tweet$Date <- as.Date(tweet$V1)
adjclose$Date <- as.Date(adjclose$Date)
adjclose <- data.frame(adjclose)
tweet <- data.frame(tweet)
sort(apply(tweet[,-c(1, length(tweet[1,]))], 2, FUN = function(x) sum(abs(x))))

###Basetable building function
tweet_stock_data <- function(stock, price = F){
  s_clss <- paste(stock, 'Close', sep = '_')
  BAC <- adjclose[,c('Date',s_clss)]
  twt_BAC <- tweet[,c('Date', stock)]
  
  ts <- seq.POSIXt(as.POSIXct("2014-01-01", origin = 'EST'), as.POSIXct("2014-12-31", origin = 'EST'), by="day")
  
  ts <- as.Date(round(ts, units =  "days"))
  df <- data.frame(Date=ts)
  
  BACC <- full_join(df,BAC, by = 'Date')
  
  for(i in 2:length(BACC[,1])){
    if(is.na(BACC[i,2])){
      BACC[i,2] <- BACC[i-1,2]
    }
  }
  
  names(BACC) <- c('Date', 's_cls')
  if(price == F){
    names(twt_BAC) <- c('Date', 'stock')
  }else{
    twt_BAC <- BACC
    names(twt_BAC) <- c('Date', 'stock')
  }
  
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
 
  return(BACC_train_test_10)
  }
###



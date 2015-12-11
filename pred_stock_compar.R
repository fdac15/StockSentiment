pred_stock_compar <- function(stock, twt_only = F){
  set.seed(1337)
  BACC_train_test_10 <- tweet_stock_data(stock, price = T)
  
  predicts <- c() 
  labels <- c()
  for(i in 0:99){
    j = i*1
    BACC_train <- BACC_train_test_10[(j+2):(244+j), ]
    BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
    mod2 <- randomForest(bi_5~., data = BACC_train)
    predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
    labels <- c(labels, BACC_test$bi_5)
  }
  
  pred_test_boa_p <- prediction(predicts, labels)
  perf_test_boa_p <- performance(pred_test_boa_p,"tpr","fpr")
  plot(perf_test_boa_p,main =  stock)
  abline(0, 1)
  
  AUC_boa_p <- as.numeric(performance(pred_test_boa_p,"auc")@y.values)
  #legend(.6, .4, paste('AUC=' ,round(AUC_boa_p,4)))
  
  ###
  BACC_train_test_price <- tweet_stock_data(stock, price = T)
  names(BACC_train_test_price) <- paste(names(BACC_train_test_price), 'price', sep = '_')
  BACC_train_test_twt <- tweet_stock_data(stock)
  BACC_train_test_10 <- cbind(BACC_train_test_price[,-1], BACC_train_test_twt)
  
  predicts <- c() 
  labels <- c()
  for(i in 0:99){
    j = i*1
    BACC_train <- BACC_train_test_10[(j+2):(244+j), ]
    BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
    mod2 <- randomForest(bi_5~., data = BACC_train)
    predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
    labels <- c(labels, BACC_test$bi_5)
  }
  
  
  pred_test_boa_p_t <- prediction(predicts, labels)
  perf_test_boa_p_t <- performance(pred_test_boa_p_t,"tpr","fpr")
  par(new=TRUE)
  plot(perf_test_boa_p_t,col = 'red', main =  stock)
  #abline(0, 1)
  AUC_boa_p_t <- as.numeric(performance(pred_test_boa_p_t,"auc")@y.values)
#  legend(.5, .4, c(paste('AUC price only=' ,round(AUC_boa_p_t,4)),paste('AUC tweet and price=' ,round(AUC_boa_p,4))), lty = c(1,1), col = c('red','black'))

  if(twt_only == T){
    BACC_train_test_10 <- tweet_stock_data(stock)
    
    predicts <- c() 
    labels <- c()
    for(i in 0:99){
      j = i*1
      BACC_train <- BACC_train_test_10[(j+2):(244+j), ]
      BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
      mod2 <- randomForest(bi_5~., data = BACC_train)
      predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
      labels <- c(labels, BACC_test$bi_5)
    }
    
    pred_test_boa_t <- prediction(predicts, labels)
    perf_test_boa_t <- performance(pred_test_boa_t,"tpr","fpr")
    par(new=TRUE)
    plot(perf_test_boa_t,col = 'blue', main =  stock)
    #abline(0, 1)
    AUC_boa_t <- as.numeric(performance(pred_test_boa_t,"auc")@y.values)
    legend(.5, .4, c(paste('AUC price only=' ,round(AUC_boa_p,4)),paste('AUC tweet & price=' ,round(AUC_boa_p_t,4)),paste('AUC tweet only=' ,round(AUC_boa_t,4))), lty = c(1,1,1), col = c('black','red','blue'))
    
  }else{
    legend(.5, .4, c(paste('AUC price only=' ,round(AUC_boa_p,4)),paste('AUC tweet & price=' ,round(AUC_boa_p_t,4))), lty = c(1,1), col = c('red','black'))
  }
  return(list(AUC_boa_p_t = AUC_boa_p_t, AUC_boa_p = AUC_boa_p))
  }
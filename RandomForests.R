#bank of america
BACC_train_test_10 <- tweet_stock_data('BAC')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_boa <- prediction(predicts, labels)
perf_test_boa <- performance(pred_test_boa,"tpr","fpr")
plot(perf_test_boa,main =  'BOA')
abline(0, 1)

AUC_boa <- as.numeric(performance(pred_test_boa,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_boa,4)))

#AT&T
BACC_train_test_10 <- tweet_stock_data('T')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_t <- prediction(predicts, labels)
perf_test_t <- performance(pred_test_t,"tpr","fpr")
plot(perf_test_t,main =  'AT&T')
abline(0, 1)

AUC_T <- as.numeric(performance(pred_test_t,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_T,4)))

#Visa

BACC_train_test_10 <- tweet_stock_data('V')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_v <- prediction(predicts, labels)
perf_test_v <- performance(pred_test_v,"tpr","fpr")
plot(perf_test_v,main =  'Visa')
abline(0, 1)

AUC_V <- as.numeric(performance(pred_test_v,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_V,4)))
#MasterCard

#Visa

BACC_train_test_10 <- tweet_stock_data('MA')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_m <- prediction(predicts, labels)
perf_test_m <- performance(pred_test_m,"tpr","fpr")
plot(perf_test_m,main =  'MasterCard')
abline(0, 1)

AUC_M <- as.numeric(performance(pred_test_m,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_M,4)))
#Pepsi

BACC_train_test_10 <- tweet_stock_data('PEP')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_p <- prediction(predicts, labels)
perf_test_p <- performance(pred_test_p,"tpr","fpr")
plot(perf_test_p,main =  'Pepsi')
abline(0, 1)

AUC_P <- as.numeric(performance(pred_test_p,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_P,4)))
#IBM

BACC_train_test_10 <- tweet_stock_data('IBM')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_i <- prediction(predicts, labels)
perf_test_i <- performance(pred_test_i,"tpr","fpr")
plot(perf_test_i,main =  'IBM')
abline(0, 1)

AUC_I <- as.numeric(performance(pred_test_i,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_I,4)))

#C
BACC_train_test_10 <- tweet_stock_data('C')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_c <- prediction(predicts, labels)
perf_test_c <- performance(pred_test_c,"tpr","fpr")
plot(perf_test_c,main =  'citi')
abline(0, 1)

AUC_C <- as.numeric(performance(pred_test_c,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_C,4)))

#DISNy
BACC_train_test_10 <- tweet_stock_data('DIS')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_d <- prediction(predicts, labels)
perf_test_d <- performance(pred_test_d,"tpr","fpr")
plot(perf_test_d,main =  'Disney')
abline(0, 1)

AUC_D <- as.numeric(performance(pred_test_d,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_D,4)))


#CVS
BACC_train_test_10 <- tweet_stock_data('CVS')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_cv <- prediction(predicts, labels)
perf_test_cv <- performance(pred_test_cv,"tpr","fpr")
plot(perf_test_cv,main =  'CVS')
abline(0, 1)

AUC_CV <- as.numeric(performance(pred_test_cv,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_CV,4)))

#CVS
BACC_train_test_10 <- tweet_stock_data('LFC')

predicts <- c() 
labels <- c()
for(i in 0:99){
  j = i*1
  BACC_train <- BACC_train_test_10[(j+1):(244+j), ]
  BACC_test <- BACC_train_test_10[-c(1:(244+j), (244+j+2):353), ]
  mod2 <- randomForest(bi_5~., data = BACC_train)
  predicts <- c(predicts, predict(mod2, BACC_test, type = 'prob')[,2])
  labels <- c(labels, BACC_test$bi_5)
}

pred_test_cv <- prediction(predicts, labels)
perf_test_cv <- performance(pred_test_cv,"tpr","fpr")
plot(perf_test_cv,main =  'LFC')
abline(0, 1)

AUC_CV <- as.numeric(performance(pred_test_cv,"auc")@y.values)
legend(.6, .4, paste('AUC=' ,round(AUC_CV,4)))

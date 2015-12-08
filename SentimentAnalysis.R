library(data.table)
library(dplyr)
library(zoo)
library(randomForest)
library(AUC)

setwd(paste0(getwd(),'./UT - Business Analytics/Fall 2015/COSC 545 - Digital Archeology/FinalStockSentiment/StockSentiment'))

lf <- list.files(paste0(getwd(),'/PriceData'))

prls <- list()
i<-0
for (f in lf) {
  prdata <- read.csv(paste0(getwd(),'/PriceData/',f))
  if (f == 'BABA.csv') {
    next
  }
  i = i+1
  prls[[i]] <- data.frame(Date = as.Date(prdata$Date), Close = prdata$AdjClose, stringsAsFactors=FALSE)
  names(prls[[i]])[2] <- paste0(substr(f,1,nchar(f)-4))
}

pricedf <- Reduce(function(x,y) merge(x,y,by="Date"), prls)
write.csv(pricedf,file=paste0(getwd(),'/pricedf.csv'),row.names=FALSE)
head(pricedf,1)

tweets <- fread(paste0(getwd(),'/all_tweet.csv'))
tweets <- data.frame(tweets)[,-which(names(tweets) %in% "BABA")]
names(tweets)[1] <- 'Date'
tweets$Date <- as.Date(tweets$Date)
head(tweets,1)


(first.end.date <- max(pricedf$Date[which(pricedf$Date <= as.Date('2014-06-30')-14)]))
(start.date <- min(pricedf$Date))

#loop through each week from first.end.date to the end of 2014
#loop again for each symbol to create variables of lagged sentiment values using different aggregations (last day, last week, etc)
#   and the symbol's response variable for that row (i.e., ifelse( (price.at.date + 7)/(price.at.date)>1, 1, 0))
#at end of both loops, row bind the results to create the basetable at end.date and build a randomForest model against the response
#also at end of both loops, test the model at (ie., ifelse( (price.at.date + 14)/(price.at.date+7)>1, 1, 0) )

auc.vector <- c(); fpr <- c(); tpr <- c(); tnr <- c(); pvalues<-c()
tm <- 0
for (end.date in seq(first.end.date,as.Date(max(pricedf$Date))-7,by='1 week')) {
  tm<-tm+1
  train.df.list <- list()
  i<-0
  for (symbol in names(tweets)[-1]) {
    i<-i+1
    #create response variable
    train.prices <- pricedf[,which(names(pricedf) %in% c('Date',symbol))] 
    
    train.prices <- train.prices %>%
      mutate(Price.7.Ahead = c(train.prices[-c(1:7),2],rep(NA,7)),
             Response = ifelse(Price.7.Ahead/train.prices[,2] > 1,1,0)
      )
    
    train.prices <- train.prices[which(train.prices$Date >= start.date & train.prices$Date <= end.date),]
    
    #subset data to be inside current time window
    sentiment.subset <- tweets[which(tweets$Date >= start.date & tweets$Date <= end.date),
                               which(names(pricedf) %in% c('Date',symbol))]
    
    sentiment.subset <- sentiment.subset %>% arrange(Date) %>%
      mutate(Day1 = lag(sentiment.subset[,grep(symbol,names(sentiment.subset))],1),
             Days2 = lag(sentiment.subset[,grep(symbol,names(sentiment.subset))],2),
             Days3 = lag(sentiment.subset[,grep(symbol,names(sentiment.subset))],3),
             MaxWeek = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=max,na.rm=T,fill = NA),
             MinWeek = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=min,na.rm=T,fill = NA),
             MaxMonth = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=max,na.rm=T,fill = NA),
             MinMonth = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=min,na.rm=T,fill = NA),
             MnWeek1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=mean,na.rm=T,fill = NA),
             MnWeek2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=14, FUN=mean,na.rm=T,fill = NA),
             MnWeek3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=21, FUN=mean,na.rm=T,fill = NA),
             MnMonth1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=mean,na.rm=T,fill = NA),
             MnMonth2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=60, FUN=mean,na.rm=T,fill = NA),
             MnMonth3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=89, FUN=mean,na.rm=T,fill = NA),
             SdWeek1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=sd,na.rm=T,fill = NA),
             SdWeek2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=14, FUN=sd,na.rm=T, fill = NA),
             SdWeek3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=21, FUN=sd,na.rm=T, fill = NA),
             SdMonth1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=sd,na.rm=T, fill = NA),
             SdMonth2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=60, FUN=sd,na.rm=T, fill = NA),
             SdMonth3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=89, FUN=sd,na.rm=T, fill = NA)
      ) 
    
    #full join sentiment.subset with the response variable on date
    train.df <- inner_join(sentiment.subset[,-2],train.prices[c('Date','Response')],by='Date')
    train.df <- train.df[complete.cases(train.df),]
    train.df$Symbol <- rep(paste0(symbol),nrow(train.df))
    train.df.list[[i]] <- train.df
  }
  #rbind all the symbol's sentiment data.frames
  train.df.r <- Reduce(function(x,y) rbind(x,y),train.df.list)
  train.df.r <- train.df.r[complete.cases(train.df.r),]
  
  #build randomForest model for time window, t
  RFmod <- randomForest(x=train.df.r[,-grep('Date|Symbol|Response',names(train.df.r))],y=as.factor(train.df.r$Response),ntree=500,
                        importance=TRUE)
  
  #test the model on the next time step forward
  test.df.list <- list()
  i<-0
  for (symbol in names(tweets)[-1]) {
    i<-i+1
    #create response variable
    test.prices <- pricedf[,which(names(pricedf) %in% c('Date',symbol))]
    
    test.prices <- test.prices %>%
      mutate(Price.7.Ahead = c(test.prices[-c(1:7),2],rep(NA,7)),
             Response = ifelse(Price.7.Ahead/test.prices[,2] > 1,1,0)
      )

    test.prices <- test.prices[which(test.prices$Date >= start.date & test.prices$Date <= end.date+7),]
    
    #subset data to be inside current time window
    sentiment.subset <- tweets[which(tweets$Date >= start.date & tweets$Date <= end.date+7),
                               which(names(pricedf) %in% c('Date',symbol))]
    
    sentiment.subset <- sentiment.subset %>% arrange(Date) %>%
      mutate(Day1 = lag(sentiment.subset[,grep(symbol,names(sentiment.subset))],1),
             Days2 = lag(sentiment.subset[,grep(symbol,names(sentiment.subset))],2),
             Days3 = lag(sentiment.subset[,grep(symbol,names(sentiment.subset))],3),
             MaxWeek = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=max,na.rm=T,fill = NA),
             MinWeek = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=min,na.rm=T,fill = NA),
             MaxMonth = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=max,na.rm=T,fill = NA),
             MinMonth = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=min,na.rm=T,fill = NA),
             MnWeek1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=mean,na.rm=T,fill = NA),
             MnWeek2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=14, FUN=mean,na.rm=T,fill = NA),
             MnWeek3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=21, FUN=mean,na.rm=T,fill = NA),
             MnMonth1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=mean,na.rm=T,fill = NA),
             MnMonth2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=60, FUN=mean,na.rm=T,fill = NA),
             MnMonth3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=89, FUN=mean,na.rm=T,fill = NA),
             SdWeek1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=7, FUN=sd,na.rm=T, fill = NA),
             SdWeek2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=14, FUN=sd,na.rm=T, fill = NA),
             SdWeek3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=21, FUN=sd,na.rm=T, fill = NA),
             SdMonth1 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=30, FUN=sd,na.rm=T, fill = NA),
             SdMonth2 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=60, FUN=sd,na.rm=T, fill = NA),
             SdMonth3 = rollapplyr(data=sentiment.subset[,grep(symbol,names(sentiment.subset))], width=89, FUN=sd,na.rm=T, fill = NA)
      ) 
    
    #full join sentiment.subset with the response variable on date
    test.df <- inner_join(sentiment.subset[,-2],test.prices[c('Date','Response')],by='Date')
    test.df <- test.df[which(test.df$Date > end.date & test.df$Date <= end.date+7),]
    test.df$Symbol <- rep(paste0(symbol),nrow(test.df))
    test.df.list[[i]] <- test.df
  }
  #rbind all the symbol's sentiment data.frames
  test.df.r <- Reduce(function(x,y) rbind(x,y),test.df.list)
  if (sum(complete.cases(test.df)) == 0) {break}
  test.df.r <- test.df.r[complete.cases(test.df.r),]
  
  #predictions one step forward
  pred <- predict(RFmod,newdata=test.df.r[,-grep('Date|Symbol|Response',names(test.df.r))],type='prob')[,2]
  auc.vector[tm] <- AUC::auc(roc(pred,as.factor(test.df.r$Response)))
  pdf(file=paste0(getwd(),'/plots/roc',tm,'.pdf'))
  plot(roc(pred,as.factor(test.df.r$Response)))
  title(paste('Train Period:',start.date,'to',as.Date(end.date),'\n','Test Period:',as.Date(end.date+1),'to',as.Date(end.date+7)))
  dev.off()
  pred <- predict(RFmod,newdata=test.df.r[,-grep('Date|Symbol|Response',names(test.df.r))],type='response')
  cmat <- as.matrix(table(pred,test.df.r$Response))
  fpr[tm] <- cmat[2,1]/sum(cmat[1,1],cmat[2,1])
  tpr[tm] <- cmat[2,2]/sum(cmat[1,2],cmat[2,2])
  tnr[tm] <- cmat[1,1]/sum(cmat[1,1],cmat[1,2])
  pvalues[tm] <- chisq.test(cmat)$p.value
  write.csv(cmat,file=paste0(getwd(),'/confusion/confusion_',tm,'.csv'),row.names = TRUE)
}
(mean(auc.vector))
(mean(tpr))
(mean(fpr))
(mean(tnr))
# (mean(pvalues))
write.csv(auc.vector,file=paste0(getwd(),'/aucs.csv'),row.names = FALSE)
write.csv(tpr,file=paste0(getwd(),'/tprs.csv'),row.names = FALSE)
write.csv(fpr,file=paste0(getwd(),'/fprs.csv'),row.names = FALSE)
write.csv(tnr,file=paste0(getwd(),'/tnrs.csv'),row.names = FALSE)
write.csv(pvalues,file=paste0(getwd(),'/pvalues.csv'),row.names = FALSE)

boxplot(pvalues)



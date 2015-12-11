for( i in c('PEP', 'T', 'BAC', 'V', 'MA')){
  pred_stock_compar(i, twt_only = T)
}

for( i in c('DIS', 'CVS', 'BMY', 'BP','CHL', 'CVX', 'GE')){
  pred_stock_compar(i, twt_only = T)
}

for( i in c('IBM', 'JPM', 'MCD', 'ORCL','PFE','PG','PM')){
  pred_stock_compar(i, twt_only = T)
}


asdf <- c(.764-.7844,.8117-.7267,.754-.7335,.7954-.797,
.7437-.74,.7437-.76,.8277-.821,.8779-.845,
.865-.862,.847-.85,.87-.85,.8444-.84,.8316-.819, 
.794-.786, .7897-.8, .8417- .80, .7786-.778, .6879-.71,
.772-.76)

#full comparison test of all 48 stocks
auc_compar <- list()
stock <- unlist(lapply(strsplit(names(adjclose)[-c(1,3)],split = '_'), FUN = function(x) x[1]))
stock <- stock[!(stock == 'HSBC')]
for( i in stock){
  auc_compar <- append(auc_compar, list(unlist(pred_stock_compar(i, twt_only = F))))
}

auc_p_t <- unlist(lapply(auc_compar, FUN = function(x) x[1]))
auc_p <- unlist(lapply(auc_compar, FUN = function(x) x[2]))
t.test(auc_p_t,auc_p,alternative="greater", paired=TRUE)

mean(auc_p_t-auc_p)/mean(auc_p)

for(i in c('CVX', 'ORCL', 'MCD', "GE")){
  stock = i
  s_clss <- paste(stock, 'Close', sep = '_')
  
  plot(tweet[,stock], type = 'l', main = stock,ylim=c(-30,30))
  par(new = T)
  plot(smooth.spline(tweet[,stock], spar = 0.6389564), type = 'l', col = 'blue', lwd = 3,ylim=c(-30,30),ylab = '')
  abline(v = 244, col = 'red')
  legend(100, -20, c('Stock Price', 'Tweet Sentiment'), lty = c(1,1), col = c('purple', 'black'))
  par(new = T)
  plot(adjclose[,s_clss], type = 'l', main = '', col = 'purple', axes = F, ylab = '')
  from_p  = range(adjclose[,s_clss])[1]
  to_p = range(adjclose[,s_clss])[2]
  axis(side = 4, at=round(seq(from_p, to_p,(to_p-from_p)/6),0), labels=round(seq(from_p, to_p,(to_p-from_p)/6),0) )
  }


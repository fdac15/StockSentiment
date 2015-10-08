#Query Price Data for top 50 Stocks on NYSE

#load necessary packages
if (require('quantmod')==FALSE) install.packages('quantmod');require('quantmod') #for stock price history
if (require('data.table')==FALSE) install.packages('data.table');require('data.table') #for fast csv reads

#read in largest 50 stocks on the NYSE
#this file came from:
#http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=ALL&exchange=NYSE&sortname=marketcap&sorttype=1
NYSElist <- fread('C:/Users/mooregm/Desktop/companylist.csv',select = c("Symbol","Name"), nrows = 50)

#initialize list to store all price dataframes
datalist <- c()
#loop over every stock, query price data, and store in datalist
for (stock in NYSElist$Symbol) {

  #query price data (Yahoo is default)
  getSymbols(stock, from = '2014-01-01', to = '2014-12-31')
  #'getSymbols' returns open/high/low/close/volume/adjustedClose by day in an object with same name as the symbol name
  
  #want to store the returned object in the datalist
  #to get the object, must evaluate the symbol's string name to get the exact object (use function 'mget' to evaluate the string to object)
  object <- data.frame(mget(stock))
  #convert to data.frame and rename columns
  object <- data.frame(Date = rownames(object), Open = object[,1], High = object[,2], 
                       Low = object[,3], Close = object[,4], Volume = object[,5], AdjClose = object[,6])
  
  #write each price dataframe to a csv file
  write.table(x = object,file=paste0('./PriceData/',stock,'.csv'), sep=",", row.names=FALSE) 
  
  #remove symbol after storing and writing to csv
  rm(list=setdiff(ls(),c("stock","NYSElist")))
  
  #sleep 5 seconds after every 10 queries
  if (i%%10 == 0) {Sys.sleep(5)}
}

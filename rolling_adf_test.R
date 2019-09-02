# Rolling ADF function 
require(data.table)

SPY <- read.csv("//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/moo/SPY.csv",stringsAsFactors = FALSE,header = TRUE)

# Rolling ADF test
library(zoo)
library(tseries)
library(lubridate)

adftest <- function(x)
{
  adfPvalue <-  adf.test(x)$p.value  #statistic / p.value 
  adfPvalue
}

PValue <- function(x) {
  Pvalue <- rollapply(x, 252, adftest)
  Pvalue
}

# pad with NA to make same length as original SPY series
nas <- rep(NA,251)

adf <- PValue(SPY$Adj.Close)
adf.vec <- c(nas,adf)
head(adf.vec)
Date <- SPY$Date
plot.df <- data.frame(Date,adf=adf.vec,stringsAsFactors = FALSE)
plot.df$Date <- ymd(plot.df$Date)
#plot.df$sma <- SMA(plot.df$adf,20)  sma
#plot.df <- subset(plot.df, Date >= as.POSIXct("2015-01-01") ) # subset by date
plot(plot.df$Date,plot.df$adf,type="l",main="Test Value - Rolling Augmented Dickey Fuller Test")
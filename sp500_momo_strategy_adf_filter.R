# SPX Momentum Strategy with ADF Filter
require(data.table)
library(zoo)
library(tseries)
library(lubridate)
require(TTR)
require(dplyr)
require(xts)

SPX <- read.csv("//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/SPY.csv",stringsAsFactors = FALSE,header = TRUE)
SPX$Date <- ymd(SPX$Date)
# Rolling ADF test

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

adf <- PValue(SPX$Close)
adf.vec <- c(nas,adf)

df <- data.frame(SPX,"adf" = adf.vec)
#Date <- SPX$Date
#plot.df <- data.frame(Date,adf=adf.vec,stringsAsFactors = FALSE)
#plot.df$Date <- ymd(plot.df$Date)
#plot.df$sma <- SMA(plot.df$adf,20)  sma
#plot.df <- subset(plot.df, Date >= as.POSIXct("2015-01-01") ) # subset by date
#plot(plot.df$Date,plot.df$adf,type="l",main="Test Value - Rolling Augmented Dickey Fuller Test")

# Create sma 
df$sma <- SMA(df$Close,200)
df$adf_sma <- SMA(df$adf,39)
plot(df$Date,df$adf_sma,type='l')

# Create signal no filter 
df$long_sig <- ifelse(df$Close > df$sma, 1,0)
# Create signal with ADF filter 
df$adf_filter_signal <- ifelse(df$Close > df$sma & df$adf_sma > 0.07, 1,0)
plot(df$Date,df$adf,type="l")

df1 <- subset(df, Date >= as.POSIXct("2015-01-01") ) 
head(df1)
# Calcualte open to close returns 
# Buy at the open 
df$open_ret <- ROC(df$Open, n = 1, type = c("discrete"))
# close to close returns
df$close_ret <- ROC(df$Close, n = 1, type = c("discrete"))

df <- subset(df, Date >= as.POSIXct("1950-01-01") ) 

# Calculate equity curves
# Buy open and sel on the close
# no filter
df <- df %>%
  dplyr::mutate(RunID = rleid(long_sig)) %>%
  group_by(RunID) %>%
  dplyr::mutate(eq_curve_1 = ifelse(long_sig == 0, 0,
                                               ifelse(row_number() == 1, open_ret, close_ret))) %>%
  ungroup() %>%
  select(-RunID)

#ADF filter
df <- df %>%
  dplyr::mutate(RunID = rleid(adf_filter_signal)) %>%
  group_by(RunID) %>%
  dplyr::mutate(eq_curve_adf_filt = ifelse(adf_filter_signal == 0, 0,
                                  ifelse(row_number() == 1, open_ret, close_ret))) %>%
  ungroup() %>%
  select(-RunID)

# Pull select columns from data frame to make XTS whilst retaining formats
xts1 = xts(df$eq_curve_1 ,order.by=as.Date(df$Date, format="%m/%d/%Y"))
xts2 = xts(df$eq_curve_adf_filt ,order.by=as.Date(df$Date, format="%m/%d/%Y"))
xts3 = xts(df$close_ret ,order.by=as.Date(df$Date, format="%m/%d/%Y"))
# Join XTS together
compare <- cbind(xts1,xts2,xts3)

# Use the PerformanceAnalytics package for trade statistics

require(PerformanceAnalytics)
colnames(compare) <- c("Long over 189sma - no filter","ADF Filter","Buy and Hold")
charts.PerformanceSummary(compare,main="Cumulative Returns", wealth.index=TRUE, colorset=rainbow12equal)
performance.table <- rbind(table.AnnualizedReturns(compare),maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
drawdown.table <- rbind(table.Drawdowns(xts2))

logRets <- log(cumprod(1+xts2))
chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="Log Returns")


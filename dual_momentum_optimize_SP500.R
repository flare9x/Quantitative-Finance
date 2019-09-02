# Dual Momentum 
# Andrew Bannerman 11.7.2017

# Long Term S&P500 - Switch To Bonds
# Andrew Bannerman 10.8.2017

require(lubridate)
require(dplyr)
require(magrittr)
require(TTR)
require(zoo)
require(data.table)
require(xts)
require(PerformanceAnalytics)
require(sendmailR)
require(alphavantager)


# Data path
data.dir <- "D:/R Projects/Final Scripts/Month Momentum/Data"
SPX <- paste(data.dir,"$SPX.csv",sep="/")

# Read data
read.spx <- read.csv(SPX,header=TRUE, sep=",",skip=0,stringsAsFactors=FALSE)

head(read.spx)

# Convert Values To Numeric
cols <-c(3:8)
read.spx[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))

#Convert Date Column [1]
read.spx$Date <- ymd(read.spx$Date)
df <- data.frame(read.spx)


# Use TTR package to create rolling SMA n day moving average
# Create function and loop in order to repeat the desired number of SMAs for example 2:30
getSMA <- function(numdays) {
  function(df) {
    ROC(df[,"Close"], n=numdays,type = "discrete")
  }
}
# Create a matrix to put the SMAs in
sma.matrix <- matrix(nrow=nrow(df), ncol=0)

# Loop for filling it
for (i in 1:20) {
  sma.matrix <- cbind(sma.matrix, getSMA(i)(df))
}

# Rename columns
colnames(sma.matrix) <- sapply(1:20, function(n)paste("lagged.return.month.n", n, sep=""))

# Bind to existing dataframe
df <-  cbind(df, sma.matrix)

# Convert all NA to 0
df[is.na(df)] <- 0

# Calculate Returns from open to close
df$spx.ocret <- apply(df[,c('Open', 'Close')], 1, function(x) { (x[2]-x[1])/x[1]} )

# Calculate Close-to-Close returns
df$spx.clret <- ROC(df$Close)
df$spx.clret[1] <- 0

# Add leverage multiplier
#df$spx.clret <- df$spx.clret 

# Subset Date
df <- subset(df, Date >= as.POSIXct("1978-01-01") ) 
colnames(df)

mo.ret <- 1:20
i=1
# Initialize data frame
data_output_df <- data.frame() # Daily Differences, Running 5 day count until FDA date
# Optimize #########
optIMIZE = function(x){
 #spx.sma <- df[,paste0("close.sma.n", sma[i])]
  
  # Enter buy / sell rules
  #df$signal.long.stocks <- ifelse(df$lagged.return.month.n12 > 0, 1,0)
  df$signal.long.stocks <- ifelse(df[,paste0("lagged.return.month.n", mo.ret[i])] > 0, 1,0)
  
  # lag signal by one forward day to signal entry next month
  df$signal.long.stocks <- lag(df$signal.long.stocks,1) # Note k=1 implies a move *forward*
  
  df[is.na(df)] <- 0  # Set NA to 0
  
  #Plot VIFNX Monthly with 9sma
  plot(df$Date, df$Close, col = ifelse(df$lagged.return.month.n12 < 0,'red','black'), pch = 10, cex=.5,ylab="SPX Monthly Close",main="SPX Below Monthly 12sma - 1928 To Present",log = "y", sub="Red Denotes Times Where 12 month return was negative")
  
  # Calculate equity curves
  # Long Stocks
  df <- df %>%
    dplyr::mutate(RunID = rleid(signal.long.stocks)) %>%
    group_by(RunID) %>%
    dplyr::mutate(long.stocks.equity.curve = ifelse(signal.long.stocks== 0, 0,
                                                    ifelse(row_number() == 1, spx.ocret, spx.clret))) %>%
    ungroup() %>%
    select(-RunID)
  
  # Pull select columns from data frame to make XTS whilst retaining formats
  xts1 = xts(df$long.stocks.equity.curve, order.by=as.Date(df$Date, format="%m/%d/%Y"))
  xts2 = xts(df$spx.clret, order.by=as.Date(df$Date, format="%m/%d/%Y")) 
  
  # Join XTS together
  compare <- cbind(xts1,xts2)
  
  # Use the PerformanceAnalytics package for trade statistics
  
  require(PerformanceAnalytics)
  colnames(compare) <- c("Long when current month is higher than previous 12 month","Buy And Hold")
  charts.PerformanceSummary(compare,main="Long when current month is higher than previous 12 month", wealth.index=TRUE, colorset=rainbow12equal)
  performance.table <- rbind(table.AnnualizedReturns(compare),maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
  drawdown.table <- rbind(table.Drawdowns(xts2))
  #dev.off()
  logRets <- log(cumprod(1+compare))
  chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="Log Returns")
  
  print(performance.table)
  print(drawdown.table)
  cum.ret <- Return.cumulative(xts1, geometric = TRUE)
  annualized <- Return.annualized(xts1, scale = NA, geometric = TRUE)
  dd <- maxDrawdown(xts1)
  sharpe <- SharpeRatio.annualized(xts1, Rf = 0, scale = NA, geometric = TRUE)

  
  # Create data output of rep and close.diff columns rbind
  data_output_df <- data.frame("Annualized Return" = annualized,"Annualized Sharpe" = sharpe,"Cumulative Return" = cum.ret,"Maximum Draw Down" = dd)
  
}

for (i in 1:length(mo.ret)){    # Length of optimization
  tryCatch({
    temp <- optIMIZE(mo.ret[[i]])
    rownames(temp) <- paste0("",mo.ret[i])
    #cum_ret <- rbind.data.frame(cum_ret, temp)
    data_output_df <- rbind.data.frame(data_output_df,temp)
    ptm0 <- proc.time()
    Sys.sleep(0.1)  
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

i=1

# Join SMA number to data frame
data_output_df <- data.frame(data_output_df,mo.ret)
colnames(df)

# Plot 
colnames(data_output_df)
library(ggplot2)
ggplot(data=data_output_df, aes(x=mo.ret,Maximum.Draw.Down))+
  geom_bar(stat="identity")+
  theme_classic()+
  scale_x_continuous(breaks = seq(min(data_output_df$mo.ret), max(data_output_df$mo.ret)))
scale_y_continuous(breaks = seq(min(data_output_df$Maximum.Draw.Down), max(data_output_df$Maximum.Draw.Down),by=0.0010))

#write.csv(df,file="D:/R Projects/momo.csv")

# Create Scatter Plot of 1 day S&P500 Returns 
boxplot(df$spx.clret, main="Tail Risk - S&P500 Daily Returns",col=c("gold"))
# Maximum Loss and Gain 
min(df$spx.clret)
max(df$spx.clret)
# Location of min/max
min.loc <- which.min(df$spx.clret)
max.loc <- which.max(df$spx.clret)
# Date of Min 
min <- df$Date[min.loc]
max <- df$Date[max.loc]
min
max

colnames(df)

# Count how many 1 day losss 
loss.5 <- sum(df$long.stocks.equity.curve <= -.05)
loss.10 <- sum(df$long.stocks.equity.curve <= -.10)
loss.15 <- sum(df$long.stocks.equity.curve <= -.15)
loss.20 <- sum(df$long.stocks.equity.curve <= -.20)
loss.25 <- sum(df$long.stocks.equity.curve <= -.25)
loss.30 <- sum(df$long.stocks.equity.curve <= -.30)
loss.35 <- sum(df$long.stocks.equity.curve <= -.35)
loss.40 <- sum(df$long.stocks.equity.curve <= -.40)

one.day.loss.df <- cbind(loss.5,loss.10,loss.15,loss.20,loss.25,loss.30,loss.35)
barplot(one.day.loss.df, main="1 Day Loss % - Total Number Days Below Thresholds (Long above 12 month sma", cex.main=".8")

# Send Email When Sell Signal Generated 


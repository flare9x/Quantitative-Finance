# Train / Test Set VXV / VXMT Strategy 
# Andrew Bannerman 

################################################
# Procedure 
# 1. Split data into train / test sets 
# 2. Select 1 year look back for each train set (252 days)
# 3. Select 6 months test set (126)
# 4. Re-calibrate on a moving window through entire time series 
#################################################

# Required Packages 
library(readxl)
require(xts)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(lubridate)
require(magrittr)
require(scales)
require(reshape2)
require(PerformanceAnalytics)
require(dplyr)
require(TTR)

#################################################
# 1. Load Data 
#################################################
# Load Syntehtic and join to alpha vantage adjusted prices 
# Load synthetic VXX and XIV data 
synth <- read_excel("D:/R Projects/Final Scripts/VIX_term_structure/vix-funds-models-no-formulas.xls", col_names = TRUE)
synth1 <- read_excel("D:/R Projects/Final Scripts/VIX_term_structure/vix-mt-funds-models-no-formulas.xls", col_names = TRUE)
synth <- as.data.frame(synth)
synth1 <- as.data.frame(synth1)
# Extract synthetic series 
vxx.synth <- data.frame(synth$Date, synth$'VXX calc')
xiv.synth <- data.frame(synth$Date, synth$'XIV calc')
ziv.synth <- data.frame(synth1$Date, synth1$'ZIV calc')
vxz.synth <- data.frame(synth1$Date, synth1$'VXZ calc')
colnames(vxx.synth)[1] <- "Date"
colnames(vxx.synth)[2] <- "vxx_close"
colnames(xiv.synth)[1] <- "Date"
colnames(xiv.synth)[2] <- "xiv_close"
colnames(ziv.synth)[1] <- "Date"
colnames(ziv.synth)[2] <- "ziv_close"
colnames(vxz.synth)[1] <- "Date"
colnames(vxz.synth)[2] <- "vxz_close"
vxx.synth$Date <- ymd(vxx.synth$Date)
xiv.synth$Date <- ymd(xiv.synth$Date)
ziv.synth$Date <- ymd(ziv.synth$Date)
vxz.synth$Date <- ymd(vxz.synth$Date)

# Download data from alphavantage
VXX <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=VXX&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
VXX$timestamp <- ymd(VXX$timestamp)   #Lubridate to change character date to date format
VXX <- arrange(VXX,timestamp)   #dplyr to sort data frame by date ascending order
colnames(VXX)[1] <- "Date"
VXX$Date <- ymd(VXX$Date)
VXX <- as.data.frame(VXX)

XIV <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=XIV&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
XIV$timestamp <- ymd(XIV$timestamp)   #Lubridate to change character date to date format
XIV <- arrange(XIV,timestamp)   #dplyr to sort data frame by date ascending order
colnames(XIV)[1] <- "Date"
XIV$Date <- ymd(XIV$Date)
XIV <- as.data.frame(XIV)

# Join sythentic data to alpha vantage 
vxx.synth <- subset(vxx.synth, Date <= as.POSIXct("2009-01-29"))
xiv.synth <- subset(xiv.synth, Date <= as.POSIXct("2010-11-29"))
# Subset only date and close from alpha vantage data 
VXX <- VXX[ -c(2:5, 7:9) ]  # subset adjusted close
XIV <- XIV[ -c(2:5, 7:9) ]  # subset adjusted close
colnames(VXX)[2] <- "vxx_close"
colnames(XIV)[2] <- "xiv_close"

# row bind 
VXX <- rbind(vxx.synth,VXX)
XIV <- rbind(xiv.synth,XIV)
df <- cbind(VXX,XIV)

# Download VXV and VXMT data from CBOE website
# Download VXV Data From CBOE website 
VXV_cboe <- fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vix3mdailyprices.csv")
VXV_cboe <- as.data.frame(VXV_cboe)
VXV_cboe <- VXV_cboe[3:nrow(VXV_cboe), ]
colnames(VXV_cboe)[1] = "Date"
colnames(VXV_cboe)[2] = "vxv_cboe_open"
colnames(VXV_cboe)[3] = "vxv_cboe_high"
colnames(VXV_cboe)[4] = "vxv_cboe_low"
colnames(VXV_cboe)[5] = "vxv_cboe_close"
VXV_cboe$Date <- mdy(VXV_cboe$Date)
cols <-c(2:5)
VXV_cboe[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))

# Download VXMT Data from CBOE website
VXMT_cboe <- fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vxmtdailyprices.csv")
VXMT_cboe <- as.data.frame(VXMT_cboe)
VXMT_cboe <- VXMT_cboe[3:nrow(VXMT_cboe), ]
colnames(VXMT_cboe)[1] = "Date"
colnames(VXMT_cboe)[2] = "vxmt_cboe_open"
colnames(VXMT_cboe)[3] = "vxmt_cboe_high"
colnames(VXMT_cboe)[4] = "vxmt_cboe_low"
colnames(VXMT_cboe)[5] = "vxmt_cboe_close"
VXMT_cboe$Date <- mdy(VXMT_cboe$Date)
cols <-c(2:5)
VXMT_cboe[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))

# Join VIX, VIX3m (VXV) and VXMT CBOE data to ETF df
cboe.df <- merge(VIX_cboe,VXV_cboe, by="Date")
cboe.df <- merge(cboe.df,VXMT_cboe, by="Date")
start_date <- cboe.df$Date[1]
# Subset Strategy To Start At VXMT Date 
df <- subset(df, Date >= as.POSIXct(start_date) )
df <- df[,c(-3)] # Drop unused dates
df <- full_join(df, cboe.df, by = c("Date" = "Date"))

####################################################################
# Create VXV VXMT Ratio and ratio SMA 
###################################################################
# VXV / VXMT Ratio 
df$vxv.vxmt.ratio <- df$vxv_cboe_close / df$vxmt_cboe_close
# Calculate Close-to-Close returns
df$vxx.close.ret <- ROC(df$vxx_close, type = c("discrete"))
df$xiv.close.ret <- ROC(df$xiv_close, type = c("discrete"))

# Calculate SMA of VXV , VXMT ratio 
sma.lookback <- 2:170
getSMA <- function(y) {
  function(x) {
    SMA(df[,"vxv.vxmt.ratio"], sma.lookback[i])    # Calls TTR package to create SMA
  }
}
# Create a matrix to put the SMAs in
sma.matrix <- matrix(nrow=nrow(df), ncol=0)
# Loop for filling it
for (i in 1:length(sma.lookback)) {
  sma.matrix <- cbind(sma.matrix, getSMA(i)(x))
}
# Rename columns
colnames(sma.matrix) <- sapply(sma.lookback, function(n)paste("ratio.sma.n", n, sep=""))
# Bind to existing dataframe
df <-  cbind(df, sma.matrix)

####################################################################
# Split Data Into Train and Test Sets
# 252 days train , 126 days test 
##################################################################
# Set row numbers for splitting to train and test sets
total.row <- nrow(df) # total rows
train.length <- 252
test.length <- 125
train.list <- list()
test.list <- list()
# Set start and ending rows for subsetting by row
train_num_set <- seq(1, nrow(df), by=test.length+1)
test_num_set <- seq(train.length+1, nrow(df), by=test.length+1)
train.list<- lapply(train_num_set, function(i) df[c(i:(i+train.length)),])
test.list<- lapply(test_num_set, function(i) df[c(i:(i+test.length)),])


####################################################################
# Run Optimizations over all training windows
# Select optimal sharpe ratio 
####################################################################
train.set.results <- data.frame()
i=1
train.loop.length[i]
train.loop.length <- rep(1:length(train.list),each=length(sma.lookback)) # run through the dfs the same length as the number of smas
sma.loop.length <- rep(2:length(sma.lookback),length(train.list)) # run each sma over every df
train.xts <- list()
# Loop for running all smas through all train sets, note need to wrap train.loop.length and sma.loop.lenght in [[i]]
for (i in 1:length(train.loop.length)) {
  tryCatch({
    df.num <- train.loop.length[[i]]
  # Enter buy / sell rules
  train.temp.df <- data.frame(Date=train.list[[df.num]]$Date)
  train.temp.df$vxx.signal <- ifelse(train.list[[df.num]]$vxv.vxmt.ratio > 1 & train.list[[df.num]]$vxv.vxmt.ratio > train.list[[df.num]][,paste0("ratio.sma.n", sma.loop.length[[i]])], 1,0)
  train.temp.df$xiv.signal <- ifelse(train.list[[df.num]]$vxv.vxmt.ratio < 1 & train.list[[df.num]]$vxv.vxmt.ratio < train.list[[df.num]][,paste0("ratio.sma.n", sma.loop.length[[i]])], 1,0)
  
  # lag signal by two forward days
  # CBOE data is available next day
  train.temp.df$vxx.signal <- lag(train.temp.df$vxx.signal,2) # Note k=1 implies a move *forward*
  train.temp.df$xiv.signal <- lag(train.temp.df$xiv.signal,2) # Note k=1 implies a move *forward*
  
  train.temp.df[is.na(train.temp.df)] <- as.Date(0)  # Set NA to 0
  
  # Calculate equity curves
  train.temp.df$vxx.signal.ret <-   train.temp.df$vxx.signal *  train.list[[df.num]]$vxx.close.ret
  train.temp.df$xiv.signal.ret <-   train.temp.df$xiv.signal *  train.list[[df.num]]$xiv.close.ret
  
  # Combine signals 
   train.temp.df$total.signal.ret <-  train.temp.df$vxx.signal.ret +  train.temp.df$xiv.signal.ret
  
  # Pull select columns from data frame to make XTS whilst retaining formats
  xts1 = xts(train.temp.df$vxx.signal.ret, order.by=as.Date(train.temp.df$Date, format="%y-%m-%d"))
  xts2 = xts(train.temp.df$xiv.signal.ret, order.by=as.Date(train.temp.df$Date, format="%y/%m/%d")) 
  xts3 = xts(train.temp.df$total.signal.ret, order.by=as.Date(train.temp.df$Date, format="%y/%m/%d")) 
  
  # Join XTS together
  compare <- cbind(xts1,xts2,xts3)
  
  # Use the PerformanceAnalytics package for trade statistics
  require(PerformanceAnalytics)
  colnames(compare) <- c("vxx","xiv","combined")
  #charts.PerformanceSummary(compare,main="Long when current month is higher than previous 12 month", wealth.index=TRUE, colorset=rainbow12equal)
  #performance.table <- rbind(table.AnnualizedReturns(compare),maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
  #drawdown.table <- rbind(table.Drawdowns(xts3))
  #dev.off()  
  # logRets <- log(cumprod(1+compare))
  # chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="Log Returns")
  
  #print(performance.table)
  #print(drawdown.table)
  cum.ret <- Return.cumulative(xts3, geometric = TRUE)
  annualized <- Return.annualized(xts3, scale = NA, geometric = TRUE)
  dd <- maxDrawdown(xts3)
  sharpe <- SharpeRatio.annualized(xts3, Rf = 0, scale = NA, geometric = TRUE)
  
  
  # Output data
  temp <- data.frame("Annualized Return" = annualized,"Annualized Sharpe" = sharpe,"Cumulative Return" = cum.ret,"Maximum Draw Down" =dd, ID=as.numeric(sma.loop.length[[i]]))
  rownames(temp) <- paste0("train.set.",train.loop.length[[i]],"_sma_",sma.loop.length[[i]])
  train.set.results <- rbind.data.frame(train.set.results,temp)
 compare <- data.frame(compare)
 compare <-  data.frame(date=index(compare), coredata(compare))
 train.xts[[i]] <- cbind.data.frame(compare)
  
  ptm0 <- proc.time()
  Sys.sleep(0.1)  
  ptm1=proc.time() - ptm0
  time=as.numeric(ptm1[3])
  cat('\n','Iteration Train Set',train.loop.length[i],'sma',sma.loop.length[i],'took', time, "seconds to complete")
}, error = function(e) { print(paste("i =", i, "failed:")) })
}


# Seperate train set results from data frame output
train.set.results <- setDT(train.set.results, keep.rownames = TRUE)[] # Set row names
train.set.results$ID <- as.numeric(train.set.results$ID)
# Subset test set results
l <- nrow(train.set.results)
train.result.list <- list()
output_num <- seq(1, l, by=length(sma.lookback))
train.result.list<- lapply(output_num, function(i) train.set.results[c(i:(i+length(sma.lookback)-1)),])

# Extract highest performance metric from each data frame within list
# Sharpe Ratio in this case
highest.sharpe <- list()
i=1
for (i in 1:length(train.result.list)) {  # for every data frame inside results list
  temp <- train.result.list[[i]][order(train.result.list[[i]]$Annualized.Sharpe),]
  highest.sharpe[[i]] <- as.data.frame(tail(temp[,6],1))
}

# Save performance metric
sharpe.df <- do.call(cbind,highest.sharpe)
sharpe.vec <- paste(sharpe.df)
sharpe.vec <- as.numeric(sharpe.vec)
cat("Optimal In-Sample Train Set Parameters:SMA of VXV/VXMT Ratio - Optimal SMAs =",sharpe.vec)

###### Sharpe Optimization Complete ##########

#################################################################
# Run Optimal Parameters over all train sets 
#################################################################

test.set.results <- data.frame()
test.xts <- list()
test.loop.length <- rep(1:length(test.list),1) # run through the all df's 1x 
optimal.sma <- sharpe.vec # run each sma over every df
i=1
################### Manual Intervention ############################ 
# Remove nas on last test data frame inside list (Note this willl change adjust as needed)
#######################################################################
length.test.set <- length(test.list)
tail(test.list[[length.test.set]])
nrow.test.list <- NROW(test.list[[length.test.set]])
test.list[[length.test.set]] <- test.list[[length.test.set]][1:((nrow.test.list)-5),]
# Loop for running all smas through all train sets, note need to wrap train.loop.length and sma.loop.lenght in [[i]]
for (i in 1:length(test.loop.length )) {
  tryCatch({
    df.num <- test.loop.length[i]
    # Enter buy / sell rules
    test.temp.df <- data.frame(Date=test.list[[df.num]]$Date)
    test.temp.df$vxx.signal <- ifelse(test.list[[df.num]]$vxv.vxmt.ratio > 1 & test.list[[df.num]]$vxv.vxmt.ratio > test.list[[df.num]][,paste0("ratio.sma.n", optimal.sma[[i]])], 1,0)
    test.temp.df$xiv.signal <- ifelse(test.list[[df.num]]$vxv.vxmt.ratio < 1 & test.list[[df.num]]$vxv.vxmt.ratio < test.list[[df.num]][,paste0("ratio.sma.n", optimal.sma[[i]])], 1,0)
    
    # lag signal by two forward days
    # CBOE data is available next day
    test.temp.df$vxx.signal <- lag(test.temp.df$vxx.signal,2) # Note k=1 implies a move *forward*
    test.temp.df$xiv.signal <- lag(test.temp.df$xiv.signal,2) # Note k=1 implies a move *forward*
    
    test.temp.df[is.na(test.temp.df)] <- as.Date(0)  # Set NA to 0
    
    # Calculate equity curves
    test.temp.df$vxx.signal.ret <-   test.temp.df$vxx.signal *  test.list[[df.num]]$vxx.close.ret
    test.temp.df$xiv.signal.ret <-   test.temp.df$xiv.signal *  test.list[[df.num]]$xiv.close.ret
    
    # Combine signals 
    test.temp.df$total.signal.ret <-  test.temp.df$vxx.signal.ret +  test.temp.df$xiv.signal.ret
    
    # Pull select columns from data frame to make XTS whilst retaining formats
    xts1 = xts(test.temp.df$vxx.signal.ret, order.by=as.Date(test.temp.df$Date, format="%y-%m-%d"))
    xts2 = xts(test.temp.df$xiv.signal.ret, order.by=as.Date(test.temp.df$Date, format="%y/%m/%d")) 
    xts3 = xts(test.temp.df$total.signal.ret, order.by=as.Date(test.temp.df$Date, format="%y/%m/%d")) 
    
    # Join XTS together
    compare <- cbind(xts1,xts2,xts3)
    
    # Use the PerformanceAnalytics package for trade statistics
    require(PerformanceAnalytics)
    colnames(compare) <- c("vxx","xiv","combined")
    charts.PerformanceSummary(compare,main="Long when current month is higher than previous 12 month", wealth.index=TRUE, colorset=rainbow12equal)
    #performance.table <- rbind(table.AnnualizedReturns(compare),maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
    #drawdown.table <- rbind(table.Drawdowns(xts3))
    #dev.off()  
    # logRets <- log(cumprod(1+compare))
    # chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="Log Returns")
    
    #print(performance.table)
    #print(drawdown.table)
    cum.ret <- Return.cumulative(xts3, geometric = TRUE)
    annualized <- Return.annualized(xts3, scale = NA, geometric = TRUE)
    dd <- maxDrawdown(xts3)
    sharpe <- SharpeRatio.annualized(xts3, Rf = 0, scale = NA, geometric = TRUE)
    
    
    # Save test set results
    temp <- data.frame("Annualized Return" = annualized,"Annualized Sharpe" = sharpe,"Cumulative Return" = cum.ret,"Maximum Draw Down" =dd, ID=as.numeric(optimal.sma[[i]]))
    test.set.results <- rbind.data.frame(test.set.results)
    test.xts[[i]] <- cbind.data.frame(compare)
    
    ptm0 <- proc.time()
    Sys.sleep(0.1)  
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration Test Set',test.loop.length[i],'optimal.sma',optimal.sma[i],'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

# Extract xts3 cumulative returns 
cum.rets <- do.call(rbind,test.xts)
cum.rets <- setDT(cum.rets, keep.rownames = TRUE)[] # Set row names
colnames(cum.rets)[1] <- "Date"
cum.rets[is.na(cum.rets)] <- 0
cum.rets$Date <- ymd(cum.rets$Date)
cum.rets.xts <- xts(cum.rets$combined, order.by=as.Date(cum.rets$Date, format="%y-%m-%d")) 

# Summary Statistic on final walk forward curve 
charts.PerformanceSummary(cum.rets,main="Long when current month is higher than previous 12 month", wealth.index=TRUE, colorset=rainbow12equal)
table.AnnualizedReturns(cum.rets.xts)
maxDrawdown(cum.rets.xts)
CalmarRatio(cum.rets.xts)
table.DownsideRisk(cum.rets.xts)
drawdown.table <- rbind(table.Drawdowns(cum.rets.xts))
drawdown.table

# Extract train and test dates for plotting 
# Train Dates
train.dateslist <- list()
train.dates.loop.length <- rep(1:length(train.list),each=1)
temp <- data.frame()
i=1
for (i in 1:length(train.dates.loop.length)) {
  temp <- data.frame("ID"=paste("train.id",train.dates.loop.length[[i]]))
  date <- train.list[[i]]$Date
  train.dateslist[[i]] <- cbind("Date" = date,ID=temp)

# Test Dates
test.dateslist <- list()
test.dates.loop.length <- rep(1:length(test.list),each=1)
i=1
for (i in 1:length(test.dates.loop.length)) {
  temp <- data.frame("ID"=paste("test.id",test.dates.loop.length[[i]]))
  date <- test.list[[i]]$Date
  test.dateslist[[i]] <- cbind("Date" = date,ID=temp)
  }

# Loop to extract start and end dates of each train / test segment 
train.dates.start <- list()
train.dates.end <- list()
for (i in 1:length(train.dateslist)) {
train.dates.start[[i]] <- train.dateslist[[i]]$Date[1]
train.dates.end[[i]] <- tail(train.dateslist[[i]]$Date,1)
}
test.dates.start <- list()
test.dates.end <- list()
for (i in 1:length(test.dateslist)) {
  test.dates.start[[i]] <- test.dateslist[[i]]$Date[1]
  test.dates.end[[i]] <- tail(test.dateslist[[i]]$Date,1)
}

# Plot train and test starting / ending periods
  ggplot()+
    theme_bw()+
    scale_y_continuous(expand = c(0, 0),breaks = round(seq(min(1), max(length(test.dateslist)), by = 1)))+ # set y axis to same amount as total train / test sample periosd
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  ggtitle("Train And Test Windows",subtitle=paste("Train Width =",train.length,"Days, Test Width =",test.length,"Days"))+
    geom_rect(aes(xmin=as.Date(train.dates.start[[1]]),xmax=as.Date(train.dates.end[[1]]),ymin=1, ymax=2),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[2]]),xmax=as.Date(train.dates.end[[2]]),ymin=2, ymax=3),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[3]]),xmax=as.Date(train.dates.end[[3]]),ymin=3, ymax=4),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[4]]),xmax=as.Date(train.dates.end[[4]]),ymin=4, ymax=5),fill="#0072B2",alpha=.8)+    
    geom_rect(aes(xmin=as.Date(train.dates.start[[5]]),xmax=as.Date(train.dates.end[[5]]),ymin=5, ymax=6),fill="#0072B2",alpha=.8)+    
    geom_rect(aes(xmin=as.Date(train.dates.start[[6]]),xmax=as.Date(train.dates.end[[6]]),ymin=6, ymax=7),fill="#0072B2",alpha=.8)+    
    geom_rect(aes(xmin=as.Date(train.dates.start[[7]]),xmax=as.Date(train.dates.end[[7]]),ymin=7, ymax=8),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[8]]),xmax=as.Date(train.dates.end[[8]]),ymin=8, ymax=9),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[9]]),xmax=as.Date(train.dates.end[[9]]),ymin=9, ymax=10),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[10]]),xmax=as.Date(train.dates.end[[10]]),ymin=10, ymax=11),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[11]]),xmax=as.Date(train.dates.end[[11]]),ymin=11, ymax=12),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[12]]),xmax=as.Date(train.dates.end[[12]]),ymin=12, ymax=13),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[13]]),xmax=as.Date(train.dates.end[[13]]),ymin=13, ymax=14),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[14]]),xmax=as.Date(train.dates.end[[14]]),ymin=14, ymax=15),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[15]]),xmax=as.Date(train.dates.end[[15]]),ymin=15, ymax=16),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[16]]),xmax=as.Date(train.dates.end[[16]]),ymin=16, ymax=17),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[17]]),xmax=as.Date(train.dates.end[[17]]),ymin=17, ymax=18),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(train.dates.start[[18]]),xmax=as.Date(train.dates.end[[18]]),ymin=18, ymax=19),fill="#0072B2",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[1]]),xmax=as.Date(test.dates.end[[1]]),ymin=1, ymax=2),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[2]]),xmax=as.Date(test.dates.end[[2]]),ymin=2, ymax=3),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[3]]),xmax=as.Date(test.dates.end[[3]]),ymin=3, ymax=4),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[4]]),xmax=as.Date(test.dates.end[[4]]),ymin=4, ymax=5),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[5]]),xmax=as.Date(test.dates.end[[5]]),ymin=5, ymax=6),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[6]]),xmax=as.Date(test.dates.end[[6]]),ymin=6, ymax=7),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[7]]),xmax=as.Date(test.dates.end[[7]]),ymin=7, ymax=8),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[8]]),xmax=as.Date(test.dates.end[[8]]),ymin=8, ymax=9),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[9]]),xmax=as.Date(test.dates.end[[9]]),ymin=9, ymax=10),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[10]]),xmax=as.Date(test.dates.end[[10]]),ymin=10, ymax=11),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[11]]),xmax=as.Date(test.dates.end[[11]]),ymin=11, ymax=12),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[12]]),xmax=as.Date(test.dates.end[[12]]),ymin=12, ymax=13),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[13]]),xmax=as.Date(test.dates.end[[13]]),ymin=13, ymax=14),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[14]]),xmax=as.Date(test.dates.end[[14]]),ymin=14, ymax=15),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[15]]),xmax=as.Date(test.dates.end[[15]]),ymin=15, ymax=16),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[16]]),xmax=as.Date(test.dates.end[[16]]),ymin=16, ymax=17),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[17]]),xmax=as.Date(test.dates.end[[17]]),ymin=17, ymax=18),fill="#D55E00",alpha=.8)+
    geom_rect(aes(xmin=as.Date(test.dates.start[[18]]),xmax=as.Date(test.dates.end[[18]]),ymin=18, ymax=19),fill="#D55E00",alpha=.8)+
    labs(x="Date",y="Train / Test Set No.")
    
    
# Extract final equity curve from test set 
  final.plot <- list()
  i=1
  for (i in 1:length(test.xts)) {
    to.sum <- data.frame("to.sum"=test.xts[[i]]$combined)
    dates <- test.list[[i]]$Date
      final.plot[[i]] <- data.frame("Date"=dates,to.sum)
  }

  # Make df of output 
  final.df <- do.call(rbind,final.plot)
  # Cumsum equity curve
final.df$equity <- cumprod(final.df$to.sum + 1) -1 
  final.df <- final.df[c(1,3) ]
  # Melt df 
  final.plot.df <- melt(data = final.df,id.vars = 'Date')   # melt df for plotting with ggplot2
  # Create ID
  final.plot.df$final_num <- as.numeric(rep(1:test.length+1, each=test.length+1, length.out=nrow(final.plot.df)))
  # Subset plots to plot each test period a different colour
  subset.plots <- list()
  test.width <- 125
  subset.plots<- lapply(test_num_set, function(i) final.plot.df[c(i:(i+test.length)),])
 # Create plot 
  # Create colour scheme
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Paired")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 18))
  # Plot out of sample test results equity curve                           
  ggplot(data = subset.plots[[1]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[2]], aes(x =Date, y = value))+
    geom_line(data = subset.plots[[3]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[4]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[5]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[6]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[7]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[8]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[9]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[10]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[11]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[12]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[13]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[14]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[15]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[16]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[17]], aes(x =Date, y = value,colour=final_num))+
    geom_line(data = subset.plots[[18]], aes(x =Date, y = value,colour=final_num))+
    sc+ 
    theme_bw()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = seq(1, 110, by = 5))+
    ggtitle("Test Set Results", subtitle =paste("Test Set Lengths =",test.length," rows")) +
    labs(x="Date",y="Cumulative Return")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))

# Meboot time series resampling 
# Andrew Bannerman 12.29.2017

require(xts)
require(data.table)
require(ggplot2)
require(lubridate)
require(magrittr)
require(scales)
require(reshape2)
require(PerformanceAnalytics)
require(dplyr)
require(TTR)
require(meboot)
require(np)

######### Download ETF Data ###############
# Load Syntehtic and join to alpha vantage adjusted prices 
# Load synthetic VXX and XIV data 
library(readxl)
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

# Download SPY data
# Note you need tyo place your API key...your_key_here
SPY <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=SPY&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
SPY$timestamp <- ymd(SPY$timestamp)   #Lubridate to change character date to date format
SPY <- arrange(SPY,timestamp)   #dplyr to sort data frame by date ascending order
colnames(SPY)[1] <- "Date"
SPY$Date <- ymd(SPY$Date)
SPY <- as.data.frame(SPY)
SPY <- subset(SPY, Date >= as.POSIXct("2004-03-26") ) # synthetic data start
head(SPY)

VXX <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=VXX&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
VXX$timestamp <- ymd(VXX$timestamp)   #Lubridate to change character date to date format
VXX <- arrange(VXX,timestamp)   #dplyr to sort data frame by date ascending order
colnames(VXX)[1] <- "Date"
VXX$Date <- ymd(VXX$Date)
VXX <- as.data.frame(VXX)
head(VXX)

XIV <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=XIV&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
XIV$timestamp <- ymd(XIV$timestamp)   #Lubridate to change character date to date format
XIV <- arrange(XIV,timestamp)   #dplyr to sort data frame by date ascending order
colnames(XIV)[1] <- "Date"
XIV$Date <- ymd(XIV$Date)
XIV <- as.data.frame(XIV)
head(XIV)
#XIV <- subset(XIV, Date >= as.POSIXct("2012-01-01"))

ZIV <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=ZIV&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
ZIV$timestamp <- ymd(ZIV$timestamp)   #Lubridate to change character date to date format
ZIV <- arrange(ZIV,timestamp)   #dplyr to sort data frame by date ascending order
colnames(ZIV)[1] <- "Date"
ZIV$Date <- ymd(ZIV$Date)
ZIV <- as.data.frame(ZIV)
head(ZIV)

VXZ <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=VXZ&outputsize=full&apikey=your_api_key&datatype=csv") #fread() data.table for downloading directly to a data frame
VXZ$timestamp <- ymd(VXZ$timestamp)   #Lubridate to change character date to date format
VXZ <- arrange(VXZ,timestamp)   #dplyr to sort data frame by date ascending order
colnames(VXZ)[1] <- "Date"
VXZ$Date <- ymd(VXZ$Date)
VXZ <- as.data.frame(VXZ)
tail(VXZ)

# Join sythentic data to alpha vantage 
vxx.synth <- subset(vxx.synth, Date <= as.POSIXct("2009-01-29"))
xiv.synth <- subset(xiv.synth, Date <= as.POSIXct("2010-11-29"))
ziv.synth <- subset(ziv.synth, Date <= as.POSIXct("2010-11-29"))
vxz.synth <- subset(vxz.synth, Date <= as.POSIXct("2009-02-19"))
# Subset only date and close from alpha vantage data 
VXX <- VXX[ -c(2:5, 7:9) ]  # subset adjusted close
XIV <- XIV[ -c(2:5, 7:9) ]  # subset adjusted close
ZIV <- ZIV[ -c(2:5, 7:9) ]  # subset adjusted close
VXZ <- VXZ[ -c(2:5, 7:9) ]  # subset adjusted close
SPY <- SPY[ -c(3:5, 7:9) ]  # subset adjusted close
colnames(VXX)[2] <- "vxx_close"
colnames(XIV)[2] <- "xiv_close"
colnames(ZIV)[2] <- "ziv_close"
colnames(VXZ)[2] <- "vxz_close"
colnames(SPY)[2] <- "spy_open"
colnames(SPY)[3] <- "spy_close"

# row bind 
VXX <- rbind(vxx.synth,VXX)
XIV <- rbind(xiv.synth,XIV)
ZIV <- rbind(ziv.synth,ZIV)
VXZ <- rbind(vxz.synth,VXZ)
df <- cbind(VXX,XIV,ZIV,VXZ,SPY)
tail(df)

# Download Spot VIX Price and VXV Price from CBOE website
VIX_cboe <- fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv")
VIX_cboe <- as.data.frame(VIX_cboe)
VIX_cboe <- VIX_cboe[2:nrow(VIX_cboe), ]
colnames(VIX_cboe)[1] = "Date"
colnames(VIX_cboe)[2] = "vix_open"
colnames(VIX_cboe)[3] = "vix_high"
colnames(VIX_cboe)[4] = "vix_low"
colnames(VIX_cboe)[5] = "vix_close"
VIX_cboe$Date <- mdy(VIX_cboe$Date)
cols <-c(2:5)
VIX_cboe[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
VIX_cboe <- subset(VIX_cboe, Date >= as.POSIXct("2007-12-04") )
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
tail(cboe.df)
df <- df[,c(-3,-5,-7,-9)] # Drop unused dates
df <- full_join(df, cboe.df, by = c("Date" = "Date"))

# Remove last rows
nrow <- NROW(df)-2
df <- head(df,nrow)
tail(df)

############################################
# Back Test VXV / VXMT Ratio 
# Find best params without bootstrapping
############################################
# Calculate Close-to-Close returns
df$vxx.close.ret <- ROC(df$vxx_close, type = c("discrete"))
df$xiv.close.ret <- ROC(df$xiv_close, type = c("discrete"))

# VXV / VXMT Ratio
df$vxv.vxmt.ratio <- df$vxv_cboe_close / df$vxmt_cboe_close
df[is.na(df)] <- 0
head(df)

# Calculate SMA of ratio 
numdays <- 2:500
getSMA <- function(numdays) {
  function(df) {
    SMA(df[,"vxv.vxmt.ratio"], numdays)    # Calls TTR package to create SMA
  }
}
# Create a matrix to put the SMAs in
sma.matrix <- matrix(nrow=nrow(df), ncol=0)

# Loop for filling it
for (i in numdays) {
  sma.matrix <- cbind(sma.matrix, getSMA(i)(df))
}

# Rename columns
colnames(sma.matrix) <- sapply(numdays, function(n)paste("ratio.sma.n", n, sep=""))

# Bind to existing dataframe
df <-  cbind(df, sma.matrix)
tail(df)

##############################################
# Optimize Strategy Params 
##############################################
num.days <- 2:300
i=3
# Initialize data frame
data_output_df <- data.frame() 
# Optimize #########
optIMIZE = function(x){
  #spx.sma <- df[,paste0("close.sma.n", sma[i])]
  names(df)
  # Enter buy / sell rules
  #df$vxx.signal <- ifelse(df$vxv.vxmt.ratio > 1 & df$vxv.vxmt.ratio > df$ratio.sma , 1,0)
  #df$xiv.signal <- ifelse(df$vxv.vxmt.ratio < 1 & df$vxv.vxmt.ratio < df$ratio.sma , 1,0)
  df$vxx.signal <- ifelse(df$vxv.vxmt.ratio > 1 & df$vxv.vxmt.ratio > df[,paste0("ratio.sma.n", num.days[i])], 1,0)
  df$xiv.signal <- ifelse(df$vxv.vxmt.ratio < 1 & df$vxv.vxmt.ratio < df[,paste0("ratio.sma.n", num.days[i])], 1,0)
  
  # lag signal by two forward days
  # CBOE data is available next day
  df$vxx.signal <- lag(df$vxx.signal,2) # Note k=1 implies a move *forward*
  df$xiv.signal <- lag(df$xiv.signal,2) # Note k=1 implies a move *forward*
  
  df[is.na(df)] <- 0  # Set NA to 0
  
  # Calculate equity curves
  df$vxx.signal.ret <-  df$vxx.signal * df$vxx.close.ret
  df$xiv.signal.ret <-  df$xiv.signal * df$xiv.close.ret
  
  # Combine signals 
  df$total.signal.ret <- df$vxx.signal.ret + df$xiv.signal.ret
  
  # Pull select columns from data frame to make XTS whilst retaining formats
  xts1 = xts(df$vxx.signal.ret, order.by=as.Date(df$Date, format="%m/%d/%Y"))
  xts2 = xts(df$xiv.signal.ret, order.by=as.Date(df$Date, format="%m/%d/%Y")) 
  xts3 = xts(df$total.signal.ret, order.by=as.Date(df$Date, format="%m/%d/%Y")) 
  tail(xts3)
  
  # Join XTS together
  compare <- cbind(xts1,xts2,xts3)
  
  # Use the PerformanceAnalytics package for trade statistics
  
  require(PerformanceAnalytics)
  colnames(compare) <- c("vxx","xiv","combined")
  #charts.PerformanceSummary(compare,main="Long when current month is higher than previous 12 month", wealth.index=TRUE, colorset=rainbow12equal)
  # performance.table <- rbind(table.AnnualizedReturns(compare),maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
  # drawdown.table <- rbind(table.Drawdowns(xts3))
  #dev.off()
  # logRets <- log(cumprod(1+compare))
  # chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="Log Returns")
  
  #print(performance.table)
  #print(drawdown.table)
  cum.ret <- Return.cumulative(xts3, geometric = TRUE)
  annualized <- Return.annualized(xts3, scale = NA, geometric = TRUE)
  dd <- maxDrawdown(xts3)
  sharpe <- SharpeRatio.annualized(xts3, Rf = 0, scale = NA, geometric = TRUE)
  
  
  # Create data output of rep and close.diff columns rbind
  data_output_df <- data.frame("Annualized Return" = annualized,"Annualized Sharpe" = sharpe,"Cumulative Return" = cum.ret,"Maximum Draw Down" = dd)
  
}

for (i in 1:length(num.days)){    # Length of optimization
  tryCatch({
    temp <- optIMIZE(num.days[[i]])
    rownames(temp) <- paste0("",num.days[i])
    #cum_ret <- rbind.data.frame(cum_ret, temp)
    data_output_df <- rbind.data.frame(data_output_df,temp)
    ptm0 <- proc.time()
    Sys.sleep(0.1)  
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

# Join SMA number to data frame
data_output_df <- data.frame(data_output_df,num.days)
show <- data_output_df[51:70,]

# Plot 
colnames(data_output_df)
library(ggplot2)
ggplot(data=data_output_df, aes(x=num.days,Annualized.Return))+
  geom_bar(stat="identity")+
  theme_classic()+
  scale_x_continuous(breaks = seq(min(data_output_df$num.days), max(data_output_df$num.days)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5,size=15))+
  ggtitle("VXV/VXMT Volatility Strategy - Optimized sma vzx/vxmt ratio",subtitle="2008 to present")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  geom_rect(aes(xmin=51,xmax=70,ymin=0,ymax=Inf),alpha=0.01,fill="#CC6666")+
  geom_rect(aes(xmin=117,xmax=161,ymin=0,ymax=Inf),alpha=0.01,fill="#CC6666")+
  annotate("text", label = "sma 51 to 70", x = 60, y = .65, color = "red")+
  annotate("text", label = "sma 117 to 161", x = 135, y = .65, color = "red")
  

  
#scale_y_continuous(breaks = seq(min(data_output_df$Annualized.Return), max(data_output_df$Annualized.Return),by=0.0010))


acf(df$xiv_close)
####### End Back Test Original Sample ##########
############################
# Meboot 
# Generate Maximum Entropy Bootstrapped Time Series Ensemble
############################
# XIV
df[is.na(df)] <- 0
df <- head(df,NROW(df)-1) # Remove missng value...
tail(df$xiv_close,10)
xiv.boot <- meboot(df$xiv_close, reps=100, trim=list(trim=0.10, xmin=NULL, xmax=NULL), reachbnd=FALSE,
               expand.sd=TRUE, force.clt=FALSE, scl.adjustment = FALSE, sym = FALSE,
               elaps=TRUE, colsubj, coldata, coltimes)
# Place meboot results in data frame
xiv.ensemble.df <- data.frame(xiv.boot$ensemble)
xiv.ensemble.df <- data.frame(xiv.ensemble.df,"Date"=df$Date)
# Melt for plotting
xiv.plot.df <- melt(xiv.ensemble.df,id.vars = "Date")
# Plot ggplot2
ggplot(data = xiv.plot.df, aes(x=Date,y=value))+
  geom_line(aes(group = variable))+
  theme_classic()+
  theme(legend.position = "none")+
  geom_line(data=df,aes(x=Date,y=xiv_close,colour="red"))+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  ggtitle("Resampled Time Series - XIV",subtitle="100 Iterations")
  

# VXX
vxx.boot <- meboot(df$vxx_close, reps=100, trim=list(trim=0.10, xmin=NULL, xmax=NULL), reachbnd=FALSE,
                   expand.sd=TRUE, force.clt=FALSE, scl.adjustment = FALSE, sym = FALSE,
                   elaps=TRUE, colsubj, coldata, coltimes)
# Place meboot results in data frame
vxx.ensemble.df <- data.frame(vxx.boot$ensemble)
vxx.ensemble.df <- data.frame(vxx.ensemble.df,"Date"=df$Date)
# Melt for plotting
vxx.plot.df <- melt(vxx.ensemble.df,id.vars = "Date")
# Plot ggplot2
ggplot(data = vxx.plot.df, aes(x=Date,y=value))+
  geom_line(aes(group = variable))+
  theme_classic()+
  theme(legend.position = "none")+
  geom_line(data=df,aes(x=Date,y=vxx_close,colour="red"))+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  ggtitle("Resampled Time Series - VXX", subtitle="100 Iterations")

# VXV
vxv.boot <- meboot(df$vxv_cboe_close, reps=100, trim=list(trim=0.10, xmin=NULL, xmax=NULL), reachbnd=FALSE,
                   expand.sd=TRUE, force.clt=FALSE, scl.adjustment = FALSE, sym = FALSE,
                   elaps=TRUE, colsubj, coldata, coltimes)
# Place meboot results in data frame
vxv.ensemble.df <- data.frame(vxv.boot$ensemble)
vxv.ensemble.df <- data.frame(vxv.ensemble.df,"Date"=df$Date)
# Melt for plotting
vxv.plot.df <- melt(vxv.ensemble.df,id.vars = "Date")
# Plot ggplot2
ggplot(data = vxv.plot.df, aes(x=Date,y=value))+
  geom_line(aes(group = variable))+
  theme_classic()+
  theme(legend.position = "none")+
  geom_line(data=df,aes(x=Date,y=vxv_cboe_close,colour="red"))+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  ggtitle("Resampled Time Series - VXV", subtitle="100 Iterations")

# VXMT
vxmt.boot <- meboot(df$vxmt_cboe_close, reps=100, trim=list(trim=0.10, xmin=NULL, xmax=NULL), reachbnd=FALSE,
                   expand.sd=TRUE, force.clt=FALSE, scl.adjustment = FALSE, sym = FALSE,
                   elaps=TRUE, colsubj, coldata, coltimes)
# Place meboot results in data frame
vxmt.ensemble.df <- data.frame(vxmt.boot$ensemble)
vxmt.ensemble.df <- data.frame(vxmt.ensemble.df,"Date"=df$Date)
# Melt for plotting
vxmt.plot.df <- melt(vxmt.ensemble.df,id.vars = "Date")
# Plot ggplot2
ggplot(data = vxmt.plot.df, aes(x=Date,y=value))+
  geom_line(aes(group = variable))+
  theme_classic()+
  theme(legend.position = "none")+
  geom_line(data=df,aes(x=Date,y=vxmt_cboe_close,colour="red"))+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  ggtitle("Resampled Time Series - VXMT")

# Back test optimal band found in initial sample over boot strapped series 
df.boot <- data.frame("Date" = xiv.ensemble.df$Date)
# boot strap sample series results 
# Initialize list
boot_output <- list()
#Drop dates (not used for plotting)
xiv.ensemble.df <- xiv.ensemble.df[,-101]
vxx.ensemble.df <- vxx.ensemble.df[,-101]
vxv.ensemble.df <- vxv.ensemble.df[,-101]
vxmt.ensemble.df <- vxmt.ensemble.df[,-101]

i=1
sma <- rep(51:70,each=length(xiv.ensemble.df))
length.dfs <- rep(1:100,11)
for (i in 1:length(length.dfs)) {
    tryCatch({
  ############################################
  # Back Test VXV / VXMT Ratio 
  # Find best params without bootstrapping
  ############################################
  # Calculate Close-to-Close returns
  df.boot$vxx.close.ret <- ROC(vxx.ensemble.df[,length.dfs[i]], type = c("discrete"))
  df.boot$xiv.close.ret <- ROC(xiv.ensemble.df[,length.dfs[i]], type = c("discrete"))
  
  # VXV / VXMT Ratio
  df.boot$vxv.vxmt.ratio <- vxv.ensemble.df[,length.dfs[i]]/ vxmt.ensemble.df[,length.dfs[i]]
  df.boot$vxv.vxmt.ratio[is.na(df.boot$vxv.vxmt.ratio)] <- 0
  df.boot$vxv.vxmt.ratio[is.nan(df.boot$vxv.vxmt.ratio)] <- 0
  df.boot$vxv.vxmt.ratio[is.infinite(df.boot$vxv.vxmt.ratio)] <- 0
  
  # Create sma
  df.boot$sma <- SMA(df.boot[,"vxv.vxmt.ratio"], sma[i])    # Calls TTR package to create SMA

  ##############################################
  # Optimize Strategy Params 
  ##############################################
    # Enter buy / sell rules
  sma.n <- sma[i]
    #df.boot$vxx.signal <- ifelse(df.boot$vxv.vxmt.ratio > 1 & df.boot$vxv.vxmt.ratio > df.boot[,paste0("ratio.sma.n", sma.n)], 1,0)
  df.boot$vxx.signal <- ifelse(df.boot$vxv.vxmt.ratio > 1 & df.boot$vxv.vxmt.ratio > df.boot$sma, 1,0)
  df.boot$xiv.signal <- ifelse(df.boot$vxv.vxmt.ratio < 1 & df.boot$vxv.vxmt.ratio < df.boot$sma, 1,0)
    
    # lag signal by two forward days
    # CBOE data is available next day
    df.boot$vxx.signal <- lag(df.boot$vxx.signal,2) # Note k=1 implies a move *forward*
    df.boot$xiv.signal <- lag(df.boot$xiv.signal,2) # Note k=1 implies a move *forward*
    
    # Calculate equity curves
    df.boot$vxx.signal.ret <-  df.boot$vxx.signal * df.boot$vxx.close.ret
    df.boot$xiv.signal.ret <-  df.boot$xiv.signal * df.boot$xiv.close.ret
    
    # Combine signals 
    df.boot$total.signal.ret <- df.boot$vxx.signal.ret + df.boot$xiv.signal.ret
    
    # Pull select columns from data frame to make XTS whilst retaining formats
    xts1 = xts(df.boot$vxx.signal.ret, order.by=as.Date(df.boot$Date, format="%m/%d/%Y"))
    xts2 = xts(df.boot$xiv.signal.ret, order.by=as.Date(df.boot$Date, format="%m/%d/%Y")) 
    xts3 = xts(df.boot$total.signal.ret, order.by=as.Date(df.boot$Date, format="%m/%d/%Y")) 
    tail(xts3)
    
    # Join XTS together
    compare <- cbind(xts1,xts2,xts3)
    
    # Use the PerformanceAnalytics package for trade statistics
    
    require(PerformanceAnalytics)
    colnames(compare) <- c("vxx","xiv","combined")
    #charts.PerformanceSummary(compare,main="Long when current month is higher than previous 12 month", wealth.index=TRUE, colorset=rainbow12equal)
    # performance.table <- rbind(table.AnnualizedReturns(compare),maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
    # drawdown.table <- rbind(table.Drawdowns(xts3))
    #dev.off()
    # logRets <- log(cumprod(1+compare))
    # chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="Log Returns")
    
    #print(performance.table)
    #print(drawdown.table)
    cum.ret <- Return.cumulative(xts3, geometric = TRUE)
    annualized <- Return.annualized(xts3, scale = NA, geometric = TRUE)
    dd <- maxDrawdown(xts3)
    sharpe <- SharpeRatio.annualized(xts3, Rf = 0, scale = NA, geometric = TRUE)
    id <- paste0("col",length.dfs[i],"sma",sma[i])
    
    
    # Create data output of rep and close.diff columns rbind
   out <- data.frame("Annualized Return" = annualized,"Annualized Sharpe" = sharpe,"Cumulative Return" = cum.ret,"Maximum Draw Down" = dd, id = id)
   rownames(out) <- paste0("col",length.dfs[i],"sma",sma[i])
    boot_output[[i]] <- rbind(out)

     ptm0 <- proc.time()
    Sys.sleep(0.1)  
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}
  
  # Join  boot output
  master <- do.call(rbind, boot_output)
  names(master)
  
  # Find confidence intervals 
  annualized.ret.mean <- mean(master$Annualized.Return)
  annualized.ret.sdev <- sd(master$Annualized.Return)
  annualized.ret.sample.size <- nrow(master)
  annualized.error <- qnorm(0.975)* annualized.ret.sdev/sqrt( annualized.ret.sample.size)
  annualized.left <- annualized.ret.mean - annualized.error
  annualized.right <- annualized.ret.mean + annualized.error
  
  #annualized.SE <- annualized.ret.sdev / sqrt(annualized.ret.sample.size)
  #t.test(master$Annualized.Sharpe,conf.level = 0.98)
  
  annualized.sharpe.mean <- mean(master$Annualized.Sharpe)
  annualized.sharpe.sdev <- sd(master$Annualized.Sharpe)
  annualized.sharpe.sample.size <- nrow(master)
  annualized.sharpe.error <- qnorm(0.975)* annualized.sharpe.sdev/sqrt(annualized.sharpe.sample.size)
  annualized.sharpe.left <- annualized.sharpe.mean - annualized.error
  annualized.sharpe.right <- annualized.sharpe.mean + annualized.sharpe.error
  
  Maximum.Draw.Down.mean <- mean(master$Maximum.Draw.Down)
  Maximum.Draw.Down.sdev <- sd(master$Maximum.Draw.Down)
  Maximum.Draw.Down.sample.size <- nrow(master)
  Maximum.Draw.Down.error <- qnorm(0.975)* Maximum.Draw.Down.sdev/sqrt( Maximum.Draw.Down.sample.size)
  Maximum.Draw.Down.left <- Maximum.Draw.Down.mean - annualized.error
  Maximum.Draw.Down.right <- Maximum.Draw.Down.mean + Maximum.Draw.Down.error
  
  #t.test(master$Annualized.Return)
 # jarque.bera.test(master$Maximum.Draw.Down)
  require(tseries)
  

  # Plot 
  # Annualized Return
  library(ggplot2)
  p1 <- ggplot(data=master, aes(Annualized.Return,col=I("red")))+
    geom_histogram(binwidth = 0.02)+
    theme_classic()+
    ggtitle("Resampled Strategy Results - Sma 51 to 70 - Annualized Return",subtitle="Strategy run over 100 resampled time series")+
    labs(x="Annualized Return",y="Count")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
   # geom_vline(xintercept = left, color="blue", linetype="dashed")+
    #geom_vline(xintercept = right, color="blue", linetype="dashed")+
    geom_vline(xintercept = annualized.ret.mean, color="blue", linetype="dashed")+
    annotate("text", label = "Mean = 0.425727", x = 0.35, y = 117, color = "blue")
    
  
  
 # Sharpe Ratio
 p3 <- ggplot(data=master, aes(Annualized.Sharpe,col=I("red")))+
    geom_histogram(binwidth = 0.001)+
    theme_classic()+
    ggtitle("Resampled Strategy Results - Sma 51 to 70 - Annualized Sharpe Ratio",subtitle="Strategy run over 100 resampled time series")+
    labs(x="Annualized.Sharpe",y="Count")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    geom_vline(xintercept = annualized.sharpe.mean, color="blue", linetype="dashed")+
   geom_vline(xintercept = annualized.sharpe.right, color="blue", linetype="dashed")+
   geom_vline(xintercept = annualized.sharpe.left, color="blue", linetype="dashed")+
    annotate("text", label = "Mean = 1.02887", x = 1.02887, y = 15, color = "blue")
 
 annualized.sharpe.right
  
  # Maximum DD
  # Sharpe Ratio
  p5 <- ggplot(data=master, aes(Maximum.Draw.Down,col=I("red")))+
    geom_histogram(binwidth = 0.01)+
    theme_classic()+
    ggtitle("Resampled Strategy Results - Sma 51 to 70 - Maximum Draw Down",subtitle="Strategy run over 100 resampled time series")+
    labs(x="Maximum.Draw.Down",y="Count")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    geom_vline(xintercept = Maximum.Draw.Down.mean, color="blue", linetype="dashed")+
    annotate("text", label = "Mean = 0.5020338", x = 0.6, y = 65, color = "blue")
  
  
  ################### 
  # Plot Original back test results 
  ##################
  # Plot 
  # Annualized Return
  library(ggplot2)
  # find original mean annualized 
  mean.orig.annaulized <- mean(show$Annualized.Return)
  p2 <- ggplot(data=show, aes(Annualized.Return,col=I("red")))+
    geom_histogram(binwidth = 0.01)+
    theme_classic()+
    ggtitle("Original Strategy Results - Sma 51 to 70 - Annualized Return",subtitle="")+
    labs(x="Annualized Return",y="Count")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    # geom_vline(xintercept = left, color="blue", linetype="dashed")+
    #geom_vline(xintercept = right, color="blue", linetype="dashed")+
    geom_vline(xintercept = mean.orig.annaulized, color="blue", linetype="dashed")+
    annotate("text", label = "Mean = 0.6039258", x = 0.585, y = 15, color = "blue")
  require(gridExtra)
  gridExtra::grid.arrange(p1, p2, nrow = 1)
  
  # Sharpe Ratio
  # find original mean annualized 
  mean.orig.sharpe <- mean(show$Annualized.Sharpe)
 p4 <- ggplot(data=show, aes(Annualized.Sharpe,col=I("red")))+
    geom_histogram(binwidth = 0.02)+
    theme_classic()+
    ggtitle("Original Strategy Results - Sma 51 to 70 - Annualized Sharpe Ratio",subtitle="")+
    labs(x="Annualized.Sharpe",y="Count")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    geom_vline(xintercept = mean.orig.sharpe, color="blue", linetype="dashed")+
    annotate("text", label = "Mean = 1.565738", x = 1.52, y = 6.5, color = "blue")
 gridExtra::grid.arrange(p3, p4, nrow = 1)
 
  
  # Maximum DD
  # Sharpe Ratio
 names(show)
 show$Maximum.Draw.Down
 mean.orig.maxdd <- mean(show$Maximum.Draw.Down)
  p6 <- ggplot(data=show, aes(Maximum.Draw.Down,col=I("red")))+
    geom_histogram(binwidth = 0.0001)+
    theme_classic()+
    ggtitle("Original Strategy Results - Sma 51 to 70 - Maximum Draw Down",subtitle="")+
    labs(x="Maximum.Draw.Down",y="Count")+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
    geom_vline(xintercept = mean.orig.maxdd, color="blue", linetype="dashed")+
    annotate("text", label = "Mean = 0.3420499", x = 0.35, y = 7, color = "blue")
  gridExtra::grid.arrange(p5, p6, nrow = 1)
  

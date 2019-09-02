# Scrape Historical FDA events 
# Clean Data - Extract Date from text 
# Clean up Catalyst Column - remove date, white space 
# Sort by ascending date order 
# Note 2x non ticker symbols, these are removed

require(XML)
require(dplyr)
require(PerformanceAnalytics)
require(data.table)
require(xts)
require(RCurl)
require(dplyr)
require(lubridate)
# Import html data to data.frame 
url <- "https://www.biopharmcatalyst.com/calendars/historical-catalyst-calendar"
urldata <- getURL(url)
fda.table <- as.data.frame(readHTMLTable(urldata, stringsAsFactors = FALSE))

# Rename Columns
colnames(fda.table)[1] <- "Ticker"
colnames(fda.table)[2] <- "Drug"
colnames(fda.table)[3] <- "Stage"
colnames(fda.table)[4] <- "Catalyst"

# Extrat Date from character text string 
# Extracting first 10 elements as order of date positioning is consistent throughout the table
fda.table$Catalyst.Date <-  substring(fda.table$Catalyst, 1, last = 10) 

# Convert string Date to Date format 
# Use lubridate package 
fda.table$Catalyst.Date <- mdy(fda.table$Catalyst.Date)

# Sort data by date ascending order 
# Use dplyr 
fda.table <- arrange(fda.table, Catalyst.Date)
nrow(fda.table)

# Clean up text in Catalyst Column 
# Remove white space and drop the date
#fda.table$Catalyst <-  gsub("\\d+/\\d+/\\d+\\n\\s+(.+)$", "\\1", fda.table$Catalyst)

#\\W match a non-word character \\d+ match one or more numbers
#Put those two in brackets which means match either one of those.
#+ match either of the above pronouns more than once
#([a-zA-Z].*) match any letter (upper or lowercase), and then the remainder of the string (.* = 0 or more of any character) and capture it in a group.
#We return the group with \\1
fda.table$Catalyst <-  gsub("^[\\W\\d+]+([a-zA-Z].*)","\\1",fda.table$Catalyst,perl=TRUE)

# Remove 2x non ticker symbols at bottom two rows of data frame
n <- dim(fda.table)[1]
fda.table <- fda.table[1:(n-2),]

# Subset List of Tickers 
# Note used in function below
tickers <- fda.table$Ticker
#tickers <- unique(tickers)    # Remove duplicate tickers

# Subset list of Dates
# Note used in function below
dates <- fda.table$Catalyst.Date

# Filter date frame to remove duplicates
#moo <- fda.table[!duplicated(fda.table$Ticker), ]

# Export symbol column as .txt file 
#write.table(tickers,"D:/R Projects/biotech_catalyst.txt",append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

# Back Test Script
# 10.19.2017
# Andrew Bannerman 

require(lubridate)
require(dplyr)
require(magrittr)
require(TTR)
require(zoo)
require(data.table)
require(xts)
require(PerformanceAnalytics)

# Data Dir
input.data.dir <- "D:/R Projects/Biotech Catalyst Study/data"
png.output.dir <- "D:/R Projects/Biotech Catalyst Study/Biotech_catalyst_rsi2_chart_output"
row.names = tickers
#files = list.files(path = "C:/R Projects/Data/etf_list_nasdaq_dot_com/etf_13min", pattern = ".", full.names = TRUE)

# Initialize the output dataframe to which we will rowbind the results
cum_ret <- data.frame() # Cumualtive / Annualized Return
data_output_df <- data.frame() # Daily Differences, Running 5 day count until FDA date

# Back test function
genCHART = function(x){
  next.png <- tickers[i] 
  date.list <- dates[i] 
  input.file <- paste0("D:/R Projects/Biotech Catalyst Study/data/",next.png,".csv")
  next.file <- read.table(input.file,header=TRUE, sep=",", stringsAsFactors=FALSE)
  new.df <- data.frame(next.file)
  
  # For TS Data
  #colnames(new.df)[1] <- "Date"
  #colnames(new.df)[2] <- "Open"
  #colnames(new.df)[3] <- "High"
  #colnames(new.df)[4] <- "Low"
  #colnames(new.df)[5] <- "Close"
  
  # Convert Values To Numeric 
  cols <-c(3:8)   # note TS is 2:5 and needs column names above
  new.df[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
  #Convert Date Column [1]
  new.df$Date <- ymd(new.df$Date)   #mdy for .txt, mdt)hms for TS intraday
  
  # Merge Date and Time Column to column Date
  # new.df$Date <- paste(new.df$Date, new.df$Time)
  #new.df$Date <- as.POSIXct(new.df$Date, format="%Y-%m-%d %H:%M")
  
  # Use TTR package to create rolling SMA n day moving average 
  # Create function and loop in order to repeat the desired number of SMAs for example 2:30
  getSMA <- function(numdays) {
    function(new.df) {
      SMA(new.df[,"Close"], numdays)    # Calls TTR package to create SMA
    }
  }
  # Create a matrix to put the SMAs in
  sma.matrix <- matrix(nrow=nrow(new.df), ncol=0)
  
  # Loop for filling it
  for (i in 2:3) {
    sma.matrix <- cbind(sma.matrix, getSMA(i)(new.df))
  }
  
  # Rename columns
  colnames(sma.matrix) <- sapply(2:3, function(n)paste("close.sma.n", n, sep=""))
  
  # Bind to existing dataframe
  new.df <-  cbind(new.df, sma.matrix)
  
  # Use TTR package to create rolling Standard Deviation
  # Create function and loop in order to repeat the desired number of Stdev for example 2:30
  getSD <- function(numdays) {
    function(new.df) {
      runSD(new.df$Close, numdays, cumulative = FALSE)    # Calls TTR package to create SMA
    }
  }
  # Create a matrix to put the SD's in
  sd.matrix <- matrix(nrow=nrow(new.df), ncol=0)
  
  # Loop for filling it
  for (i in 2:3) {
    sd.matrix <- cbind(sd.matrix, getSD(i)(new.df))
  }
  
  # Rename columns
  colnames(sd.matrix) <- sapply(2:3, function(n)paste("close.sd.n", n, sep=""))
  
  # Bind to existing dataframe
  new.df <-  cbind(new.df, sd.matrix)
  
  # Use TTR package to create n day SMA RSI
  getRSI.SMA <- function(numdays) {
    function(new.df) {
      RSI(new.df$Close, n=numdays, maType = SMA)    # Calls TTR package to create RSI
    }
  }
  # Create a matrix to put the RSI SMA's in
  rsi.sma.matrix <- matrix(nrow=nrow(new.df), ncol=0)
  
  # Loop for filling it
  for (i in 2:3) {
    rsi.sma.matrix <- cbind(rsi.sma.matrix, getRSI.SMA(i)(new.df))
  }
  
  # Rename columns
  colnames(rsi.sma.matrix) <- sapply(2:3, function(n)paste("rsi.sma.n", n, sep=""))
  
  # Bind to existing dataframe
  new.df <-  cbind(new.df, rsi.sma.matrix)
  
  # Use TTR package to create n day EMA RSI
  getRSI.EMA <- function(numdays) {
    function(new.df) {
      RSI(new.df$Close, n=numdays, maType = EMA)    # Calls TTR package to create RSI
    }
  }
  # Create a matrix to put the RSI EMAs's in
  rsi.ema.matrix <- matrix(nrow=nrow(new.df), ncol=0)
  
  # Loop for filling it
  for (i in 2:3) {
    rsi.ema.matrix <- cbind(rsi.ema.matrix, getRSI.EMA(i)(new.df))
  }
  
  # Rename columns
  colnames(rsi.ema.matrix) <- sapply(2:3, function(n)paste("rsi.ema.n", n, sep=""))
  
  # Bind to existing dataframe
  new.df <-  cbind(new.df, rsi.ema.matrix)
  
  
  # Use base R to work out the rolling z-score (Close - roll mean) / stdev
  #new.df$close.zscore.n2 <- apply(new.df[,c('Close','close.sma.n2', 'close.sd.n2')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n3 <- apply(new.df[,c('Close','close.sma.n3', 'close.sd.n3')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n4 <- apply(new.df[,c('Close','close.sma.n4', 'close.sd.n4')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n5 <- apply(new.df[,c('Close','close.sma.n5', 'close.sd.n5')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n6 <- apply(new.df[,c('Close','close.sma.n6', 'close.sd.n6')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n7 <- apply(new.df[,c('Close','close.sma.n7', 'close.sd.n7')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n8 <- apply(new.df[,c('Close','close.sma.n8', 'close.sd.n8')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n9 <- apply(new.df[,c('Close','close.sma.n9', 'close.sd.n9')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n10 <- apply(new.df[,c('Close','close.sma.n10', 'close.sd.n10')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n11 <- apply(new.df[,c('Close','close.sma.n11', 'close.sd.n11')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n12 <- apply(new.df[,c('Close','close.sma.n12', 'close.sd.n12')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n13 <- apply(new.df[,c('Close','close.sma.n13', 'close.sd.n13')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n14 <- apply(new.df[,c('Close','close.sma.n14', 'close.sd.n14')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n15 <- apply(new.df[,c('Close','close.sma.n15', 'close.sd.n15')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n16 <- apply(new.df[,c('Close','close.sma.n16', 'close.sd.n16')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n17 <- apply(new.df[,c('Close','close.sma.n17', 'close.sd.n17')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n18 <- apply(new.df[,c('Close','close.sma.n18', 'close.sd.n18')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n19 <- apply(new.df[,c('Close','close.sma.n19', 'close.sd.n19')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n20 <- apply(new.df[,c('Close','close.sma.n20', 'close.sd.n20')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n21 <- apply(new.df[,c('Close','close.sma.n21', 'close.sd.n21')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n22 <- apply(new.df[,c('Close','close.sma.n22', 'close.sd.n22')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n23 <- apply(new.df[,c('Close','close.sma.n23', 'close.sd.n23')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n24 <- apply(new.df[,c('Close','close.sma.n24', 'close.sd.n24')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n25 <- apply(new.df[,c('Close','close.sma.n25', 'close.sd.n25')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n26 <- apply(new.df[,c('Close','close.sma.n26', 'close.sd.n26')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n27 <- apply(new.df[,c('Close','close.sma.n27', 'close.sd.n27')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n28 <- apply(new.df[,c('Close','close.sma.n28', 'close.sd.n28')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n29 <- apply(new.df[,c('Close','close.sma.n29', 'close.sd.n29')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n30 <- apply(new.df[,c('Close','close.sma.n30', 'close.sd.n30')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n31 <- apply(new.df[,c('Close','close.sma.n31', 'close.sd.n31')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n32 <- apply(new.df[,c('Close','close.sma.n32', 'close.sd.n32')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n33 <- apply(new.df[,c('Close','close.sma.n33', 'close.sd.n33')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n34 <- apply(new.df[,c('Close','close.sma.n34', 'close.sd.n34')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n35 <- apply(new.df[,c('Close','close.sma.n35', 'close.sd.n35')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n36 <- apply(new.df[,c('Close','close.sma.n36', 'close.sd.n36')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n37 <- apply(new.df[,c('Close','close.sma.n37', 'close.sd.n37')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n38 <- apply(new.df[,c('Close','close.sma.n38', 'close.sd.n38')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n39 <- apply(new.df[,c('Close','close.sma.n39', 'close.sd.n39')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n40 <- apply(new.df[,c('Close','close.sma.n40', 'close.sd.n40')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n41 <- apply(new.df[,c('Close','close.sma.n41', 'close.sd.n41')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n42 <- apply(new.df[,c('Close','close.sma.n42', 'close.sd.n42')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n43 <- apply(new.df[,c('Close','close.sma.n43', 'close.sd.n43')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n44 <- apply(new.df[,c('Close','close.sma.n44', 'close.sd.n44')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n45 <- apply(new.df[,c('Close','close.sma.n45', 'close.sd.n45')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n46 <- apply(new.df[,c('Close','close.sma.n46', 'close.sd.n46')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n47 <- apply(new.df[,c('Close','close.sma.n47', 'close.sd.n47')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n48 <- apply(new.df[,c('Close','close.sma.n48', 'close.sd.n48')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n49 <- apply(new.df[,c('Close','close.sma.n49', 'close.sd.n49')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n50 <- apply(new.df[,c('Close','close.sma.n50', 'close.sd.n50')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n51 <- apply(new.df[,c('Close','close.sma.n51', 'close.sd.n51')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n52 <- apply(new.df[,c('Close','close.sma.n52', 'close.sd.n52')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n53 <- apply(new.df[,c('Close','close.sma.n53', 'close.sd.n53')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n54 <- apply(new.df[,c('Close','close.sma.n54', 'close.sd.n54')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n55 <- apply(new.df[,c('Close','close.sma.n55', 'close.sd.n55')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n56 <- apply(new.df[,c('Close','close.sma.n56', 'close.sd.n56')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n57 <- apply(new.df[,c('Close','close.sma.n57', 'close.sd.n57')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n58 <- apply(new.df[,c('Close','close.sma.n58', 'close.sd.n58')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n59 <- apply(new.df[,c('Close','close.sma.n59', 'close.sd.n59')], 1, function(x) { (x[1]-x[2])/x[3] } )
  #new.df$close.zscore.n60 <- apply(new.df[,c('Close','close.sma.n60', 'close.sd.n60')], 1, function(x) { (x[1]-x[2])/x[3] } )
  
  # Use TTR package to create rolling MACD
  fast <- 3 #3
  slow <- 40  #40
  signal <- 5 # 5 tests good
  new.df$ema.fast <- EMA(new.df[,"Close"], fast)
  new.df$ema.slow <- EMA(new.df[,"Close"], slow)
  new.df$macd <- apply(new.df[,c('ema.fast','ema.slow')], 1, function(x) { (x[1]-x[2])} )
  new.df$signal <- EMA(new.df[,"macd"], signal)
  
  
  
  # Calculate quartiles, where close is relation to range (Close - High) / (High - Low)
  #new.df$quartile <- apply(new.df[,c('Close', 'Low', 'High')], 1, function(x) { (x[1]-x[2])/(x[3]-x[2])} )
  
  # Compute daily price differences 
  # We replicate NA 1 time in order to maintain correct positioning of differences
  # Within the data frame
  new.df$close.diff <- c(rep(NA, 1), diff(new.df$Close, lag = 1, differences = 1, arithmetic = TRUE, na.pad = TRUE))
  
  # Convert all NA to 0
  new.df[is.na(new.df)] <- 0
  
  # Calculate Returns from open to close 
  new.df$ocret <- apply(new.df[,c('Open', 'Close')], 1, function(x) { (x[2]-x[1])/x[1]} )
  
  # Calculate Close-to-Close returns
  new.df$clret <- ROC(new.df$Close, type = c("discrete"))
  new.df$clret[1] <- 0
  
  # Subset Date
  # Pick date of event subtract 6 months
  start.date <- date.list - 6*30
  #date.list <- date.list + 3*30
  new.df <- subset(new.df, Date >= as.Date(start.date) & Date <= as.Date(date.list))
  
  # Name indicators #
  #train.indicator <- train.set$close.zscore.n10
  #test.indicator <- test.set$close.zscore.n10
  macd <- new.df$macd
  signal <- new.df$signal
  rsi.sma <- new.df$rsi.sma.n2
  rsi.ema <- new.df$rsi.ema.n2
  
  # Enter buy / sell rules
  #new.df$enter.momo <- ifelse(macd > signal, 1,0)
  #new.df$exit.momo <- ifelse(macd < signal, 1,0)
  #new.df$enter.mean.rev <- ifelse(macd < signal, 1,0)
  #new.df$exit.mean.rev <- ifelse(macd > signal, 1,0)
  new.df$enter.momo <- ifelse(rsi.sma > 70, 1,0)
  new.df$exit.momo <- ifelse(rsi.sma < 85, 1,0)
  new.df$enter.mean.rev <- ifelse(rsi.sma < 20, 1,0)
  new.df$exit.mean.rev <- ifelse(rsi.sma > 70, 1,0)
  
  #plot(new.df$rsi.sma.n2,type="l")
  
  # Momo
  new.df <- new.df %>%
    dplyr::mutate(sig.momo = ifelse(enter.momo == 1, 1,
                                    ifelse(exit.momo ==1, 0, 0)))
  
  # Mean Rev
  new.df <- new.df %>%
    dplyr::mutate(sig.mean.rev = ifelse(enter.mean.rev == 1, 1,
                                        ifelse(exit.mean.rev == 1, 0, 0)))
  
  # lag signal by one forward day to signal entry next day 
  new.df$sig.momo <- lag(new.df$sig.momo,1) # Note k=1 implies a move *forward*
  new.df$sig.mean.rev <- lag(new.df$sig.mean.rev,1) # Note k=1 implies a move *forward*
  
  new.df[is.na(new.df)] <- 0  # Set NA to 0
  
  # Calculate equity curves
  # Momentum
  new.df <- new.df %>%
    dplyr::mutate(RunID = rleid(sig.momo)) %>%
    group_by(RunID) %>%
    dplyr::mutate(equity.curve.momo = ifelse(sig.momo == 0, 0,
                                             ifelse(row_number() == 1, ocret, clret))) %>%
    ungroup() %>%
    select(-RunID)
  
  # Mean Rev
  new.df <- new.df %>%
    dplyr::mutate(RunID = rleid(sig.mean.rev)) %>%
    group_by(RunID) %>%
    dplyr::mutate(equity.curve.mean.rev = ifelse(sig.mean.rev == 0, 0,
                                                 ifelse(row_number() == 1, ocret, clret))) %>%
    ungroup() %>%
    select(-RunID)
  
  # Pull select columns from data frame to make XTS whilst retaining formats 
  xts1 = xts(new.df$equity.curve.momo, order.by=as.POSIXct(new.df$Date, format="%Y-%m-%d %H:%M")) 
  xts2 = xts(new.df$equity.curve.mean.rev, order.by=as.POSIXct(new.df$Date, format="%Y-%m-%d %H:%M")) 
  xts3 = xts(new.df$clret, order.by=as.POSIXct(new.df$Date, format="%Y-%m-%d %H:%M")) 
  
  # Join XTS together 
  compare <- cbind(xts1,xts2,xts3)
  
  # Use the PerformanceAnalytics package for trade statistics
  colnames(compare) <- c("Momentum","Mean Reversion","Buy And Hold")
  charts.PerformanceSummary(compare,main="Cumulative Returns", wealth.index=TRUE, colorset=rainbow12equal)
  #performance.table <- rbind(maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
  #drawdown.table <- rbind(table.Drawdowns(compare))
  #table.AnnualizedReturns(compare)
  #dev.off()
  #logRets <- log(cumprod(1+compare))
  #chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal)
  
  # Place time in a vector for .png file name differentiation
  time <- as.character(Sys.time())
  time <- gsub("[^[:alnum:]]", "", time)
  
  #print(performance.table)
  #print(drawdown.table)
  # Save plot out put to folder
  mypath <- file.path(png.output.dir,paste(time,next.png[1],".png"))
  #  mypath <- file.path(png.output.dir,paste(next.png[1], ".png", sep = ""))
  png(file=mypath,width= 1400, height=1000)
  mytitle = paste0(next.png[1])
  #png(file=sprintf("Rplot_%s_%s",mypath,".png", format(Sys.time(), '%d%m%Y_%H_%M'), '%03d'))
  #charts.PerformanceSummary(compare,main=mytitle,wealth.index=TRUE,colorset=rainbow12equal)
  chart.CumReturns(compare,main=mytitle,wealth.index=TRUE,colorset=rainbow12equal)
  dev.off()
  #png(filename = "C:/R Projects/moo.png") #width= 1000, height=800)
  #chart.CumReturns(compare,wealth.index=TRUE,colorset=rainbow12equal)
  #chart.Drawdown(compare,geometric =TRUE,colorset=rainbow12equal)
  #dev.off()
  #charts.PerformanceSummary(compare,main="Cumulative Returns"=TRUE,colorset=rainbow12equal)
  
  # Prepare Data For Cumulative and compound returns 
  # Calculate annualized and cumulatuve return 
  # Make prices vector
  # Momoentum 
  momo.prices <- new.df[,"equity.curve.momo", drop = FALSE]
  colnames(new.df)
  
  # Denote n the number of time periods:
  #n <- nrow(momo.prices)
  
  # Calculate close to close returns
  # lead in with rep,NA,1 to maintain length of vector comparible to data frame
  #momo.close_ret <- c(rep(NA, 1),(momo.prices[2:n, 1] - momo.prices[1:(n-1), 1])/momo.prices[1:(n-1), 1])
  #close_ret[1] <- 0
  
  # Compute continuously  returns (log returns)
  #close_ccret <- log(prices[2:n, 1]) - log(prices[1:(n-1), 1])
  
  # Compute gross returns
  momo.close_gret <- 1 + momo.prices   # use close to close ret
  
  # Compute future values
  momo.close_fv <- cumprod(momo.close_gret)
  
  # Obtain first and last values
  momo.ret.last <- tail(momo.close_fv, n=1)
  momo.ret.first <- head(momo.close_fv, n=1)
  momo.cum.ret <- (momo.ret.last-momo.ret.first)/momo.ret.first
  
  # Get First And Last row to calculate time between
  momo.ret.first.row <- head(new.df$Date, n=1)
  momo.ret.last.row <- tail(new.df$Date, n=1)
  
  # Time diff 
  #trading.years.between <- as.numeric(difftime(as.Date(ret.last.row), as.Date(ret.first.row), unit="weeks"))/52.25
  
  # Find time diff
  momo.ret.time <- momo.ret.last.row - momo.ret.first.row
  momo.ret.trading.years.between <- momo.ret.time/365   #252 trading days or 365 
  momo.ret.trading.years.between <- as.numeric(momo.ret.trading.years.between, units="days")   # Extract numerical value from time difference 'Time difference of 2837.208 days'
  # Annualized return
  # (1 + % diff of final) / (last balance to the power of 1/time first and last balance) -1
  momo.ret.annual.return <- (1+momo.cum.ret) ^ (1/momo.ret.trading.years.between) -1
  
  # Prepare Data For Cumulative and compound returns 
  # Calculate annualized and cumulatuve return 
  # Make prices vector
  # Mean Reversion
  mean.rev.prices <- new.df[,"equity.curve.mean.rev", drop = FALSE]
  
  # Denote n the number of time periods:
  #n <- nrow(mean.rev.prices)
  
  # Calculate close to close returns
  # lead in with rep,NA,1 to maintain length of vector comparible to data frame
  #momo.close_ret <- c(rep(NA, 1),(momo.prices[2:n, 1] - momo.prices[1:(n-1), 1])/momo.prices[1:(n-1), 1])
  #close_ret[1] <- 0
  
  # Compute continuously  returns (log returns)
  #close_ccret <- log(prices[2:n, 1]) - log(prices[1:(n-1), 1])
  
  # Compute gross returns
  mean.rev.close_gret <- 1 + mean.rev.prices   # use close to close ret
  
  # Compute future values
  mean.rev.close_fv <- cumprod(mean.rev.close_gret)
  
  # Obtain first and last values
  mean.rev.ret.last <- tail(mean.rev.close_fv, n=1)
  mean.rev.ret.first <- head(mean.rev.close_fv, n=1)
  mean.rev.cum.ret <- (mean.rev.ret.last-mean.rev.ret.first)/mean.rev.ret.first
  
  # Get First And Last row to calculate time between
  mean.rev.ret.first.row <- head(new.df$Date, n=1)
  mean.rev.ret.last.row <- tail(new.df$Date, n=1)
  
  # Time diff 
  #trading.years.between <- as.numeric(difftime(as.Date(ret.last.row), as.Date(ret.first.row), unit="weeks"))/52.25
  
  # Find time diff
  mean.rev.ret.time <- mean.rev.ret.last.row - momo.ret.first.row
  mean.rev.ret.trading.years.between <- mean.rev.ret.time/365   #252 trading days or 365 
  mean.rev.ret.trading.years.between <- as.numeric(mean.rev.ret.trading.years.between, units="days")   # Extract numerical value from time difference 'Time difference of 2837.208 days'
  # Annualized return
  # (1 + % diff of final) / (last balance to the power of 1/time first and last balance) -1
  mean.rev.ret.annual.return <- (1+mean.rev.cum.ret) ^ (1/mean.rev.ret.trading.years.between) -1
  
  # Buy And Hold Annualized / Cumulative 
  # Extract by and hold equity curve 
  buy.hold.prices <- new.df[,"clret", drop = FALSE]
  # Denote n the number of time periods:
  #n <- nrow(mean.rev.prices)
  
  # Calculate close to close returns
  # lead in with rep,NA,1 to maintain length of vector comparible to data frame
  #momo.close_ret <- c(rep(NA, 1),(momo.prices[2:n, 1] - momo.prices[1:(n-1), 1])/momo.prices[1:(n-1), 1])
  #close_ret[1] <- 0
  
  # Compute continuously  returns (log returns)
  #close_ccret <- log(prices[2:n, 1]) - log(prices[1:(n-1), 1])
  
  # Compute gross returns
  buy.hold.close.gret <- 1 + buy.hold.prices   # use close to close ret
  
  # Compute future values
  buy.hold.close_fv <- cumprod(buy.hold.close.gret)
  
  # Obtain first and last values
  buy.hold.ret.last <- tail(buy.hold.close_fv, n=1)
  buy.hold.ret.first <- head(buy.hold.close_fv, n=1)
  buy.hold.cum.ret <- (buy.hold.ret.last-buy.hold.ret.first)/buy.hold.ret.first
  
  # Get First And Last row to calculate time between
  buy.hold.ret.first.row <- head(new.df$Date, n=1)
  buy.hold.ret.last.row <- tail(new.df$Date, n=1)
  
  # Time diff 
  #trading.years.between <- as.numeric(difftime(as.Date(ret.last.row), as.Date(ret.first.row), unit="weeks"))/52.25
  
  # Find time diff
  buy.hold.ret.time <- buy.hold.ret.last.row - momo.ret.first.row
  buy.hold.ret.trading.years.between <- buy.hold.ret.time/365   #252 trading days or 365 
  buy.hold.ret.trading.years.between <- as.numeric(buy.hold.ret.trading.years.between, units="days")   # Extract numerical value from time difference 'Time difference of 2837.208 days'
  # Annualized return
  # (1 + % diff of final) / (last balance to the power of 1/time first and last balance) -1
  buy.hold.ret.annual.return <- (1+buy.hold.cum.ret) ^ (1/buy.hold.ret.trading.years.between) -1
  
  # Count days 6 months before FDA date (6*30 days prior)
  new.df$rep <- rep(1:NROW(new.df), each = 1, len = NROW(new.df))
  
  
  
  # Group by day rep number 
  #final <- new.df %>%
  # group_by(rep) %>%
  #summarize(n=n(),mn=mean(close.diff),sd=sd(close.diff))
  
  # Create the output of the function : a named data.frame 
  #out_df <- data.frame("Momo Annualized Return" = momo.ret.annual.return[,1],
   #                    "Momo Cumulative Return" = momo.cum.ret[,1],
    #                   "Mean Reversion Annualized Return" = mean.rev.ret.annual.return[,1],
     #                 "Mean Reversion Cumulative Return" = mean.rev.cum.ret[,1],
       #              "Buy And Hold Annualized Return" = buy.hold.ret.annual.return[,1],
             #          "Buy And Hold Cumulative Return" = buy.hold.cum.ret[,1])
  
  # Create data output of rep and close.diff columns rbind
  data_output_df <- data.frame("Ticker" = new.df$Ticker,
                               "Rep" = new.df$rep,
                               #"Close Differences" = new.df$close.diff )
                               "Close Ret" = new.df$clret)
  
  # Sanity Check performanceAnalystics to confirm cum / annual return 
  #Return.annualized(xts1)
  #Return.cumulative(xts1,geometric = TRUE)
  
  #return(out_df)
  #return(data_output_df)
  
}

# Study results in data frame 
# Note this used out_df that is initialized top of script
# Make row names a column
cum_ret.results <- tibble::rownames_to_column(cum_ret,"Symbol")
#cum_ret1$decile <- ntile(cum_ret$Momo.Cumulative.Return,10)
# Sort Results 
cum_ret.results <- arrange(cum_ret.results,desc(Momo.Cumulative.Return)) # Sort by Momentum cumulative return
head(cum_ret.results,50) # Print console top 50
cum_ret.results <- arrange(cum_ret.results,desc(Mean.Reversion.Cumulative.Return)) # Sort by Mean Reversion cumulative return
NROW(cum_ret.results)
head(cum_ret.results,50) # Print console top 50
cum_ret.results <- arrange(cum_ret.results,desc(Buy.And.Hold.Cumulative.Return)) # Sort by Buy and Hold cumulative return
head(cum_ret.results,50) # Print console top 50

# Split by month... 1,2,3,4,5,6
# Take mean daily change per time period for each month 
# See if anything notable per time
# to do 

# Group by event type
#to do

# Some Statistics 
# Positive cumulative, neg, total, min max
pos <- sum(cum_ret.results$Buy.And.Hold.Cumulative.Return >0, na.rm = TRUE)
neg <- sum(cum_ret.results$Buy.And.Hold.Cumulative.Return <0, na.rm = TRUE)
total <- pos+neg
pos/750
sum(cum_ret.results$Buy.And.Hold.Cumulative.Return ==0, na.rm = TRUE)
mean(cum_ret.results$Buy.And.Hold.Cumulative.Return)
max(cum_ret.results$Buy.And.Hold.Cumulative.Return)

# Subset by cumulative return 
cum.pos <- filter(cum_ret.results, Buy.And.Hold.Cumulative.Return >0)
str(cum.pos)
mean(cum.pos$Buy.And.Hold.Cumulative.Return)
plot(cum.pos$Buy.And.Hold.Cumulative.Return)

tail(data_output_df)

# Perform mean of all results
mean <- data_output_df %<>%
  group_by(Rep) %>%
  summarise(mean=mean(Close.Differences,na.rm = TRUE))

# Plot 
library(ggplot2)
ggplot(mean, aes(Rep, mean)) +
  geom_col()+
  theme_classic()+
  scale_x_continuous(breaks = seq(0, 127, by = 7))+
  ggtitle("Mean Daily Spread By Trading Day Pre/Post FDA Event - Day 63 = FDA Event", subtitle = "Biopharmcatlyst") +
  labs(x="Day",y="Mean Daily Spread")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))




# Loop for running function over all files in fda list (note files to be held on hard drive)
for (i in 1:length(tickers)){
  tryCatch({
    temp <- genCHART(tickers[[i]])
    #rownames(temp) <- paste0("Daily_",row.names[i])
    #cum_ret <- rbind.data.frame(cum_ret, temp)
    data_output_df <- rbind.data.frame(data_output_df,temp)
    ptm0 <- proc.time()
    Sys.sleep(0.1)  
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

#Save file
# Write output to file
#write.csv(cum_ret.results,file="D:/R Projects/Biotech Catalyst Study/fda.first.output.csv")

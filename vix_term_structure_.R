# Vix Term Strcuture 

require(xts)
require(data.table)
require(ggplot2)
require(lubridate)
require(magrittr)
require(scales)
require(reshape2)
require(dplyr)
require(PerformanceAnalytics)

# 06 through 18
years <- c(paste0("0", c(6:9)), as.character(c(10:18)))

# futures months
futMonths <- c("F", "G", "H", "J", "K", "M",
               "N", "Q", "U", "V", "X", "Z")

# expiries come from http://www.macroption.com/vix-expiration-calendar/
expiries <- read.table("D:/R Projects/Final Scripts/VIX_term_structure/expiries.txt", header=FALSE)

# convert expiries into dates in R
dateString <- paste(expiries$V3, expiries$V2, expiries$V1, sep = "-")
dates <- as.Date(dateString, format = "%Y-%B-%d")

# map futures months to numbers for dates
monthMaps <- cbind(futMonths, c("01", "02", "03", "04", "05", "06",
                                "07", "08", "09", "10", "11", "12"))
monthMaps <- data.frame(monthMaps)
colnames(monthMaps) <- c("futureStem", "monthNum")

dates <- data.frame(dates)
dates$dateMon <- substr(dates$dates, 1, 7) # Extract year month only

contracts <- expand.grid(futMonths, years)
contracts <- paste0(contracts[,1], contracts[,2])
#contracts <- c(contracts, "F18")
stem <- "https://cfe.cboe.com/Publish/ScheduledTask/MktData/datahouse/CFE_"
#contracts <- paste0(stem, contracts, "_VX.csv")

masterlist <- list()
timesToExpiry <- list()
i=1
for(i in 1:length(contracts)) {
  
  # obtain data
  contract <- contracts[i]
  dataFile <- paste0(stem, contract, "_VX.csv")
  expiryYear <- paste0("20",substr(contract, 2, 3)) # Paste element 2 and 3 from the contract xYY
  expiryMonth <- monthMaps$monthNum[monthMaps$futureStem == substr(contract,1,1)]
  expiryDate <- dates$dates[dates$dateMon == paste(expiryYear, expiryMonth, sep="-")]
  expiryYear
  expiryMonth
  expiryDate
  data <- suppressWarnings(fread(dataFile))
  tail(data)
  # create dates
  dataDates <- as.Date(data$`Trade Date`, format = '%m/%d/%Y')
  
  # create time to expiration xts
  toExpiry <- xts(expiryDate - dataDates, order.by=dataDates)
  colnames(toExpiry) <- contract
  timesToExpiry[[i]] <- toExpiry
  
  # get settlements
  settlement <- xts(data$Settle, order.by=dataDates)
  colnames(settlement) <- contract
  masterlist[[i]] <- settlement
}
i
# cbind outputs
masterlist <- do.call(cbind, masterlist)
timesToExpiry <- do.call(cbind, timesToExpiry)
head(timesToExpiry,200)
ncol(masterlist)
ncol(timesToExpiry)

#write.csv(timesToExpiry,file="D:/R Projects/time_to_expirary.csv")

# NA out zeroes in settlements
masterlist[masterlist==0] <- NA

sumNonNA <- function(row) {
  return(sum(!is.na(row)))
}

simultaneousContracts <- xts(apply(masterlist, 1, sumNonNA), order.by=index(masterlist))
chart.TimeSeries(simultaneousContracts)

dim(masterlist)
nrow(masterlist)
ncol(masterlist)
tail(masterlist[,135:145])

i=1
termStructure <- list()
expiryStructure <- list()
masterDates <- unique(c(first(index(masterlist)), dates$dates[dates$dates %in% index(masterlist)], Sys.Date()-1)) # %in% operator matches dates, sys.date-1 to include final range date
for(i in 1:(length(masterDates)-1)) {
  subsetDates <- masterDates[c(i, i+1)]
  dateRange <- paste(subsetDates[1], subsetDates[2], sep="::")
  subset <- masterlist[dateRange,c(i:(i+7))]
  subset <- subset[-1,]
  expirySubset <- timesToExpiry[index(subset), c(i:(i+7))]
  colnames(subset) <- colnames(expirySubset) <- paste0("C", c(1:8))
  termStructure[[i]] <- subset
  expiryStructure[[i]] <- expirySubset
}

termStructure <- do.call(rbind, termStructure)
expiryStructure <- do.call(rbind, expiryStructure)

simultaneousContracts <- xts(apply(termStructure, 1, sumNonNA), order.by=index(termStructure))
chart.TimeSeries(simultaneousContracts)

plot(t(coredata(last(termStructure))), type = 'b')
# Plot specific date term structure
backwardation <- termStructure["2017-11-30"] # Subset specific date
back.df <- as.data.frame(backwardation)
back.df <- setDT(back.df, keep.rownames = TRUE)[] # Set row names
colnames(back.df)[1] <- "Date"
back.df$Date <- ymd(back.df$Date)
back.df <- melt(data = back.df,id.vars = 'Date')   # melt df for plotting with ggplot2
colnames(back.df)[2] <- "Contract"

# plot
ggplot(data=back.df,aes(x=Contract,y=value,group = 1))+
  geom_point()+  geom_line()+
  theme_classic()+
  ggtitle("VIX Term Structure for Date 2017-11-30",subtitle="Example of Contango")+
  labs(x="Contract",y="Settlement")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))


# Prepare Data for plotting varying contract expirations
# Adjust for 10:1 split pre 03-26-2007
# Split information here: http://cfe.cboe.com/publish/CFEinfocirc/CFEIC07-003%20.pdf
# Change termStucture column names
colnames(termStructure)[1] = "F"
colnames(termStructure)[2] = "G"
colnames(termStructure)[3] = "H"
colnames(termStructure)[4] = "J"
colnames(termStructure)[5] = "K"
colnames(termStructure)[6] = "M"
colnames(termStructure)[7] = "N"
colnames(termStructure)[8] = "Q"

# Prepare Data For backwardation Plot
# Obtain backwardation dates 
backwardation <- data.frame(termStructure)
backwardation <- setDT(backwardation, keep.rownames = TRUE)[] # Set row names
colnames(backwardation)[1] <- "Date"
head(backwardation)
backwardation.G <- ifelse(termStructure$F > termStructure$G,1,0)
backwardation.H <- ifelse(termStructure$G > termStructure$H,1,0)
backwardation.J <- ifelse(termStructure$H > termStructure$J,1,0)
backwardation.K <- ifelse(termStructure$J > termStructure$K,1,0)
backwardation.M <- ifelse(termStructure$K > termStructure$M,1,0)
backwardation.N <- ifelse(termStructure$M > termStructure$N,1,0)
backwardation.vec <- backwardation.G + backwardation.H + backwardation.J + backwardation.K + backwardation.M + backwardation.N
dates <- c(backwardation$Date)
backwardation.df <- data.frame(dates,backwardation.vec)
colnames(backwardation.df)[1] <- "Date"
colnames(backwardation.df)[2] <- "backwardation"

# Plot Synthetic VXX Data
# Save xts series to date frame in order to melt 
df <- data.frame(termStructure)
df <- setDT(df, keep.rownames = TRUE)[] # Set row names
colnames(df)[1] = "Date"
df.10.split <- subset(df, Date < as.POSIXct("2007-03-26/") ) # subset data prior to split
library(magrittr)
df.10.split[,2:9] %<>% lapply(function(x) x / 10) # appply split to all columns excluding date 1
df.post.split <- subset(df, Date >= as.POSIXct("2007-03-26/") )  # subset post split
df <- rbind(df.10.split,df.post.split) # rbind pre and post split data frames
df <- melt(data = df,id.vars = 'Date')   # melt df for plotting with ggplot2
library(lubridate)
df$Date <- ymd(df$Date)  # Convert date column to Date format
colnames(df)[2] = "Contract" # Rename

# Plot Term Strcuture with backwardation
# Subset Dates 
backwardation.dates <- subset(backwardation.df, backwardation >= 1)
backwardation.dates$Date <- ymd(backwardation.dates$Date)
str(backwardation.dates)
backwardation <- backwardation.dates$Date

ggplot(data=df,aes(x=Date,y=value,colour=Contract))+
  geom_line()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("VIX Term Structure",subtitle="2007-06-01 to 2010-01-01 - Pink = backwardation")+
  labs(x="Year",y="Settlement")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  geom_vline(xintercept = backwardation, color = "red", size=1,alpha=0.01)

# Plot term structure
ggplot(data=df,aes(x=Date,y=value,colour=Contract))+
  geom_line()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("VIX Term Structure",subtitle="2007-06-01 to 2010-01-01")+
  labs(x="Year",y="Settlement")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))

# Plot term structure between date range
# Subet by date (for plotting any window for further examination) 
df <- subset(df, Date >= as.POSIXct("2007-06-01") & Date <= as.POSIXct("2010-01-01") ) 
ggplot(data=df,aes(x=Date,y=value,colour=Contract))+
  geom_line()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("VIX Term Structure",subtitle="2007-06-01 to 2010-01-01")+
  labs(x="Year",y="Settlement")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))

# Count how often market in backwardation / contango 
total.backwardation.days <- sum(backwardation.df$backwardation > 0, na.rm=TRUE)
total.days <- nrow(backwardation.df)
backwardation.perc <- total.backwardation.days/total.days *100
contango = 100-backwardation.perc
print(contango)
print(backwardation.perc)

# Beta Convexity 
# Download SPY data
# Note you need tyo place your API key...your_key_here
SPY <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=SPY&outputsize=full&apikey=your_key_here&datatype=csv") #fread() data.table for downloading directly to a data frame
SPY$timestamp <- ymd(SPY$timestamp)   #Lubridate to change character date to date format
SPY <- arrange(SPY,timestamp)   #dplyr to sort data frame by date ascending order
colnames(SPY)[1] <- "Date"
SPY$Date <- ymd(SPY$Date)
SPY <- as.data.frame(SPY)
SPY <- subset(SPY, Date >= as.POSIXct("2009-01-30") ) # VXX start
SPY <- subset(SPY, Date >= as.POSIXct("2000-01-03")) # XIV start

VXX <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=VXX&outputsize=full&apikey=your_key_here&datatype=csv") #fread() data.table for downloading directly to a data frame
VXX$timestamp <- ymd(VXX$timestamp)   #Lubridate to change character date to date format
VXX <- arrange(VXX,timestamp)   #dplyr to sort data frame by date ascending order
colnames(VXX)[1] <- "Date"
VXX$Date <- ymd(VXX$Date)
VXX <- as.data.frame(VXX)

XIV <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=XIV&outputsize=full&apikey=your_key_here&datatype=csv") #fread() data.table for downloading directly to a data frame
XIV$timestamp <- ymd(XIV$timestamp)   #Lubridate to change character date to date format
XIV <- arrange(XIV,timestamp)   #dplyr to sort data frame by date ascending order
colnames(XIV)[1] <- "Date"
XIV$Date <- ymd(XIV$Date)
XIV <- as.data.frame(XIV)
XIV <- subset(XIV, Date >= as.POSIXct("2000-01-03 "))

# Calculate returns 
spy.xts = xts(SPY$adjusted_close, order.by=as.Date(SPY$Date, format="%Y/%m/%d"))
vxx.xts = xts(VXX$adjusted_close, order.by=as.Date(VXX$Date, format="%Y/%m/%d"))
xiv.xts = xts(XIV$adjusted_close, order.by=as.Date(XIV$Date, format="%Y/%m/%d"))
spy.ret <- Return.calculate(spy.xts)
vxx.ret <- Return.calculate(vxx.xts)
xiv.ret <- Return.calculate(xiv.xts)

rets <- na.omit(cbind(xiv.ret, spy.ret))
colnames(rets) <- c("XIV", "SPY")

betaConvexity <- function(Ra, Rb) {
  positiveBench <- Rb[Rb > 0]
  assetPositiveBench <- Ra[index(positiveBench)]
  positiveBeta <- CAPM.beta(Ra = assetPositiveBench, Rb = positiveBench)
  
  negativeBench <- Rb[Rb < 0]
  assetNegativeBench <- Ra[index(negativeBench)]
  negativeBeta <- CAPM.beta(Ra = assetNegativeBench, Rb = negativeBench)
  
  out <- (positiveBeta - negativeBeta) ^ 2
  return(out)
}

betaConvexity(rets$XIV, rets$SPY)

xiv <- c(rets$XIV)
spy <- c(rets$SPY)

# Run rolling linear regression for beta
# Stock SPY as independant variable
rolling_lms <- lapply(seq(20,nrow(rets)), function(x) lm(XIV ~ SPY, data = rets[1:x , ]) )
length(rolling_lms)
nrow(rets)
all_slopes <-unlist(sapply(1:length(rolling_lms),function(j) rolling_lms[[j]]$coefficients[2]))
all_slopes<- unlist(all_slopes)
plot(all_slopes,type="l")
# Join regression output to original data frame 
row.diff <- rep(NA, nrow(rets) - length(rolling_lms)) # make leading NAs to position data correclty with original
beta <- c(row.diff,all_slopes)
rets <- data.frame(rets,beta)
rets <- setDT(rets, keep.rownames = TRUE)[] # Set row names
colnames(rets)[1] <- "Date"
rets$Date <- ymd(rets$Date)

# Plot beta 
ggplot() +
  geom_line(data=rets,aes(x=Date,y=beta), colour="red") +
  theme_classic()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("Rolling Linear Regression - Plot XIV Beta", subtitle = "XIV Inception") +
  labs(x="Date",y="VXX")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))

lm.fit <- lm(rets$SPY~rets$XIV) # regression line (y~x)

# Plot Linear Regression
ggplot(rets, aes(x=SPY, y=XIV))+ 
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  ggtitle("Linear Regression y~x XIV~SPY", subtitle = "Start date = XIV inception") +
  labs(x="SPY",y="XIV")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))
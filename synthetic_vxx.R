# Vix Term Stucture
# Synthetic VXX

require(xts)
require(data.table)
require(ggplot2)
require(lubridate)
require(magrittr)
require(scales)
require(reshape2)


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
head(termStructure)

write.csv(masterlist,file="D:/R Projects/masterlist.csv")
#This script allows you to calculate proxy values for the VXX ETF before its inception on the 29t of January 2009
#Prerequisites:

#obtain historical CBOE VIX Futures data following the instructions outlined in this post by Ilya Kipnis (QuantStratTradeR)
#https://quantstrattrader.wordpress.com/2017/04/27/creating-a-vix-futures-term-structure-in-r-from-official-cboe-settlement-data/

#have the variables expiries and masterlist defined exactly as described in the post above

#have obtained TBill Rates from https://www.treasurydirect.gov/instit/annceresult/annceresult_query.htm and manipulated as described in my post

#Loading required packages
require(xts)
require(bizdays)
require(timeDate)
load_rmetrics_calendars(2004:2018)

# expiries come from http://www.macroption.com/vix-expiration-calendar/
expiries <- read.table("D:/R Projects/Final Scripts/VIX_term_structure/expiries_1.txt", header=FALSE)


#Transforming the expiries
expiries = as.Date(apply(expiries,1,function(x){paste(x,collapse=" ")}),format="%d %b %Y")

#preparing the tbillrates
tbillrates <- read.csv("D:/R Projects/Final Scripts/VIX_term_structure/tbills.2.csv", header=TRUE, stringsAsFactors = FALSE)
tbillrates[is.na(tbillrates)] <- 0
tbillrates$Date <- mdy(tbillrates$Date)
str(tbillrates)
tbillrates = xts(tbillrates[,"Rate"],as.Date(tbillrates[,"Date"]))

#defining function to calculate the contract roll weights
getCRW <- function(today){
  today = as.Date(today)
  periodstart = expiries[max(which(expiries<=today))]
  periodend = expiries[min(which(expiries>today))]
  dt = bizdays(periodstart,periodend,"Rmetrics/NYSE")
  dr = bizdays(today,periodend,"Rmetrics/NYSE")-1
  return(c(dr/dt,(dt-dr)/dt))
}

#defining function to calculate TBR
getTBR <- function(today,lastday){
  today = as.Date(today)
  lastday = as.Date(lastday)
  delta = as.numeric(today-lastday)
  rate = tbillrates[max(which(index(tbillrates)<today))]
  tbr = (1/(1-91/360*rate))^(delta/91)-1
  return(tbr)
}

i=1
#calculating the index values
days = index(masterlist["2005-12-20/2009-01-29"])
indx = 100000
for(i in 2:length(days)){
  crw = getCRW(days[i-1])
  tbr = getTBR(days[i],days[i-1])
  fut1 = masterlist[days[i-1],which(!is.na(masterlist[days[i-1]]))[1:2]]
  fut2 = masterlist[days[i],which(!is.na(masterlist[days[i]]))[1:2]]
  if(!names(fut1)[1]%in%names(fut2)){
    fut1 = masterlist[days[i-1],which(!is.na(masterlist[days[i-1]]))[2:3]]
  }
  twdi = sum(crw*as.numeric(fut1))
  twdo = sum(crw*as.numeric(fut2))
  cdr = twdo/twdi-1
  #cdr = -cdr
  val = indx[length(indx)]*(1+cdr+tbr)
  indx = c(indx,val)
}
indx = xts(indx,days)

#adjusting for 10:1 split
indx["2007-03-26/"] = 10*indx["2007-03-26/"]
indxret = (indx/lag(indx)-1)[-1]

#calculating VXX values
vxxvals = 104.58 # Note enter starting date of VXX value (here using alpha vantage first VXX price) VXX Non Adjusted = 104.58
for(i in nrow(indxret):1){
  tmp = vxxvals[length(vxxvals)]/(1+indxret[i,])
  vxxvals = c(vxxvals,tmp)
}
vxxvals = rev(vxxvals)
vxxvals = xts(vxxvals,index(indx))
plot(vxxvals,type="l")
head(vxxvals)

# Join synthetic VXX data to Alpha Vantage VXX data
# Plot with ggplot2
# Download data
require(lubridate)
require(data.table)  
require(dplyr)       
# Note you need tyo place your API key...your_key_here
VXX <- fread("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=VXX&outputsize=full&apikey=your_key_here&datatype=csv") #fread() data.table for downloading directly to a data frame
VXX$timestamp <- ymd(VXX$timestamp)   #Lubridate to change character date to date format
VXX <- arrange(VXX,timestamp)   #dplyr to sort data frame by date ascending order
colnames(VXX)[1] <- "Date"
VXX <- as.data.frame(VXX)

# store synthetic VXX vxxvals price data to data.frame
df <- data.frame(vxxvals)
df <- setDT(df, keep.rownames = TRUE)[] # Set row names
colnames(df)[1] = "Date"
colnames(df)[2] = "close"
df$Date <- ymd(df$Date) # Convert to date format
df <- as.data.frame(df)

# Retain only Date and close columns for alphavantage VXX data
#VXX <- VXX[ -c(2:5, 7:9) ]  # subset adjusted close
colnames(VXX)[2] <- "close"
VXX <- VXX[ -c(2:4, 6) ]   # subset for non adjusted
head(VXX)

# ggplot2 to plot synthetic VXX and alphavantage VXX
require(ggplot2)
require(scales)
ggplot() +
  geom_line(data=df,aes(x=Date,y=close), colour="red") +
  geom_line(data=VXX,aes(x=Date,y=close), colour="black")+ 
  theme_classic()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("VXX", subtitle = "Synthetic prior to 2009-01-29") +
  labs(x="Date",y="VXX")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  annotate("text", label = "Synthetic VXX", x = as.Date("2007-04-26"), y = 100000, color = "red")+
  annotate("text", label = "Alphavantage VXX", x = as.Date("2015-04-26"), y = 100000, color = "black")

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

# Plot VXX with backwardation
# Subset Dates 
backwardation.dates <- subset(backwardation.df, backwardation >= 1)
backwardation.dates$Date <- ymd(backwardation.dates$Date)
str(backwardation.dates)
backwardation <- backwardation.dates$Date

ggplot() +
  geom_line(data=df,aes(x=Date,y=close), colour="red") +
  geom_line(data=VXX,aes(x=Date,y=close), colour="black")+ 
  theme_classic()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("VXX Non Adjusted Close", subtitle = "Synthetic prior to 2009-01-29 - Pink = backwardation") +
  labs(x="Date",y="VXX")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  annotate("text", label = "Synthetic VXX", x = as.Date("2007-04-26"), y = 100, color = "red")+
  annotate("text", label = "Alphavantage VXX", x = as.Date("2015-04-26"), y = 100, color = "black")+
  geom_vline(xintercept = backwardation, color = "red", size=1,alpha=0.01)
  
# VIX1|VIX2 30 Day Constant Maturity 
# Andrew Bannerman 12.10.2017 

require(xts)
require(data.table)
require(ggplot2)
require(lubridate)
require(magrittr)
require(scales)
require(reshape2)
require(dplyr)
require(PerformanceAnalytics)
require(quantmod)

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

settlement.dates <- data.frame(Date=dates, Expiration.Date=2)
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
  data <- suppressWarnings(fread(dataFile))
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

# NA out zeroes in settlements
masterlist[masterlist==0] <- NA

sumNonNA <- function(row) {
  return(sum(!is.na(row)))
}

simultaneousContracts <- xts(apply(masterlist, 1, sumNonNA), order.by=index(masterlist))
chart.TimeSeries(simultaneousContracts)

dim(masterlist)
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

# Save Term Structure to data frame
term.structure.df <- as.data.frame(termStructure)
term.structure.df <- setDT(term.structure.df, keep.rownames = TRUE)[] # Set row names
colnames(term.structure.df)[1] <- "Date"
term.structure.df$Date <- ymd(term.structure.df$Date)

# Adjust for 10:1 split pre 03-26-2007
# Split information here: http://cfe.cboe.com/publish/CFEinfocirc/CFEIC07-003%20.pdf
df.10.split <- subset(term.structure.df, Date < as.POSIXct("2007-03-26/") ) # subset data prior to split
library(magrittr)
df.10.split[,2:9] %<>% lapply(function(x) x / 10) # appply split to all columns excluding date 1
df.post.split <- subset(term.structure.df, Date >= as.POSIXct("2007-03-26/") )  # subset post split
term.structure.df <- rbind(df.10.split,df.post.split) # rbind pre and post split data frames
tail(term.structure.df,50)
length(term.structure.df)

# Find last dates
last <- tail(term.structure.df$Date,1)
last.settle <- tail(settlement.dates$Date,1)
date.fill <- as.data.frame(seq(as.Date(last), as.Date(last.settle), "days"))
colnames(date.fill)[1] = "Date"
date.excl.wknds <- date.fill[which(weekdays(as.Date(date.fill$Date, format = "%Y/%m/%d"))
                              %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

# Dummy data in order to fill out to future expiration dates
dates.df <- data.frame(date.excl.wknds,NA,NA,NA,NA,NA,NA,NA,NA)
colnames(dates.df)[1] <- "Date"
colnames(dates.df)[2] <- "C1"
colnames(dates.df)[3] <- "C2"
colnames(dates.df)[4] <- "C3"
colnames(dates.df)[5] <- "C4"
colnames(dates.df)[6] <- "C5"
colnames(dates.df)[7] <- "C6"
colnames(dates.df)[8] <- "C7"
colnames(dates.df)[9] <- "C8"
head(dates.df)
term.structure.df <- rbind(term.structure.df,dates.df)

# Calculate constant 30 day VIX1|VIX2 maturity
# Check weights http://www.ipathetn.com/US/16/en/details.app?instrumentId=259118
# USe method: http://app.velocitysharesetns.com/files/prospectus/PRICING_SUPPLEMENT_No__VLS_ETN-1_A36_long_form_2.pdf
# Join settlement dates 
term.structure.df <- full_join(term.structure.df, settlement.dates, by = c("Date" = "Date"))
term.structure.df$Expiration.Date[is.na(term.structure.df$Expiration.Date)] <- 1  # Turn all NA to 1
# Calculate days in roll period 
#term.structure.df <- subset(term.structure.df, Date >= as.POSIXct("2014-09-16") & Date <= as.POSIXct("2014-11-21")) # Subset between matches
term.structure.df$Day.in.roll.period <- NULL
for(i in 1:nrow(term.structure.df)) {
  term.structure.df$Day.in.roll.period[i] <- ifelse(term.structure.df$Expiration.Date[i + 1]==2,1,term.structure.df$Day.in.roll.period[i - 1]+1)
}

# Calculate dt = Total days in roll period
term.structure.df$dt <- NULL  # Initialize 
for(i in nrow(term.structure.df):1) {
  term.structure.df$dt[i] <- ifelse(term.structure.df$Expiration.Date[i+2]==2,term.structure.df$Day.in.roll.period[i],NA)
}
library(zoo)
dt.total.days.in.roll.period  <- na.locf(term.structure.df$dt, fromLast = TRUE) 
length(dt.total.days.in.roll.period)
diff <- nrow(term.structure.df) - length(dt.total.days.in.roll.period)  # Find difference in lengths 
dt.total.days.in.roll.period<- c(dt.total.days.in.roll.period ,rep(NA,diff)) # Pad out with NA to match
term.structure.df <- data.frame(term.structure.df,dt.total.days.in.roll.period)

# Calculate dr = Days remaining in roll period
term.structure.df$dr <- NULL  # Initialize 
for(i in nrow(term.structure.df):1) {
  term.structure.df$dr[i] <- ifelse(term.structure.df$Expiration.Date[i+2]==2,1,term.structure.df$dr[i + 1]+1)
}

# Calculate CRW(1,t) Front month weight at close of day
term.structure.df$CRW <- (term.structure.df$dr / term.structure.df$dt.total.days.in.roll.period)*1

# Adjust front and 2nd month contacts at Tuesday prior to Expiration
# Shift VIX2 at Tuesday before wednesday expiration
# Pass 1
term.structure.df$C.1.roll.prices.pass.1 <- NULL  # Initialize 
for(i in nrow(term.structure.df):1) {
  term.structure.df$C.1.roll.prices.pass.1[i] <- ifelse(term.structure.df$Expiration.Date[i+1]==2,term.structure.df$C2[i],term.structure.df$C1[i])
}
# Pass 2
term.structure.df$C.1.roll.prices.pass.2 <- NULL  # Initialize 
for(i in nrow(term.structure.df):1) {
  term.structure.df$C.1.roll.prices.pass.2[i] <- ifelse(term.structure.df$Expiration.Date[i]==2,term.structure.df$C2[i],term.structure.df$C.1.roll.prices.pass.1[i])
}
colnames(term.structure.df)[17] = "C1.Rolled"

# Shift VIX3 at Tuesday before wednesday expiration
# Pass 1
term.structure.df$C.2.roll.prices.pass.1 <- NULL  # Initialize 
for(i in nrow(term.structure.df):1) {
  term.structure.df$C.2.roll.prices.pass.1[i] <- ifelse(term.structure.df$Expiration.Date[i+1]==2,term.structure.df$C3[i],term.structure.df$C2[i])
}
# Pass 2
term.structure.df$C.2.roll.prices.pass.2 <- NULL  # Initialize 
for(i in nrow(term.structure.df):1) {
  term.structure.df$C.2.roll.prices.pass.2[i] <- ifelse(term.structure.df$Expiration.Date[i]==2,term.structure.df$C3[i],term.structure.df$C.2.roll.prices.pass.1[i])
}
colnames(term.structure.df)[19] = "C2.Rolled"

# 30 Day Constant Maturity futures price VIX1 | VIX2
# CRW(1,t) * VIX1 + (1-CRW(1,t)) * VIX2
term.structure.df$vix1.2.contstant <- (term.structure.df$CRW * term.structure.df$C1.Rolled) +((1-term.structure.df$CRW)* term.structure.df$C2.Rolled)
# Download spot VIX from CBOE 
VIX <- fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv", skip = 1)
VIX <- as.data.frame(VIX)
VIX$Date <- mdy(VIX$Date)
colnames(VIX)[5] <- "Spot_VIX"
head(VIX)
#VIX <- xts(VIX$vix_close, order.by=as.Date(VIX$Date, format = '%Y/%m/%d'))
#VIX1VIX2 = xts(term.structure.df$vix1.2.contstant, order.by=as.Date(term.structure.df$Date, format="%Y-%m-%d"))
tail(term.structure.df$vix1.2.contstant)
# Merge VIX1|VIX2 by common date
vix12.df <- data.frame(Date=term.structure.df$Date,VIX1VIX2=term.structure.df$vix1.2.contstant)
plot.df <- full_join(vix12.df, VIX, by = c("Date" = "Date"))

# Plot Spot Vix vs VIX1|VIX2 30 day constant maturity 
ggplot() +
  geom_line(data=plot.df ,aes(x=Date,y=VIX1VIX2), colour="red3") +
  geom_line(data=plot.df,aes(x=Date,y=Spot_VIX), colour="#0072B2")+ 
  theme_classic()+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))+
  ggtitle("VIX Spot Vs VIX1|VIX2 30 Day Constant Maturity", subtitle = "") +
  labs(x="Date",y="Settlement")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  annotate("text", label = "Spot VIX", x = as.Date("2005-04-26"), y = 30, color = "#0072B2")+
  annotate("text", label = "VIX1|VIX2", x = as.Date("2011-10-26"), y = 60, color = "red3")
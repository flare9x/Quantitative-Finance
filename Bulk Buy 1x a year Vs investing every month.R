# Bulk Buy 1x a year Vs investing every month
# Andrew Bannerman 1.3.2018

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

# Download SPX data
require(quantmod)
startdate<- "1930-01-01"
SPX <- getSymbols("^GSPC",from=startdate,auto.assign=FALSE)
SPX <-  data.frame(Date=index(SPX), coredata(SPX)) # Change XTS to data frame and retain Date column

# Add day of month column 
# This is a helper column for creating buy/sell rules
months <- SPX %>% dplyr::mutate(month = lubridate::month(Date)) %>% group_by(month) 
days <- SPX %>% dplyr::mutate(day = lubridate::day(Date)) %>% group_by(day) 
years <- SPX %>% dplyr::mutate(year = lubridate::year(Date)) %>% group_by(year) 
df <- data.frame(SPX,month=months$month,day=days$day,year=years$year)

# Subset df by date 
#df <- subset(df, Date >= as.Date("2009-01-01"))
head(df$Date)

# Enter monthly and yearly capital investments
capital.invest.e.month <- 10000/12 # Invest 10,000 a year, split into months
bulk.invest.1.year <- 10000

# Simulate buying every month
output <- df %>%
  dplyr::mutate(RunID = data.table::rleid(month)) %>%
  group_by(RunID) %>%
  mutate(ID.No = row_number()) %>%
  dplyr::mutate(total.shares.months = ifelse(ID.No == 1,first(capital.invest.e.month) / first(GSPC.Adjusted),0)) %>%  # Divide total purchased by cost price for total share
  dplyr::mutate(total.cost.months = ifelse(ID.No == 1,first(total.shares.months) * first(GSPC.Adjusted),0)) %>%  # Divide total purchased by cost price for total share
  ungroup() %>%
  select(-RunID)

df <- data.frame(output)
head(df$Date)
# Simulate buying 1x share start of each month
#output <- df %>%
#dplyr::mutate(RunID = data.table::rleid(month)) %>%
#group_by(RunID) %>%
#  mutate(ID.No = row_number()) %>%
#  dplyr::mutate(first.month.total.cost = ifelse(ID.No == 1,first(GSPC.Adjusted) * 1,0)) %>% # Own 1x share at close price change 1 to 2 for more..
#  dplyr::mutate(total.shares = ifelse(ID.No == 1,first(first.month.total.cost) / first(GSPC.Adjusted),0)) %>%  # Divide total purchased by cost price for total share
#    ungroup() %>%
#  select(-RunID)

# Simulate bulk investing 1x a year
output <- df %>%
  dplyr::mutate(RunID = data.table::rleid(year)) %>%
  group_by(RunID) %>%
  mutate(ID.No = row_number()) %>%
  dplyr::mutate(total.shares.years = ifelse(ID.No == 1,first(bulk.invest.1.year) / first(GSPC.Adjusted),0)) %>%  # Divide total purchased by cost price for total share
  dplyr::mutate(total.cost.years = ifelse(ID.No == 1,first(total.shares.years) * first(GSPC.Adjusted),0)) %>%  # Divide total purchased by cost price for total share
  ungroup() %>%
  select(-RunID)
# output data frame
df <- data.frame(output)

# Calculate average cost per share 
# sum first.month.total cost / sum of total shares bought
month.invest.avg.cost <- sum(df$total.cost.months) / sum(df$total.shares.months)
year.invest.avg.cost <- sum(df$total.cost.years) / sum(df$total.shares.years)
find.first.price <- head(df$GSPC.Adjusted,1)
find.last.price <- tail(df$GSPC.Adjusted,1)

# Subset for month avg cost
# index
df$index <- seq(1:nrow(df))
df.month <- subset(df,total.shares.months >0)
# totals 
df.month$total.shares.months.sum <- cumsum(df.month$total.shares.months)
df.month$total.cost.months.sum <- cumsum(df.month$total.cost.months)
df.month$month.roll.avg.cost <- apply(df.month[,c('total.cost.months.sum','total.shares.months.sum')], 1, function(x) { (x[1]/x[2]) } )
head(df.month$Date)
# Join original df 
df.join.month <- full_join(df, df.month, by = c("Date" = "Date"))
df.join.month$month.roll.avg.cost <- na.locf(df.join.month$month.roll.avg.cost)
head(df.join.month$Date)

# Subset for year avg year cost
df.year <- subset(df,total.shares.years >0)
# totals 
df.year$year.total.shares.years.sum <- cumsum(df.year$total.shares.years)
df.year$year.total.cost.years.sum <- cumsum(df.year$total.cost.years)
df.year$year.roll.avg.cost <- apply(df.year[,c('year.total.cost.years.sum','year.total.shares.years.sum')], 1, function(x) { (x[1]/x[2]) } )

# Join original df 
df.join.year <- full_join(df, df.year, by = c("Date" = "Date"))
df.join.year$year.roll.avg.cost <- na.locf(df.join.year$year.roll.avg.cost)
tail(plot.df,1000)
# Plot 
plot.df  <- data.frame("Date" = df.join.month$Date, "Rolling Average Cost Monthly" = df.join.month$month.roll.avg.cost,"Rolling Average Cost Yeary" = df.join.year$year.roll.avg.cost, "SPX Adjusted Close" = df$GSPC.Adjusted)
# Melt for plotting
plot.df <- melt(plot.df, id.vars="Date")
ggplot(plot.df, aes(x=Date, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+  
  ggtitle("Average Share Cost - Investing Monthly Vs 1x Bulk Investing Each Year",subtitle="SPX 1950 To Present")+
  labs(x="Date",y="SPX Close Price")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))

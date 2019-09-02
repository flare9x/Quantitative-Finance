# Es Up / Down Trend Isolation 
# 2.26.2018 
# Andrew Bannerman 

###############################################################################################
# Steps 
# 1. Define a trend day when the close price was within 25% of the high / low 
# 2. Save all dates 
# 3. Subset 5 minutes bars, save each day into a list 
# 4. Output price plots 
# 5. Study characteristics 
# 6. Add moving averages, TICK etc.. anything else to help cast a +2D view on the trend data 
###############################################################################################

# Packages 
require(lubridate)
require(dplyr)
require(ggplot2)
require(xts)
require(quantmod)

# Load Daily ES Data 
es_d <- read.csv("C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/ES Data/1.day.reg.sess.es.txt",header=TRUE,stringsAsFactors = FALSE)
es_d$Date <- mdy(es_d$Date)  # Change to date format 

# Find relative position of the closing price  (Close - Low) / (High - Low)
es_d$pos <- (es_d$Close - es_d$Low) / (es_d$High - es_d$Low)

# Calculate returns 
es_d$rets <- ((es_d$Close / lag(es_d$Close,1)) - 1) * 100

# Find gap up / down days 
es_d$gap_up <- ifelse(es_d$Open > lag(es_d$High,1),1,0)
es_d$gap_dn <- ifelse(es_d$Open < lag(es_d$Low,1),1,0)

# Find trend up / down days 
es_d$trend_up <- ifelse(es_d$pos >= .75,1,0)
es_d$trend_dn <- ifelse(es_d$pos <= .25,1,0)

# Subset all trend up / down days 
# Trend day definition: close within 25 % of day high / low and close  over .75% to exclude quiet days 
# Exclude gap up / down days where open closes over high , low
trend_up <- es_d[es_d$trend_up == 1 & es_d$rets >= .75 & es_d$gap_up == 0, ]
trend_dn <- es_d[es_d$trend_dn == 1 & es_d$rets <= -.75 & es_d$gap_dn == 0, ]

# Count total trend up / down days 
total_up <- nrow(trend_up)
total_dn <- nrow(trend_dn)

# Save Dates in order to use for susetting 5 min bars 
trend_up_dates <- trend_up$Date
trend_dn_dates <- trend_dn$Date

###############################################################################
# Load  1 minute bars 
###############################################################################
es_1 <- read.csv("C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/ES Data/1.min.reg.sess.es.txt",header=TRUE,stringsAsFactors = FALSE)
es_1$Date_new <- paste(es_1$Date,es_1$Time)
es_1$Date_new  <- mdy_hm(es_1$Date_new)  # Change date to date format 
es_1$Date <- mdy(es_1$Date)
# Save up trend plots
# initialize list 
t_UP <- list()
i=1
for (i in 1:length(trend_up_dates)) { 
  tryCatch({
    ptm0 <- proc.time()
  temp <- subset(es_1 , Date == trend_up_dates[i]) # Conditionl subset == grab trend day on intraday level
  temp$range <- temp$High - temp$Low
  t_UP[[i]] <- temp
  name_date <- as.numeric(gsub("-", "", head(temp$Date,1)))
temp <- temp[3:10]
head(temp)
colnames(temp)[7] <- "Date"
df <- data.frame("Open" = temp$Open, "High" = temp$High, "Low" = temp$Low,"Close" = temp$Close,"Date" = temp$Date)
head(df)
open = xts(df$Open, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
high = xts(df$High, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
low = xts(df$Low, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
close = xts(df$Close, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
all_ts <- cbind(open,high,low,close)
names <- c("Open","High","Low","Close")
colnames(all_ts) <- names

# Save up trend plots
dir <- "C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/Trend Days/trend_up_plots"
mypath <- file.path(dir,paste(name_date[1], ".png", sep = ""))
png(file=mypath,width= 1400, height=1000)
mytitle = paste(name_date[1])
chartSeries(all_ts, type = "bars", ,bar.type = "ohlc",theme = chartTheme("white"),name=paste("es_mini -- Trend Up Day"))
dev.off()
ptm1=proc.time() - ptm0
time=as.numeric(ptm1[3])
cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}


# General Trend Up Stats 
trend_up_out <- do.call(rbind,t_UP)
head(trend_up_out)
t_up_mean_r <- mean(trend_up_out$range)

# Save intraday down trend plots 
# initialize list 
t_DN <- list()
i=1
for (i in 1:length(trend_dn_dates)) { 
  tryCatch({
    ptm0 <- proc.time()
    Sys.sleep(0.1)  
  temp <- subset(es_1 , Date == trend_dn_dates[i])
  temp$range <- temp$High - temp$Low
  t_DN[[i]] <- temp
  name_date <- as.numeric(gsub("-", "", head(temp$Date,1))) #replace - with ""
  temp$chg <- ifelse(temp$Close > temp$Open, "up", "dn")
  temp <- temp[3:10]
  colnames(temp)[7] <- "Date"
  df <- data.frame("Open" = temp$Open, "High" = temp$High, "Low" = temp$Low,"Close" = temp$Close,"Date" = temp$Date)
  open = xts(df$Open, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M"))
  high = xts(df$High, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M"))
  low = xts(df$Low, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M"))
  close = xts(df$Close, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M"))
  all_ts <- cbind(open,high,low,close)
  names <- c("Open","High","Low","Close")
  colnames(all_ts) <- names
  
  # Save down trend plots
  dir <- "C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/Trend Days/trend_down_plots"
  mypath <- file.path(dir,paste(name_date[1], ".png", sep = ""))
  png(file=mypath,width= 1400, height=1000)
  mytitle = paste(name_date[1])
  chartSeries(all_ts, type = "bars", ,bar.type = "ohlc",theme = chartTheme("white"),name=paste("es_mini -- Trend Down Day"))
  dev.off()
  ptm1=proc.time() - ptm0
  time=as.numeric(ptm1[3])
  cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

# General Trend Down Stats 
trend_dn_out <- do.call(rbind,t_DN)
t_dn_mean_r <- mean(trend_dn_out$range)

##############################################################################
# Extract 5 days prior to trend up / down day 
##############################################################################
dates_df <- data.frame(Date = es_d$Date)
dates_df$wkdays <- weekdays(as.Date(dates_df$Date)) # extract day of week

# Grab trend up start dates
es_d_prior_m <- ifelse(dates_df$wkdays == "Monday" & es_d$Date %in% trend_up_dates, paste(es_d$Date - 7),NA)
es_d_prior_tue <- ifelse(dates_df$wkdays == "Tuesday" & es_d$Date %in% trend_up_dates, paste(es_d$Date - 7),NA) 
es_d_prior_w <- ifelse(dates_df$wkdays == "Wednesday" & es_d$Date %in% trend_up_dates, paste(es_d$Date - 7),NA)
es_d_prior_th <- ifelse(dates_df$wkdays == "Thursday" & es_d$Date %in% trend_up_dates, paste(es_d$Date - 7),NA)
es_d_prior_f <- ifelse(dates_df$wkdays == "Friday" & es_d$Date %in% trend_up_dates, paste(es_d$Date - 5),NA)
# Remove NA
es_d_prior_m <- es_d_prior_m[!is.na(es_d_prior_m)]
es_d_prior_tue <- es_d_prior_tue[!is.na(es_d_prior_tue)]
es_d_prior_w <- es_d_prior_w[!is.na(es_d_prior_w)]
es_d_prior_th <- es_d_prior_th[!is.na(es_d_prior_th)]
es_d_prior_f <- es_d_prior_f[!is.na(es_d_prior_f)]
t_up_all_prior <- c(es_d_prior_m,es_d_prior_tue,es_d_prior_w,es_d_prior_th,es_d_prior_f)
# sort dates 
t_up_all_prior <- sort(t_up_all_prior)
t_up_all_prior <- ymd(t_up_all_prior)

# up trend subsetting 
up_list <- list()
for (i in 1:length(trend_up_dates)) { 
  up_list[[i]] <- es_d[es_d$Date >= t_up_all_prior[i] & es_d$Date <= trend_up_dates[i],]
}

# Plot 5 days prior to uptrend 
t_UP_Prior <- list()
i=1
for (i in 1:length(up_list)) { 
  tryCatch({
    ptm0 <- proc.time()
    temp <- up_list[[i]] # Conditionl subset == grab trend day on intraday level
    name_date <- as.numeric(gsub("-", "", head(temp$Date,1)))
    df <- data.frame("Open" = temp$Open, "High" = temp$High, "Low" = temp$Low,"Close" = temp$Close,"Date" = temp$Date)
    open = xts(df$Open, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    high = xts(df$High, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    low = xts(df$Low, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    close = xts(df$Close, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    all_ts <- cbind(open,high,low,close)
    names <- c("Open","High","Low","Close")
    colnames(all_ts) <- names
    
    # Save up trend plots
    dir <- "C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/Trend Days/trend_up_prior"
    mypath <- file.path(dir,paste(name_date[1], ".png", sep = ""))
    png(file=mypath,width= 1400, height=1000)
    mytitle = paste(name_date[1])
    chartSeries(all_ts, type = "bars", ,bar.type = "ohlc",theme = chartTheme("white"),name=paste("es_mini -- Trend Up Day"))
    dev.off()
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

# Grab trend down start dates
es_d_prior_m <- ifelse(dates_df$wkdays == "Monday" & es_d$Date %in% trend_dn_dates, paste(es_d$Date - 7),NA)
es_d_prior_tue <- ifelse(dates_df$wkdays == "Tuesday" & es_d$Date %in% trend_dn_dates, paste(es_d$Date - 7),NA) 
es_d_prior_w <- ifelse(dates_df$wkdays == "Wednesday" & es_d$Date %in% trend_dn_dates, paste(es_d$Date - 7),NA)
es_d_prior_th <- ifelse(dates_df$wkdays == "Thursday" & es_d$Date %in% trend_dn_dates, paste(es_d$Date - 7),NA)
es_d_prior_f <- ifelse(dates_df$wkdays == "Friday" & es_d$Date %in% trend_dn_dates, paste(es_d$Date - 5),NA)
# Remove NA
es_d_prior_m <- es_d_prior_m[!is.na(es_d_prior_m)]
es_d_prior_tue <- es_d_prior_tue[!is.na(es_d_prior_tue)]
es_d_prior_w <- es_d_prior_w[!is.na(es_d_prior_w)]
es_d_prior_th <- es_d_prior_th[!is.na(es_d_prior_th)]
es_d_prior_f <- es_d_prior_f[!is.na(es_d_prior_f)]
t_up_all_prior <- c(es_d_prior_m,es_d_prior_tue,es_d_prior_w,es_d_prior_th,es_d_prior_f)
# sort dates 
t_up_all_prior <- sort(t_up_all_prior)
t_up_all_prior <- ymd(t_up_all_prior)

# down trend subsetting 
dn_list <- list()
for (i in 1:length(trend_dn_dates)) { 
  dn_list[[i]] <- es_d[es_d$Date >= t_up_all_prior[i] & es_d$Date <= trend_dn_dates[i],]
}

# Plot 5 days prior to down trend 
i=1
for (i in 1:length(dn_list)) { 
  tryCatch({
    ptm0 <- proc.time()
    temp <- dn_list[[i]] # Conditionl subset == grab trend day on intraday level
    name_date <- as.numeric(gsub("-", "", head(temp$Date,1)))
    df <- data.frame("Open" = temp$Open, "High" = temp$High, "Low" = temp$Low,"Close" = temp$Close,"Date" = temp$Date)
    open = xts(df$Open, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    high = xts(df$High, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    low = xts(df$Low, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    close = xts(df$Close, order.by=as.POSIXct(df$Date, format="%Y-%m-%d %H:%M:%S"))
    all_ts <- cbind(open,high,low,close)
    names <- c("Open","High","Low","Close")
    colnames(all_ts) <- names
    
    # Save up trend plots
    dir <- "C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/Trend Days/trend_dn_prior"
    mypath <- file.path(dir,paste(name_date[1], ".png", sep = ""))
    png(file=mypath,width= 1400, height=1000)
    mytitle = paste(name_date[1])
    chartSeries(all_ts, type = "bars", ,bar.type = "ohlc",theme = chartTheme("white"),name=paste("es_mini -- Trend Up Day"))
    dev.off()
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat('\n','Iteration',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

str(es_1$Time)
##########################################################################################
# Intraday Trend up / down
##########################################################################################
es_1 <- read.csv("C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/ES/ES Data/1.min.reg.sess.es.txt",header=TRUE,stringsAsFactors = FALSE)
es_1$Date_new <- paste(es_1$Date,es_1$Time)
es_1$Date_new  <- mdy_hm(es_1$Date_new)  # Change date to date format 
es_1$Date <- mdy(es_1$Date)
# make data.table
# data.table for speed
es_1 <- data.table(es_1)
# rolling day high
es_1[, roll_max := cummax(High), by = Date] # rolling day high
es_1[, close_over_high := ifelse(Close > shift(roll_max), 1,0),by = Date] # place 1 when close makes new day high
# rolling day low
es_1[, roll_min := cummin(Low), by = Date] # rolling day high
es_1[, close_below_low := ifelse(Close < shift(roll_min), 1,0),by = Date] # place 1 when close makes new day low
for(j in seq_along(es_1)){
  set(es_1, i = which(is.na(es_1[[j]]) & is.numeric(es_1[[j]])), j = j, value = 0)
}
# count numbert 1 minute highs before 10am central
es_1[, before_10_UCT_up := ifelse(Time >= "08:31" & Time <= "10:00", cumsum(close_over_high),0),by = Date] 
# count numbert 1 minute highs before 10am central
es_1[, before_10_UCT_dn := ifelse(Time >= "08:31" & Time <= "10:00", cumsum(close_below_low),0),by = Date]
for(j in seq_along(es_1)){
  set(es_1, i = which(is.na(es_1[[j]]) & is.numeric(es_1[[j]])), j = j, value = 0)
}
# find maximum new highs / low per day
es_1[, count_max := max(before_10_UCT_up), by = Date] 
es_1[, count_min := max(before_10_UCT_dn), by = Date] 


# Plot distribution of number of new highs / lows prior to 10am 
options(scipen=999)
plot.df <- subset(es_1, count_max > 0 | count_min > 0)
ggplot(data=plot.df, aes(plot.df$count_max)) + 
  geom_histogram(binwidth = .5,fill="darkgreen",alpha = 0.5,col="grey") + 
  scale_x_continuous(breaks = seq(plot.df$count_max), max(plot.df$count_max),name="No. New Highs Prior 10am Central")+
  scale_y_continuous(breaks = seq(0, 600000,100000))+
  ggtitle("Number of new 1 minute highs prior to 10am Central")+
  ylab("Total New Highs")

# Plot new lows 
ggplot(data=plot.df, aes(plot.df$count_min)) + 
  geom_histogram(binwidth = .5,fill="red",alpha = 0.5,col="grey") + 
  scale_x_continuous(breaks = seq(plot.df$count_min), max(plot.df$count_min),name="No. New Lows Prior 10am Central")+
  #scale_y_continuous(breaks = seq(0, 35500,2000))+
  ggtitle("Number of new 1 minute lows prior to 10am Central")+
  ylab("Total New Low")
graphics.off()

# Back test 
# Trading Signal long and short
es_1[, sig_buy := ifelse(before_10_UCT_up >= 8, 1,0)] # long signal 
es_1[, sig_exit :=  Time < "15:15"] # exit time 
es_1[, sig_short := ifelse(before_10_UCT_dn >= 8, -1,0)] # long signal 
es_1[, sig_s_exit :=  Time < "15:15"] # exit time 
#es_1[is.na(es_1)] <- 0
es_1[, sig_end_l := ifelse(sig_exit == FALSE,0,NA)] # mark end of trade retain NA
es_1[, sig_end_s := ifelse(sig_s_exit == FALSE,0,NA)] # mark end of trade retain NA
# Combine NA + signal for long trades
es_1$z_l <- ifelse(es_1$sig_buy == 1, 
               rowSums(es_1[, c("sig_buy", "sig_end_l")], na.rm=TRUE), 
               rowSums(es_1[, c("sig_buy", "sig_end_l")]))
es_1$z_l[1] <- 0
# Combine NA + signal for short trades
es_1$z_s <- ifelse(es_1$sig_short == -1, 
                   rowSums(es_1[, c("sig_short", "sig_end_s")], na.rm=TRUE), 
                   rowSums(es_1[, c("sig_short", "sig_end_s")]))
es_1$z_s[1] <- 0
# Forward fill 1's to end of the trade using package zoo, function na.locf
es_1[, final.signal_long := na.locf(z_l)] # long trades
es_1[, final.signal_short := na.locf(z_s)] # long trades
# lag signal by one forward day to signal entry next day (Trade at market open)
es_1$final.signal_long <- lag(es_1$final.signal_long,1) # Note k=1 implies a move *forward*
es_1$final.signal_short <- lag(es_1$final.signal_short,1) # Note k=1 implies a move *forward*
# Close to close returns 
es_1[, one_min_rets := (Close / shift(Close)) - 1] 
# Calculate strategy returns 
es_1[, sig_long_rets := final.signal_long * one_min_rets] 
es_1[, sig_short_rets := final.signal_short * one_min_rets] 
es_1$sig_long_rets[1] <- 0
es_1$sig_short_rets[1] <- 0
# Combine equity curves 
es_1[, combined_rets := sig_long_rets + sig_short_rets]
# Cumulative returns
es_1[, cum_rets_l := cumprod(1 + sig_long_rets) - 1] 
es_1[, cum_rets_s := cumprod(1 + sig_short_rets) - 1] 
es_1[, cum_rets_comb := cumprod(1 + combined_rets) - 1] 
plot(es_1$Date_new,es_1$cum_rets_l,type="l")
graphics.off()
# Plot equity curves 
line_plot_df <- data.frame(es_1$cum_rets_l,es_1$cum_rets_s,es_1$cum_rets_comb)
line.plot.df <- melt(line_plot_df)
date <- rep(es_1$Date,3)
line.plot.df <- cbind(line.plot.df,date)
head(line.plot.df)
ggplot(data=line.plot.df, aes(x=date, y=value, group = variable, colour = variable)) +
  geom_line()


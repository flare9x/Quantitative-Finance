# Futures Seasonality Study 

# Clear Workspace
closeAllConnections()
rm(list=ls())

###################################################
# Load Required Packages 
##################################################  

require(lubridate)
require(dplyr)
require(magrittr)
require(TTR)
require(zoo)
require(data.table)
require(xts)
require(ggplot2)
require(ggthemes)
library(gridExtra)

#####################################################
# Point to data dir for all ES time frames 
#####################################################
files = list.files(path = "//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/month_data/Cash/seasonal_portfolio/", pattern = ".csv", full.names = TRUE)
names = list.files(path = "//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/month_data/Cash/seasonal_portfolio/", pattern = ".csv")
#####################################################

# Dataframe output list 
results_list <- list()

i=1
for (i in 1:length(files)) {
  tryCatch({
    ptm0 <- proc.time()
    Sys.sleep(0.1) 
    file.name <- names[i]
    #data.next <- paste(data.dir,txt.files,sep="")
    df <- read.csv(files[i],header=TRUE, sep=",", stringsAsFactors=FALSE)
    #df <- read.csv("C:/Futures_Data/Cash/$HO.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)

# Convert Date Column [1] to Date format 
df$Date <- ymd(df$Date)
# Subset Date
df <- subset(df, Date >= as.Date("1968-01-31") ) 

# Compute daily price differences 
# We replicate NA 1 time in order to maintain correct positioning of differences
# Within the data frame
#df$close.diff <- c(rep(NA, 1), diff(df$Close, lag = 1, differences = 1, arithmetic = TRUE, na.pad = TRUE))
df$clret <- ROC(df$Close, type = c("continuous"))
head(df,100)

# Group each daily difference by month
group <- df %>% dplyr::mutate(mymonth = lubridate::month(Date)) %>% group_by(mymonth) 
df <- data.frame(df,group$mymonth)
df <- arrange(df,group.mymonth)

# Duplicate df
for.mean <- data.frame(df)
head(for.mean,100)

# Perform stats
results <- for.mean %>%
  dplyr::group_by(group.mymonth) %>%
  dplyr::summarize(mean=mean(clret, na.rm=TRUE)*100, 
                   min=min(clret, na.rm=TRUE),
                   max=max(clret,na.rm=TRUE), 
                   sd= sd(clret, na.rm=TRUE),
                   sum_pos = sum(clret > 0, na.rm=TRUE),
                   sum_neg = sum(clret < 0, na.rm=TRUE),
                   sum_even = sum(clret == 0, na.rm=TRUE),
                   total = sum(sum_pos + sum_neg + sum_even)) %>%
  mutate(win.percent = sum_pos / total)

# Adjust as ratio compared to total average 
#all.mean <- mean(results$mean)
#results$mean <- results$mean / all.mean
  
results <- data.frame(results,"Contract" = paste(file.name))

## Save df as plot 
#p2 <- tableGrob(results)

# Find counts for 

from <- head(df$Date,1)
to <- tail(df$Date,1)

# Plot 
#mypath <- file.path("C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/Futures/Seasonality/Output_plots/",paste(file.name, ".png", sep = ""))
#png(file=mypath,width= 1400, height=1000)
#p1 <- ggplot(results, aes(group.mymonth, mean)) +
# geom_line()+
# theme_classic()+
# scale_x_continuous(breaks = seq(0, 12, by = 1))+
# ggtitle(paste(file.name," - Monthly Mean % Change"), subtitle = paste("Futures Cash From",from,"To",to)) +
# labs(x="Month",y="Mean % Change")+
# theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))

#p <- gridExtra::grid.arrange(p1, p2,ncol = 1)
#ggsave(filename=paste("C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/Futures/Seasonality/Output_plots/",file.name, ".png"),units=c("in"),width= 15, height=10.67,p)

#ggplot(mean, aes(group.mymonth, mean)) +
#  geom_line()+
# theme_bw() +
# scale_x_continuous(breaks = seq(0, 12, by = 1))+
# scale_y_continuous(breaks = seq(-0.15, 0.30, by = 0.02))+
#  ggtitle("Mean Daily Spead Per Month", subtitle = "1928 To Present") +
#  labs(x="Month",y="Mean Daily Spread Per Month")+
#  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
#  geom_rect(aes(xmin=4.5,xmax=9,ymin=-Inf,ymax=Inf),alpha=0.1,fill="#CC6666")+
#  geom_rect(aes(xmin=1,xmax=4.5,ymin=-Inf,ymax=Inf),alpha=0.1,fill="#66CC99")+
#  geom_rect(aes(xmin=9,xmax=12,ymin=-Inf,ymax=Inf),alpha=0.1,fill="#66CC99")

# Save df to list 
results_list[[i]] <- rbind(results)

# Write output to file
#write.csv(mean,file=paste("C:/R Projects/Final Scripts/2018_new_scripts_saved/Final Scripts/Futures/Seasonality/Output_month_seasonality_csv/","month_seasonality_",file.name))
ptm1=proc.time() - ptm0
time=as.numeric(ptm1[3])
cat('\n','Iteration',i,paste("Continous Futures Contract",file.name,'took', time, "seconds to complete"))
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

# Cbind outputs 
all.df <- do.call(rbind,results_list)

# Sort by month and arrange 
win.perc.sort <- all.df %>% group_by(group.mymonth) %>% 
  arrange(desc(win.percent),desc(group.mymonth))

jan <- all.df[which(all.df$group.mymonth == 1),]
feb <- all.df[which(all.df$group.mymonth == 2),]
mar <- all.df[which(all.df$group.mymonth == 3),]
apr <- all.df[which(all.df$group.mymonth == 4),]
may <- all.df[which(all.df$group.mymonth == 5),]
jun <- all.df[which(all.df$group.mymonth == 6),]
jul <- all.df[which(all.df$group.mymonth == 7),]
aug <- all.df[which(all.df$group.mymonth == 8),]
sep <- all.df[which(all.df$group.mymonth == 9),]
oct <- all.df[which(all.df$group.mymonth == 10),]
nov <- all.df[which(all.df$group.mymonth == 11),]
dec <- all.df[which(all.df$group.mymonth == 12),]

head(arrange(may,desc(win.percent)),20)

top.jan <- head(arrange(jan,desc(win.percent)),1)
top.feb <- head(arrange(feb,desc(win.percent)),1)
top.mar <- head(arrange(mar,desc(win.percent)),1)
top.apr <- head(arrange(apr,desc(win.percent)),1)
top.may <- head(arrange(may,desc(win.percent)),1)
top.jun <- head(arrange(jun,desc(win.percent)),1)
top.jul <- head(arrange(jul,desc(win.percent)),1)
top.aug <- head(arrange(aug,desc(win.percent)),1)
top.sep <- head(arrange(sep,desc(win.percent)),1)
top.oct <- head(arrange(oct,desc(win.percent)),1)
top.sep <- head(arrange(sep,desc(win.percent)),1)
top.nov <- head(arrange(nov,desc(win.percent)),1)
top.dec <- head(arrange(dec,desc(win.percent)),1)

out <- rbind(top.jan,top.feb,top.mar,top.apr,top.may,top.jun,top.jul,top.aug,top.sep,top.oct,top.nov,top.dec)
out$names <- ""
out$names[1] <- "CME Lean Hogs Index"
out$names[2] <- "13-Week T. Bill Yield"
out$names[3] <- "RBOB Gasoline - New York"
out$names[4] <- "RBOB Gasoline - New York"
out$names[5] <- "CME Lean Hogs Index"
out$names[6] <- "CME Lean Hogs Index"
out$names[7] <- "13-Week T. Bill Yield"
out$names[8] <- "Heating Oil"
out$names[9] <- "Natural Gas - Henry Hub"
out$names[10] <- "Natural Gas - Henry Hub"
out$names[11] <- "Iron Ore"
out$names[12] <- "Iron Ore"

#Highest win %
out$names[1] <- "CME Lean Hogs Index"
out$names[2] <- "13-Week T. Bill Yield"
out$names[3] <- "TecDAX Index"
out$names[4] <- "CME Lean Hogs Index"
out$names[5] <- "CME Feeder Cattle Index"
out$names[6] <- "CME Feeder Cattle Index"
out$names[7] <- "CME Feeder Cattle Index"
out$names[8] <- "Iron Ore"
out$names[9] <- "TecDAX Index"
out$names[10] <- "CAC 40  Index"
out$names[11] <- "Soybeans #1 Yellow"
out$names[12] <- "Iron Ore"

# Highest Probability per month 
p3 <- tableGrob(out)
gridExtra::grid.arrange(p3,ncol = 1, top="Highest Win %")

# Plot Close Price and Close Price Distributions for each commoditiy 

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
files = list.files(path = "//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_futures/Cash/", pattern = ".csv", full.names = TRUE)
names = list.files(path = "//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_futures/Cash/", pattern = ".csv")
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
    #df <- subset(df, Date >= as.Date("1968-01-31") ) 
    
    # Compute daily price differences 
    # We replicate NA 1 time in order to maintain correct positioning of differences
    # Within the data frame
    #df$close.diff <- c(rep(NA, 1), diff(df$Close, lag = 1, differences = 1, arithmetic = TRUE, na.pad = TRUE))
    df$clret <- ROC(df$Close, type = c("continuous"))
    
    # To and from
    from <- head(df$Date,1)
    to <- tail(df$Date,1)
    
    ###############################################################
    ### Price and price distribution histogram 
    ###############################################################
    z1 <- ggplot(df,aes(Close))+
      geom_histogram(binwidth=10)+
      scale_x_continuous(breaks = seq(round(min(df$Close)),round(max(df$Close)),50))+
      ggtitle(paste(file.name," - Distribution Daily Close Prices"),subtitle = paste("From",from,"To",to))+
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
    
    z2 <- ggplot(df,aes(x=Date,y=Close))+
      geom_line() +
      ggtitle(paste(file.name," - Daily Close Prices"),subtitle = paste("From",from,"To",to))
    
    p <- gridExtra::grid.arrange(z2,z1,ncol=1)
    ggsave(filename=paste("//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_close_distributions/",file.name, ".png"),units=c("in"),width= 15, height=10.67,p)
    
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    cat(paste('\n','Iteration',i,"Distribution Of Cash Close Prices",file.name,"took",time,"seconds to complete"))
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}
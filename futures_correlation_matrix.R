# Commotities Correlation Matrix 
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

# Initialise data frame with date column 
org.df <- read.csv("//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_futures/Cash/$S1Y.csv", header=TRUE,stringsAsFactors = FALSE)
org.df <- data.frame(org.df$Date)
colnames(org.df)[1] <- "Date"
org.df$Date <- ymd(org.df$Date)
i=1

for (i in 1:length(files)) {
  tryCatch({
    ptm0 <- proc.time()
    Sys.sleep(0.1) 
    #data.next <- paste(data.dir,txt.files,sep="")
    data <- read.csv(files[i],header=TRUE, sep=",", stringsAsFactors=FALSE)
    data$Date <- ymd(data$Date)
    
    # rets 
    data$Close <- log(data$Close)
    data$rets <- ROC(data$Close, type = c("continuous"))
    data$rets[1] <-0
    
    data <- data.frame(Date=data$Date,rets = data$rets)
    colnames(data)[2] <- paste0("clrets",names[i])
    # cbind all data to original df
    out.df <- left_join(org.df, data, by = "Date")
    org.df <- out.df
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    head(data)
    cat('\n','Iteration left_join',i,names[i],'log rets',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}

org.df[is.na(org.df)] <- 0

# correlation matrix all 
cor.df <- as.matrix(org.df[,2:length(org.df)])
names(cor.df)
cor.matrix <- cor(cor.df, method = c("pearson"),use = "complete.obs")
#png(height=1200, width=1500, pointsize=0.1, file="overlap.png")
#corrplot(cor.matrix, method = "number",tl.cex = .5,pch.cex = .9)
#dev.off()
#graphics.off()

#col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
#heatmap(x = cor.matrix, col = col, symm = TRUE)
library(corrplot)
library(ggcorrplot)
png(file="corr.png", res=1800, width=3000, height=3000)
ggcorrplot(cor.matrix,tl.cex = 10,lab=TRUE,title = "Correlation Matrix - Log Returns - Futures Cash") + theme(plot.title = element_text(size=22))

# Seasonal Portfolio Backtest 

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
files = list.files(path = "//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_futures/Cash/seasonal_portfolio/", pattern = ".csv", full.names = TRUE)
names = list.files(path = "//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_futures/Cash/seasonal_portfolio/", pattern = ".csv")
#####################################################

# Initialise data frame with date column 
org.df <- read.csv("//HOUIC-NA-V508/Andrew.Bannerman$/cached/My Documents/study/daily_futures/Cash/seasonal_portfolio/$S1Y.csv", header=TRUE,stringsAsFactors = FALSE)
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
    data$rets <- ROC(data$Close, type = c("continuous"))
    data$rets[1] <-0
    data$range <- data$High - data$Low
    data$atr <- SMA(data$range,12)
    
    data <- data.frame(Date=data$Date,rets = data$rets,atr = data$atr)
    colnames(data)[2] <- paste0("clrets",names[i])
    colnames(data)[3] <- paste0("atr",names[i])
    # cbind all data to original df
    out.df <- left_join(org.df, data, by = "Date")
    org.df <- out.df
    ptm1=proc.time() - ptm0
    time=as.numeric(ptm1[3])
    head(data)
    cat('\n','Iteration left_join',names[i],'log rets',i,'took', time, "seconds to complete")
  }, error = function(e) { print(paste("i =", i, "failed:")) })
}


# Extract months / years
group <- org.df %>% dplyr::mutate(year = as.numeric(format(Date, "%Y")),month = as.numeric(format(Date, "%m"))) %>% 
  group_by(month) 
df <- data.frame(org.df,"month"=group$month,"year"=group$year)
df[is.na(df)] <- 0

# Subset by Date
#df <- subset(df, Date >= as.POSIXct("1/31/1994") )  

# create signals
#LH
df$jan.long.LH <- ifelse(df$month == 1,1,0)
df$feb.long.LH <- ifelse(df$month == 2,1,0)
df$mar.long.LH <- ifelse(df$month == 3,1,0)
df$apr.long.LH <- ifelse(df$month == 4,1,0)
df$may.long.LH <- ifelse(df$month == 5,1,0)
df$jun.long.LH <- ifelse(df$month == 6,1,0)

# Rbob  jan to apr
df$jan.long.RBOB <- ifelse(df$month == 1,1,0)
df$feb.long.RBOB <- ifelse(df$month == 2,1,0)
df$mar.long.RBOB <- ifelse(df$month == 3,1,0)
df$apr.long.RBOB <- ifelse(df$month == 4,1,0)

# Soybeans
df$oct.long.S1Y <- ifelse(df$month == 10,1,0)
df$nov.long.S1Y <- ifelse(df$month == 11,1,0)
df$dec.long.S1Y <- ifelse(df$month == 12,1,0)

# WTI long march and April 
df$mar.long.WTI <- ifelse(df$month == 3,1,0)
df$apr.long.WTI <- ifelse(df$month == 4,1,0)

# SP500 December 
df$dec.long.SP <- ifelse(df$month == 12,1,0)

# Calculate equity curves
# LH
df$equity.jan.LH <- df$jan.long.LH  * df$clrets.LH.csv
df$equity.feb.LH <- df$feb.long.LH  * df$clrets.LH.csv
df$equity.mar.LH <- df$mar.long.LH  * df$clrets.LH.csv
df$equity.apr.LH <- df$apr.long.LH * df$clrets.LH.csv
df$equity.may.LH <- df$may.long.LH * df$clrets.LH.csv
df$equity.jun.LH <- df$jun.long.LH * df$clrets.LH.csv

# RBOB
df$equity.jan.RBOB <- df$jan.long.RBOB  * df$clrets.RBOB.csv
df$equity.feb.RBOB <- df$feb.long.RBOB  * df$clrets.RBOB.csv
df$equity.mar.RBOB <- df$mar.long.RBOB * df$clrets.RBOB.csv
df$equity.apr.RBOB <- df$apr.long.RBOB * df$clrets.RBOB.csv

# Soybeans oct to dec
df$equity.oct.S1Y <- df$oct.long.S1Y  * df$clrets.S1Y.csv
df$equity.nov.S1Y <- df$nov.long.S1Y * df$clrets.S1Y.csv
df$equity.dec.S1Y <- df$dec.long.S1Y  * df$clrets.S1Y.csv

# Crude WTI march and april 
df$equity.mar.WTI <- df$mar.long.WTI  * df$clrets.WTI.csv
df$equity.apr.WTI <- df$apr.long.WTI * df$clrets.WTI.csv

# SP Dec 
df$equity.dec.SP<- df$dec.long.SP * df$clrets.SP.csv

df$combine <- df$equity.jan.LH + df$equity.feb.LH + df$equity.mar.LH + df$equity.apr.LH + df$equity.may.LH + df$equity.jun.LH + df$equity.jan.RBOB + df$equity.feb.RBOB + df$equity.mar.RBOB + df$equity.apr.RBOB + df$equity.oct.S1Y + df$equity.nov.S1Y + df$equity.dec.S1Y
df$combine1 <-  df$equity.jan.RBOB + df$equity.feb.RBOB + df$equity.mar.RBOB + df$equity.apr.RBOB + df$equity.oct.S1Y + df$equity.nov.S1Y + df$equity.dec.S1Y
df$combine3 <- df$equity.jan.LH + df$equity.feb.LH + df$equity.mar.LH + df$equity.apr.LH + df$equity.may.LH + df$equity.jun.LH + df$equity.jan.RBOB + df$equity.feb.RBOB + df$equity.mar.RBOB + df$equity.apr.RBOB + df$equity.oct.S1Y + df$equity.nov.S1Y + df$equity.dec.S1Y + df$equity.mar.WTI + df$equity.apr.WTI 
df$combine4 <- df$equity.jan.LH + df$equity.feb.LH + df$equity.mar.LH + df$equity.apr.LH + df$equity.may.LH + df$equity.jun.LH + df$equity.jan.RBOB + df$equity.feb.RBOB + df$equity.mar.RBOB + df$equity.apr.RBOB + df$equity.oct.S1Y + df$equity.nov.S1Y + df$equity.dec.S1Y + df$equity.mar.WTI + df$equity.apr.WTI + df$equity.dec.SP 
df$combine <- df$equity.jan.RBOB + df$equity.feb.RBOB + df$equity.mar.RBOB + df$equity.apr.RBOB + df$equity.oct.S1Y + df$equity.nov.S1Y + df$equity.dec.S1Y + df$equity.mar.WTI + df$equity.apr.WTI + df$equity.dec.SP 


#df[df == 0] <- NA
# Pull select columns from data frame to make XTS whilst retaining formats 
xts1 = xts(df$combine, order.by=as.POSIXct(df$Date, format="%Y-%m-%d")) 
xts2 = xts(df$combine1, order.by=as.POSIXct(df$Date, format="%Y-%m-%d")) 
xts3 = xts(df$combine3, order.by=as.POSIXct(df$Date, format="%Y-%m-%d")) 
xts4 = xts(df$combine4, order.by=as.POSIXct(df$Date, format="%Y-%m-%d")) 

# Join XTS together 
compare <- cbind(xts1,xts3,xts4)

# Use the PerformanceAnalytics package for trade statistics
colnames(compare) <- c("No Hogs :( Seasonal Portfolio","Seasonal with WTI","Adding SP")
charts.PerformanceSummary(compare,main="Seasonal Portfolio", wealth.index=TRUE, colorset=rainbow12equal)
performance.table <- rbind(table.AnnualizedReturns(compare), "Maximum Drawdown" = maxDrawdown(compare), CalmarRatio(compare),table.DownsideRisk(compare))
drawdown.table <- rbind(table.Drawdowns(compare))
#dev.off()
logRets <- log(cumprod(1+compare))
chart.TimeSeries(logRets, legend.loc='topleft', colorset=rainbow12equal,main="With and Without Lean Hogs")


# Prepare log rets plot 
p1.plot <- data.frame(logRets)
p1.plot <- setDT(p1.plot, keep.rownames = TRUE)
p1.plot$rn <- ymd_hms(p1.plot$rn)
head(p1.plot)
p1 <- ggplot()+
  geom_line(data=p1.plot,aes(x=rn,y=Seasonal.with.WTI))+  
  ggtitle("Cumulative Returns - Seasonal Portfolio",subtitle="LH Jan to Jun, RBOB Jan to Apr, Soybeans  Oct to Dec, WTI Mar to Apr, SP500 Dec")+
  labs(x="Date",y="Cumulative Log Returns")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(0,11,1))

#p2.plot <- setDT(performance.table, keep.rownames = TRUE)
p2 <- tableGrob(performance.table)
gridExtra::grid.arrange(p1,p2,ncol = 1, top="Seasonal Portfolio -  LH, RBOB, Soybeans",heights=c(10,10))
# Subset by year and study yearly results 
save <- df %>% group_by(year) %>%
  dplyr::mutate(cumret = cumprod(1 + combine4) -1) %>%
  dplyr::mutate(final = tail(cumret,1)) %>%
  dplyr::mutate(final.rets = first(final))

# extract cum rets per year
by.year.rets <- save %>% group_by(year) %>%
  arrange(final) %>%
  filter(row_number()==1)
by.year.rets <- data.frame(by.year.rets)
str(by.year.rets)

p3 <- ggplot(data=by.year.rets, aes(year, final,fill=final))+
  geom_bar(stat="identity")+
  ggtitle("Yearly Cumulative Returns",subtitle="Seasonal Portfolio")+
  labs(x="Year",y="Cumulative Returns")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(min(by.year.rets$year),max(by.year.rets$year)))+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

###########################################################################
# Calculate correlation of returns between commodities 
##########################################################################
# Over lap = RBOB and LH and WTI

LH_rets <- c(df$clrets.LH.csv)
RBOB_rets <- c(df$clrets.RBOB.csv)

lh_rb.df <- runCor(LH_rets,RBOB_rets, n=100,use = "all.obs", sample = TRUE, cumulative = FALSE)
lh_rb.df <- data.frame("Date" = df$Date,"lh_rbob_cor"= lh_rb.df,stringsAsFactors = FALSE)
head(lh_rb.df)
p4 <- ggplot()+
  geom_line(data=lh_rb.df,aes(x=Date,y=lh_rbob_cor))+  
  ggtitle("LH - RBOB Rolling Correlation of Returns",subtitle="")+
  labs(x="Date",y="Rolling 100 Day Correlation")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(-1,1,.05))
  


############## Subset only march and april 
# WTI, LH, RBOB Correlations
df.wti <- subset(df, df$month == 3 | df$month == 4)
# rolling correlation of WTI to LH and RBOB 
df.wti$wti_to_rbob_cor <- runCor(df.wti$clrets.WTI.csv,df.wti$clrets.RBOB.csv, n=100,use = "all.obs", sample = TRUE, cumulative = FALSE)
df.wti$wti_to_LH_cor <- runCor(df.wti$clrets.WTI.csv,df.wti$clrets.LH.csv, n=100,use = "all.obs", sample = TRUE, cumulative = FALSE)

p5 <- ggplot()+
  geom_line(data=df.wti,aes(x=Date,y=wti_to_rbob_cor))+  
  ggtitle("WTI - RBOB Rolling Correlation of Returns",subtitle="")+
  labs(x="Date",y="Rolling 100 Day Correlation")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(-1,1,.05))

p6 <- ggplot()+
  geom_line(data=df.wti,aes(x=Date,y=wti_to_LH_cor))+  
  ggtitle("WTI - LH Rolling Correlation of Returns",subtitle="")+
  labs(x="Date",y="Rolling 100 Day Correlation")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(-1,1,.05))

############## Subset only Dec
# SP Correlation to Soybeans
df.sp <- subset(df, df$month == 12)
names(df)
# rolling correlation of WTI to LH and RBOB 
df.sp$sp_to_soy_cor <- runCor(df.sp$clrets.SP.csv,df.sp$clrets.S1Y.csv, n=100,use = "all.obs", sample = TRUE, cumulative = FALSE)
 p7 <- ggplot()+
  geom_line(data=df.sp,aes(x=Date,y=sp_to_soy_cor))+  
  ggtitle("SP - Soybeans Rolling Correlation of Returns",subtitle="")+
  labs(x="Date",y="Rolling 100 Day Correlation")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(-1,1,.05))

# Group per commoditiy and perofrm mean of rolling ATR
results.atr.CA.csv <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.CA.csv , na.rm=TRUE)*100, 
                   min=min(atr.CA.csv , na.rm=TRUE),
                   max=max(atr.CA.csv ,na.rm=TRUE), 
                   sd= sd(atr.CA.csv , na.rm=TRUE)) %>%
  dplyr::mutate(id = paste("atr.CA"))

results.atr.FC.csv <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.FC.csv, na.rm=TRUE)*100, 
                   min=min(atr.FC.csv, na.rm=TRUE),
                   max=max(atr.FC.csv,na.rm=TRUE), 
                   sd= sd(atr.FC.csv, na.rm=TRUE))%>%
  dplyr::mutate(id = paste("atr.FC"))

results.atr.FE.csv <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.FE.csv, na.rm=TRUE)*100, 
                   min=min(atr.FE.csv, na.rm=TRUE),
                   max=max(atr.FE.csv,na.rm=TRUE), 
                   sd= sd(atr.FE.csv, na.rm=TRUE)) %>%
  dplyr::mutate(id = paste("atr.FE"))

results.atr.IRX.csv<- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.IRX.csv, na.rm=TRUE)*100, 
                   min=min(atr.IRX.csv, na.rm=TRUE),
                   max=max(atr.IRX.csv,na.rm=TRUE), 
                   sd= sd(atr.IRX.csv, na.rm=TRUE))%>%
  dplyr::mutate(id = paste("atr.IRX"))

results.atr.LH.csv <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.LH.csv, na.rm=TRUE)*100, 
                   min=min(atr.LH.csv, na.rm=TRUE),
                   max=max(atr.LH.csv,na.rm=TRUE), 
                   sd= sd(atr.LH.csv, na.rm=TRUE))%>%
  dplyr::mutate(id = paste("atr.LH"))

results.atr.S1Y.csv <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.S1Y.csv, na.rm=TRUE)*100, 
                   min=min(atr.S1Y.csv, na.rm=TRUE),
                   max=max(atr.S1Y.csv,na.rm=TRUE), 
                   sd= sd(atr.S1Y.csv, na.rm=TRUE))%>%
  dplyr::mutate(id = paste("atr.S1Y"))

results.atr.TDX.csv <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(atr.TDX.csv, na.rm=TRUE)*100, 
                   min=min(atr.TDX.csv, na.rm=TRUE),
                   max=max(atr.TDX.csv,na.rm=TRUE), 
                   sd= sd(atr.TDX.csv, na.rm=TRUE))%>%
  dplyr::mutate(id = paste("atr.TDX"))

# rbind to plots
plot.df <- rbind(results.atr.TDX.csv,results.atr.S1Y.csv,results.atr.LH.csv,results.atr.IRX.csv,results.atr.FE.csv,results.atr.FC.csv,results.atr.FC.csv,results.atr.CA.csv)
plot.df <- data.frame(plot.df)

# results mean of each
results.plot <- plot.df %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(mean=mean(mean, na.rm=TRUE))


p7 <- ggplot(data=results.plot, aes(year, mean,colour = year))+
  geom_bar(stat="identity")+
  ggtitle("Yearly Volatility",subtitle="Rolling 12 month atr of each commodity")+
  labs(x="Year",y="Mean atr")+
  theme(plot.title = element_text(hjust=0.5),plot.subtitle =element_text(hjust=0.5))+
  theme(legend.position = "none")

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7, ncol = 4, top="Seasonal Portfolio - LH, RBOB, Soybeans, Crude Oil, SP500",heights=c(10,10))


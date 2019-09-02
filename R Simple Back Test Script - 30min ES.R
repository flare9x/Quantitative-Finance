# Back test simple moving average strategy 
# Use 30min ES Data 

require(TTR)
require(quantmod)
require(lubridate)
require(dplyr)
require(microbenchmark)
i=1
out_results <- data.frame()
for (i in 1:100) {
start_time <- Sys.time()
  # Read Data 
df <- read.csv("C:/Users/Andrew.Bannerman/Desktop/Julia/30.min.es.txt", header=TRUE,stringsAsFactors = FALSE)
# Make date time column
df$Date_Time <- paste(df$Date,df$Time)
df$Date_Time <- mdy_hm(df$Date_Time)
# Create Sma
df$sma_200 <- SMA(df$Close,200)
# Create long signal
df$Long_Signal <- ifelse(df$Close > df$sma_200,1,0)
df$Long_Signal <- dplyr::lag(df$Long_Signal,1) # lag forward avoid look ahead bias
# For loop for returns 
# Slow loops are slow in R was demonstration purposes only!
#out <- vector()
#for (j in 2:nrow(df)){
#out[j] = df$Close[j]/df$Close[j-2+1] - 1.0
#}
#df <- cbind(df,out)
#colnames(df)[12] = "Close_Ret"
# Use data.table to calculate returns (WAY faster :) ) 
require(data.table)
df = data.table(df)
df[, Close_Ret := (Close / shift(Close))-1]
# Subset Data To start after SMA creation 
df = df[201:nrow(df),]
# Calculate startegy Returns
df$Sig_Rets <- df$Long_Signal * df$Close_Ret
df[is.na(df)] <- 0
# Calculate Cumulative Returns 
# Buy and hold and Strategy returns
df$Signal_cum_ret <- cumprod(1+df$Sig_Rets)-1
df$BH_cum_ret <- cumprod(1+df$Close_Ret)-1

end_time <- Sys.time()

total_time <- end_time - start_time
temp <- total_time
out_results <- rbind(out_results, temp)
cat('\n','Iteration',i,'completed')

}

colnames(out_results)[1] <- "Time"
head(out_results$Time,10)
sum(out_results$Time) / 195.793

# Plot Results
plot(df$BH_cum_ret,type="l")
plot(df$Signal_cum_ret,type="l",main="R 200SMA Back Test Result")

write.csv(df,"C:/Users/Andrew.Bannerman/Desktop/Julia/R_OUT.csv")
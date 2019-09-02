# Obtain List of ETFS From nasdaq.com
# Andrew Bannerman 10.4.2017

library(data.table)

# Read ETF list csv file from nasdaq.com
# Use fread() from data.table package 
# install.packages("data.table")
read.data <- fread("http://www.nasdaq.com/investing/etfs/etf-finder-results.aspx?download=Yes")

# Subset Column 1, Symbol Column
symbol.col <- read.data$Symbol

# Export symbol column as .txt file 
write.table(symbol.col,"D:/R Projects/etf list.us.txt",append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

# Count ETF tickers 
NROW(symbol.col)
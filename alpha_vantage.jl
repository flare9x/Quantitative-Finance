# Download Data Using Alphavantage
using HTTP
using StatPlots

res = HTTP.get("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=AAPL&interval=1min&apikey=your_api_key&datatype=csv")
mycsv = readcsv(res.body)
x = convert(DataFrame, mycsv)
x = x[2:nrow(x),:]  # subset remove header row
# Rename Columns
colnames = ["Date","Open","High","Low","Close","Volume"]
names!(x.colindex, map(parse, colnames))
# Convert String Date to Date format
x[:Date] = DateTime.(x[:Date],Dates.DateFormat("yyy-mm-dd H:M:S"))
# Sort Dataframe by Date Column
x = sort!(x, cols = [order(:Date)], rev=(false))
# Convert OHLC to Float64 and Volume to Int64
for i in 2:length(x)-1
    x[i] = convert(Array{Float64,1},x[i])
end
    x[6] = convert(Array{Int64,1},x[6])

# Plot Data
gr(size=(1500 ,1000))
@df x plot(:Date, [:Close],title = "AAPL Intraday", xlab = "Date", ylab = "Close",colour = [:red],legend = :topleft)
savefig("AAPL_1min.png")

# Multiple symbols
#start_time = Dates.now(Dates.UTC)
t=1
tickers = ["DDM","MVV","QLD","SAA","SSO","TQQQ","UDOW","UMDD","UPRO","URTY","UWM", "BIB", "FINU","LTL","ROM", 
"RXL", "SVXY","UBIO","UCC","UGE","UPW","URE","USD","UXI","UYG","UYM","DOG","DXD","MYY","MZZ","PSQ","QID","RWM","SBB",
"SDD","SDOW","SDS","SH","SPXU","SQQQ","SRTY","TWM","SMDD","UVXY","VIXM","VIXY"]
for t in 1:length(tickers)
# Using HTTP package
#sleep(5)
res = HTTP.get(joinpath(
"https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol="tickers[t]"&outputsize=full&apikey=your_api_key&datatype=csv"))
mycsv = readcsv(res.body)
x = convert(DataFrame, mycsv)
x = x[2:nrow(x),:]  # subset remove header row
# Rename Columns
colnames = ["Date","Open","High","Low","Close","Volume"]
names!(x.colindex, map(parse, colnames))
# Convert String Date to Date format
x[:Date] = Date.(x[:Date],Dates.DateFormat("yyy-mm-dd"))
# Sort Date Frame By Date
x = sort!(x, cols = [order(:Date)], rev=(false))
# Convert OHLC to Float64 and Volume to Int64
i=1
for i in 2:length(x)-1
    x[i] = convert(Array{Float64,1},x[i])
end
    x[6] = convert(Array{Int64,1},x[6])

writetable(joinpath("CSV_OUT_"tickers[t]".csv"), x)
end
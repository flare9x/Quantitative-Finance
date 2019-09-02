    using DataFrames
using Indicators
# Arrays  - see https://lectures.quantecon.org/jl/julia_arrays.html
# Initialize out_result to store time for each iteration
x_res = collect(1:1:100)
x_res = convert(Vector{Base.Dates.Millisecond}, x_res)
out_result = zeros(x_res) # Pre allocate output
#for j in 1:100
start_time = Dates.now(Dates.UTC)
# Load csv
df = readtable("30.min.es.txt", header=true)
#dt = CSV.read("SPY.csv", types=[String; fill(Float64, 5); Int])

# Make Date Time Column
a = df[:Date]
b = df[:Time]
c = map(join,zip(a,b), " ")
out = String[]
temp = String[]
for i in 1:length(a)
    temp =  map(join,zip([a[i]],[b[i]]), " ")
    append!(out,temp)
end

df[:Date_Time] = out
# Convert Date string to Date format
# note the Date.() the . vectorizes the operation
df[:Date_Time] = DateTime.(df[:Date_Time],Dates.DateFormat("mm/dd/yyyy H:M"))
# loop to iterate through each element in vector, converting to Date format
#x = df[:Date] # initialize as Date
#v = Date[]
#for i in 1:length(x)
#    z = Date(x[i], Dates.DateFormat("yyyy-mm-dd"))
#    push!(v,z)  # appends
#end

# cbind() R equivalent hcat() to existing data frame
#df = hcat(df,v)
#    head(df)

#    df[:newdate] = v
#showcols(df)


# Create simple moving average
# using Indicators
Close = convert(Array, df[:Close])
sma_200 = sma(Close,n=200)

#df[:Close_200sma] = sma(df[:Close],n=200)
df[:Close_200sma] = sma_200
# Create Signals
# Stay long over 200sma
# Exit positions below 200sma
# use ifelse() function see - https://en.wikibooks.org/wiki/Introducing_Julia/Controlling_the_flow
# remember . in front of the .> for vectorization!
df[:Signal_Long] = ifelse(df[:Close] .> df[:Close_200sma],1,0)

# Lag data +1 forward
# Avoid look ahead bias
#allowmissing!(df, :Signal_Long)
df[:Signal_Long] = [0; df[1:end-1,:Signal_Long]]

# Calculate Close to Close Returns
Close = df[:Close]
x = convert(Array, Close)
out = zeros(x)
for i in 2:size(Close,1)
    out[i] = Close[i]/Close[i-2+1] - 1.0
end

df[:Close_Rets] = out

# Convert missing to 0
#df[ isna(df[:x1]), :x1] = 0
#df[ isna(df[:Signal_Long]), :Signal_Long] = 0
#df[ismissing.(df[:Close_Rets]), :Close_Rets] = 0
#df[ismissing.(df[:Signal_Long]), :Signal_Long] = 0

# Calculate signal returns
df[:Signal_Rets] = df[:Signal_Long] .* df[:Close_Rets]

# Calculate Cumulative Returns
df = df[201:end,:]
df[:Cum_Rets] = cumprod(1+df[1:end, :Signal_Rets])-1
df[:BH_Cum_Rets] = cumprod(1+df[1:end, :Close_Rets])-1

end_time = Dates.now(Dates.UTC)

#print(end_time - start_time)
time_total = end_time - start_time
#out_result[j] = time_total
#end

# Final Time 
# Convert from millisencs to Int64
x_f = collect(1:1:100)
final_out = zero(x_f)
for f in 1:length(out_result)
temp = out_result[f]
temp = convert(Int64,temp)
final_out[f] = temp
end
print(sum(final_out))
# Plot
using StatPlots
gr(size=(1500 ,1000))
@df df plot(:Date_Time, [:Cum_Rets :BH_Cum_Rets], title = "SPY Long Over 200sma", xlab = "Date", ylab = "Cumulative Returns",colour = [:lightgreen :pink],legend = :topleft)
savefig("myplot.png")
@df df plot(:Date_Time, [:Close :Close_200sma], title = "SPY Long Over 200sma", xlab = "Date", ylab = "Cumulative Returns",colour = [:lightgreen :pink],legend = :topleft)


# Export .csv
writetable("JULIA_out.csv", df)
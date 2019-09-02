####################################################################################
# Rolling Hurst
####################################################################################

# initialize outputs
m = Array{Float64}(length(d_es_close),0)
log_n_out = m
out_RS = m

# Set lags (range or specific)
max_lag = 100
n = 200
lags = n:max_lag
# or specific lags #comment out where necessary
lags = [8,16,32,64,128,256,512,1024]
# Specify return series lag
n_lag = 2000

i=1
j=1
c=1
    for j = lags
        n=lags[c]
    # Calculate returns of the series
    #n_lag = lags[c] # set n_lag 1 for 1 day returns
    rets = zeros(d_es_close)
    n_lag = n_lag
    for i in n_lag+1:length(d_es_close)
        rets[i] = ((d_es_close[i] / d_es_close[i-n_lag])-1) # rets[i] = ((d_es_close[i] / d_es_close[i-n_lag+1])-1)
    end
    #rets = d_es_close
    # Find mean width of lookback
    mean_out = zeros(rets)
    for i = n:size(rets,1)
                mean_out[i] = mean(rets[i-n+1:i])
            end
    # Subtract deviations from the mean
    dev_mean_out = zeros(mean_out)
    for i = n:size(mean_out,1)
                dev_mean_out[i] = (rets[i] - mean_out[i])
            end
    # Roll sum the deviations from the mean
    sum_out = zeros(dev_mean_out)
    for i = n:size(dev_mean_out,1)
            sum_out[i] = sum(dev_mean_out[i-n+1:i])
        end
    # Find the maximum and minimum of sum of the mean deviations
    max_out = zeros(sum_out)
    for i = n:size(sum_out,1)
                max_out[i] = maximum(sum_out[i-n+1:i])
            end
    min_out = zeros(sum_out)
    for i = n:size(sum_out,1)
                min_out[i] = minimum(sum_out[i-n+1:i])
            end
    # R = Range, max - min
    R_out = zeros(dev_mean_out)
    for i= n:size(dev_mean_out,1)
        R_out[i] = max_out[i] - min_out[i]
    end
    # Rolling standard deviation of (returns) the mean
    stdev_out = zeros(rets)
    for i = n:size(rets,1)
            stdev_out[i] = sqrt(var(dev_mean_out[i-n+1:i]))
        end
    # Calculate rescaled range (Range (R_out) / stdev of returns )
    RS_out = zeros(R_out)
    for i = n:size(R_out,1)
            RS_out[i] = R_out[i] / stdev_out[i]
        end
    # Calculate log_n (n)
    index = fill(n,length(rets))
    log_n = zeros(rets)
    for i =n:size(index,1)
        log_n[i] = log2(index[i])
    end

# out
log_n_out = hcat(log_n_out, log_n)
#log_rs_out = hcat(log_rs_out, log_RS)
out_RS = hcat(out_RS, RS_out) # re-scaled range

c = c+1
end

# access dims of the matrix
# row ,col
#log_rs_out[20,:]

# Calculate expected value for R/S over various n
# Take the rolling mean over each varying n lag at n width
expected_RS = zeros(size(out_RS,1), size(out_RS,2))
n=100
for j = 1:size(out_RS,2)  # loop on column dimension
    for i = n:size(out_RS,1) # loop on row dimension
            expected_RS[i,j] =  mean(out_RS[i-n+1:i,j])
            end
        end

# log 2
expected_log_RS = zeros(size(out_RS,1), size(out_RS,2))
c=1
for j = 1:size(expected_log_RS,2)  # loop on column dimension
    for i = n:size(expected_log_RS,1) # loop on row dimension
            expected_log_RS[i,j] =  log2(expected_RS[i,j])
            end
                        c=c+1
        end

    # Regression slope of log(n) and expected value of log(R/S)
    # x = log(n), y = log(R/S)
    b_slope = zeros(size(out_RS,1))
    A_intercept = zeros(size(out_RS,1))

    for i = n:size(out_RS,1)
        xi = log_n_out[i,:]  # grab the row for varying lags of log_n
        yi = expected_log_RS[i,:] # grab the row for varying lags of r/s value
        # Mean of X (Mx)
        Mx = mean(xi)
        # Mean of Y (My)
        My = mean(yi)
        # Standard deviation of X (sX)
        sX = sqrt(var(xi))
        # Standard Deviation of Y (sY)
        sY = sqrt(var(yi))
        # Correlation of x and y  (r)
        r = cor(xi,yi)
        # slope (b) = r * (sY/sX)
        b = r * (sY/sX)
    # find intercept A = MY - bMX
    A = My - (b*Mx)

# out
b_slope[i] = b
A_intercept[i] = A
end
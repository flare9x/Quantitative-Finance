# Rolling Autocorrelation
# Sliding window (k width)
mean_out = zeros(size(d_es_close,1))
var_out = zeros(size(d_es_close,1))
cov_out = zeros(size(d_es_close,1))
autocor_out = zeros(size(d_es_close,1))
devmean_out = zeros(size(d_es_close,1))
n=1000
k = 1 # set lag
n_lag = 100 # return series lag

# Calculate 1 bar return series
rets = zeros(size(d_es_close,1))
for i in n_lag+1:length(d_es_close)
    rets[i] = ((d_es_close[i] / d_es_close[i-n_lag])-1) # rets[i] = ((d_es_close[i] / d_es_close[i-n_lag+1])-1)
end

# lag the series by k
lagged_series = [fill(0,k); rets[1:end-k]]

# Calculate the mean of the rolling  sample
for i = n:size(rets,1)
    mean_out[i] = mean(rets[i-n+1:i])
end

# Calculate deviations from the mean
for i = n:size(rets,1)
devmean_out[i] = rets[i] - mean_out[i]
end

# Calculate rolling variance
for i = n:size(rets,1)
    var_out[i] = var(rets[i-n+1:i])
end

# Calculate rolling covariance
for i = n:size(rets,1)
    if i+k >= size(rets,1)
        break
    end
    cov_out[i] = cov(rets[i-n+1:i],lagged_series[i-n+1:i])
end

# Rolling Autocorrelation
for i =n:size(rets,1)
    autocor_out[i] = cov_out[i] / var_out[i]
end
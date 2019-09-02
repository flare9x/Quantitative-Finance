# Packages
using Indicators 

# Build H L C matrix
h = data1_h
l = data1_l
c = data1_c
m = Array{Float64}(zeros(length(data1_c),0))
m = hcat(m,h)
m = hcat(m,l)
m = hcat(m,c)

# True Range
t_r = tr(m)

# Function for adjusting the price by a % of the true range 
    noise_perc = function(x::Array{Float64}; perc::Float64=.4)
            noise_out = zeros(t_r)
            for i =1:size(t_r,1)
            if isnan(t_r[i]) == 0
          percs = (perc* (t_r[i]))
          pos_range = collect(0.0:.25:percs)
          neg_range = -(pos_range)
          all_range = vcat(pos_range,neg_range) 
         noise_out[i] = (x[i]) + sample(all_range)  # sample() to choose a random number within the range up to perc= maximum of TR
      else
          noise_out[i] = 0
      end
  end
      return noise_out
  end

# Run function
# perc= sets the maximum % to adjust the prices by
noise_perc(data1_c,perc=.4)
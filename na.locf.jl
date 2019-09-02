# NA LOCF 
# First convert missing to 0 (just easier to work with) 

# Pull data to array 
example_array = df[:col]
example_array = Float64.(collect(Missings.replace(example_array, 0.0)))

# NA LOCF Function
function locf(x::Array{Float64})
    dx = zeros(x)
    for i in 2:length(x)
        if i == 2
            # fill index 1
            dx[i-1] = x[i-1]
        end
    if (x[i] == 0.0 && x[i-1] != 0.0)
        dx[i] = x[i-1]
    else
        dx[i] = x[i]
    end
    if (x[i] == 0.0 && x[i-1] == 0.0)
                dx[i] = dx[i-1]
    end
end
    return dx
end

# run na locf function 
out = locf(example_array)


# Fill 1x gap 
function locf(x::Array{Float64})
dx = zeros(x)
for i in 2:length(x)-1
    if x[i+1] > 0 && x[i] == 0.0
        dx[i+1] = x[i+1]
    end
        if dx[i] == 0 
            dx[i] = dx[i-1]
        end
    end
    return dx
end
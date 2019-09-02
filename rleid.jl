function rleid(x::AbstractVector)
    isempty(x) && return Int[]
    rle = similar(x, Int)
    idx = 1
    rle[1] = idx
    prev = x[1]
    for i in 2:length(x)
        this = x[i]
        if ismissing(this)
            if !ismissing(prev)
                prev = this
                idx += 1    
            end
        else
            if ismissing(prev) || this != prev
                prev = this
                idx += 1
            end
        end
        rle[i] = idx
    end
    rle
end

# test 

Main> rleid([missing,3,4,4,missing,1,1,missing,missing,6])
10-element Array{Int64,1}:
 1
 2
 3
 3
 4
 5
 5
 6
 6
 7
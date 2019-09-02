# Split data to train and test sets
# dummy data
data = collect(1:1:100)
# Train and test sets
# Set specific % for train and test sets
len = length(data)
mult = .30
split_no = Int64.(round(mult * len))
train_set = data[1:split_no]
test_set = data[split_no+1:len]
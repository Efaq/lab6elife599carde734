knapsack_brute_force = function(x, W){
  stopifnot(is.data.frame(x),
            "v" %in% colnames(x),
            "w" %in% colnames(x),
            x["v"] >= 0,
            x["w"] >= 0,
            W > 0)
  n = nrow(x)
  packed_combinations = 0:(2**n-1)
  #create dataframe or list with indexes as elements of packed combinations
  #for element in packed combinations:
    #unpack to binary
    #use indexes to select elements from data.frame of weights and values
    #calculate total value
    #set to zero if bigger than W
    #add value to dataframe
  #pick index of larger one, unpack and return value and list of indexes
}

#a = intToBits(20)
#packBits(a, type = "integer")
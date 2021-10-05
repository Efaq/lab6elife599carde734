brute_force_knapsack = function(x, W){
  stopifnot(is.data.frame(x),
            "v" %in% colnames(x),
            "w" %in% colnames(x),
            x["v"] >= 0,
            x["w"] >= 0,
            W > 0)
  n = nrow(x)
  packed_combinations = 1:(2**n-1)
  brute_force_df = data.frame(row.names = packed_combinations)
  brute_force_df["weight"] = 0
  brute_force_df["value"] = 0
  for (el in row.names(brute_force_df)){
    choices = which(intToBits(el) == 1)
    total_choice_weigth = sum(x[["w"]][choices])
    total_choice_value = sum(x[["v"]][choices])
    brute_force_df[el,]["weight"] = total_choice_weigth
    brute_force_df[el,]["value"] = ifelse(total_choice_weigth <= W, total_choice_value, 0)
  }
  solution = which.max(brute_force_df[["value"]])
  return(list(value = brute_force_df[["value"]][[solution]],
              elements = which(intToBits(solution) == 1)))
}
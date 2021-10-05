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
  brute_force_df[["choices"]] = lapply(row.names(brute_force_df), function(inp){which(intToBits(inp) == 1)})
  brute_force_df[["total_weigth"]] = lapply(brute_force_df[["choices"]], function(inp) {sum(x[["w"]][inp])})
  brute_force_df[["total_value"]] = lapply(brute_force_df[["choices"]], function(inp) {sum(x[["v"]][inp])})
  brute_force_df[["is_feasible"]] = lapply(brute_force_df[["total_weigth"]], function(inp){ifelse(inp <= W, 1, 0)})
  brute_force_df[["value"]] = lapply(row.names(brute_force_df), function(inp){brute_force_df[[inp,"total_value"]] * brute_force_df[[inp,"is_feasible"]]})
  solution = which.max(brute_force_df[["value"]])
  return(list(value = brute_force_df[["value"]][[solution]],
              elements = which(intToBits(solution) == 1)))
}
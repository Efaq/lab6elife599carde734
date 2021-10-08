maybe_par_lapply = function(is_parallel = FALSE) {
  if (is_parallel) {
    num_cores = parallel::detectCores()
    cluster = parallel::makeCluster(getOption("cl.cores", num_cores))
    return(function(input, func) {
      parallel::parLapply(cl = cluster, X = input, fun = func)
    })
  } else {
    return(function(input, func) {
      lapply(X = input, FUN = func)
    })
  }
}

brute_force_knapsack = function(x, W, parallel = FALSE) {
  stopifnot(is.data.frame(x),
            "v" %in% colnames(x),
            "w" %in% colnames(x),
            x["v"] >= 0,
            x["w"] >= 0,
            W > 0)
  lapply_func = maybe_par_lapply(is_parallel = parallel)
  n = nrow(x)
  packed_combinations = 1:(2 ** n - 1)
  brute_force_df = data.frame(row.names = packed_combinations)
  brute_force_df[["choices"]] = lapply_func(
    input = row.names(brute_force_df),
    func = function(inp) {
      which(intToBits(inp) == 1)
    }
  )
  brute_force_df[["total_weigth"]] = lapply_func(
    input = brute_force_df[["choices"]],
    func = function(inp) {
      sum(x[["w"]][inp])
    }
  )
  brute_force_df[["total_value"]] = lapply_func(
    input = brute_force_df[["choices"]],
    func = function(inp) {
      sum(x[["v"]][inp])
    }
  )
  brute_force_df[["is_feasible"]] = lapply_func(
    input = brute_force_df[["total_weigth"]],
    func = function(inp) {
      ifelse(inp <= W, 1, 0)
    }
  )
  brute_force_df[["value"]] = unlist(brute_force_df[["total_value"]]) * unlist(brute_force_df[["is_feasible"]])
  solution = which.max(brute_force_df[["value"]])
  return(list(value = brute_force_df[["value"]][[solution]],
              elements = which(intToBits(solution) == 1)))
}

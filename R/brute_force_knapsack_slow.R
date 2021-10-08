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


#'Brute force search approach for Knapsack problem - Slow version
#'
#'\code{brute_force_knapsack_slow} uses a "brute force" algorithm in order to solve the knapsack problem. 
#'Knapsack problem is a discrete optimization problem where we have a knapsack that can take a
#'limited weight W and we want to fill this knapsack with a number of items i = 1, ..., n, each with a weight wi and a value vi. 
#'The goal is to find the knapsack with the largest value of the elements added to
#'the knapsack.\cr \cr Brute force evaluates the total weight and value of all possible subsets, then selects the subset 
#'with the highest value that is still under the weight limit.
#'While this is an effective solution, it is not optimal because the time complexity is exponential. \cr \cr  This is a slower/not improved
#' version of brute_force_knapsack function.
#'
#'
#' @param x, data.frame with two variables v and w. Variables should be positive.
#'
#' @param W, numeric scalar, represents capacity.
#' 
#' @param parallel, TRUE or FALSE. 
#'\cr Is FALSE by default.If set to TRUE, the function parallelizes
#'over the detected cores.
#'
#'
#'
#'
#' @return value and elements that fill the knapsack with some given items, so that the value of the selected items is maximized.
#'
#'
#'
#' @examples
#' 
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' 
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <-
#'data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#'
#'
#'brute_force_knapsack_slow(x = knapsack_objects[1:8,], W = 3500)
#'brute_force_knapsack_slow(x = knapsack_objects[1:12,], W = 3500)
#'brute_force_knapsack_slow(x = knapsack_objects[1:8,], W = 2000)
#'brute_force_knapsack_slow(x = knapsack_objects[1:12,], W = 2000)
#'
#' 
#' @importFrom methods new
#' @export brute_force_knapsack_slow

########################################################################################



brute_force_knapsack_slow = function(x, W, parallel = FALSE) {
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
  brute_force_df[["value"]] = lapply_func(
    input = row.names(brute_force_df),
    func = function(inp) {
      brute_force_df[[inp, "total_value"]] * brute_force_df[[inp, "is_feasible"]]
    }
  )
  solution = which.max(brute_force_df[["value"]])
  return(list(value = brute_force_df[["value"]][[solution]],
              elements = which(intToBits(solution) == 1)))
}

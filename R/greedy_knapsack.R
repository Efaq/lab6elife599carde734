#'Greedy heuristic approach for Knapsack problem
#'
#'\code{greedy_knapsack} uses a Greedy heuristic algorithm in order to solve the knapsack problem. 
#'Knapsack problem is a discrete optimization problem where we have a knapsack that can take a
#'limited weight W and we want to fill this knapsack with a number of items i = 1, ..., n, each with a weight wi and a value vi. 
#'The goal is to find the knapsack with the largest value of the elements added to
#'the knapsack.
#'\cr \cr 
#'\code{greedy_knapsack} algorithm will not
#'give an exact result (but it can be shown that it will return at least 50% of the true maximum value),
#'but it will reduce the computational complexity considerably (actually to O(n log n) due to the sorting
#'part of the algorithm). 
#'
#'
#' @param x, data.frame with two variables v and w.
#'
#' @param W, numeric scalar, represents capacity.
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
#'greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#'
#' 
#' @source 
#' Read more at \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm.}
#'
#' @importFrom methods new
#' @export greedy_knapsack

########################################################################################


greedy_knapsack = function(x, W) {
  stopifnot(is.data.frame(x),
            "v" %in% colnames(x),
            "w" %in% colnames(x),
            x["v"] >= 0,
            x["w"] >= 0,
            W > 0)
  x[["v_div_w"]] = x[["v"]] / x[["w"]]
  filtered_x = x[which(x[["w"]] <= W),]
  ordered_x = filtered_x[order(filtered_x[["v_div_w"]], decreasing =  TRUE),]
  knapsack_weigth = 0
  knapsack_value = 0
  knapsack_content = vector()
  alternative_item = NULL
  counter = 1
  
  while (knapsack_weigth < W & counter <= nrow(ordered_x)) {
    item = ordered_x[counter,]
    if (knapsack_weigth + item[["w"]] <= W) {
      knapsack_weigth = knapsack_weigth + item[["w"]]
      knapsack_value = knapsack_value + item[["v"]]
      knapsack_content = append(knapsack_content, strtoi(rownames(item)))
    } else {
      alternative_item = item
      break
    }
    counter = counter + 1
  }
  if (is.null(alternative_item) ||
      knapsack_value > alternative_item[["v"]]) {
    return(list(value = knapsack_value, elements = knapsack_content))
  } else {
    return(list(value = alternative_item[["v"]],
                elements = strtoi(row.names(
                  alternative_item
                ))))
  }
}

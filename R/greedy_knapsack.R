greedy_knapsack = function(x, W) {
  stopifnot(is.data.frame(x),
            "v" %in% colnames(x),
            "w" %in% colnames(x),
            x["v"] >= 0,
            x["w"] >= 0,
            W > 0)
  x[["v_div_w"]] = x[["v"]] / x[["w"]]
  filtered_x = x[which(x[["w"]] <= W), ]
  ordered_x = filtered_x[order(filtered_x[["v_div_w"]], decreasing =  TRUE), ]
  knapsack_weigth = 0
  knapsack_value = 0
  knapsack_content = vector()
  alternative_item = NULL
  counter = 1
  
  while (knapsack_weigth < W & counter <= nrow(ordered_x)) {
    item = ordered_x[counter, ]
    if (knapsack_weigth + item[["w"]] <= W) {
      knapsack_weigth = knapsack_weigth + item[["w"]]
      knapsack_value = knapsack_value + item[["v"]]
      knapsack_content = append(knapsack_content, strtoi(rownames(item)))
      }# else if (is.null(alternative_item)) {
   #   alternative_item = item
    #}
    counter = counter + 1
  }
  #if (!is.null(alternative_item) && knapsack_value > alternative_item[["v"]]) {
    return(list(value = knapsack_value, elements = knapsack_content))
  #} else {
  #  return(list(
  #    value = alternative_item[["v"]],
  #    elements = strtoi(row.names(alternative_item))
  #  ))
  #}
}

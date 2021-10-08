## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lab6elife599carde734)

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE),
v=runif(n = n, 0, 10000)
)

## -----------------------------------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

## -----------------------------------------------------------------------------
sample_size = 1 #This value is 1 to keep the vignette quick to run, but should be larger to reduce the variance of the resulting times
t1 = system.time({for (i in 1:sample_size) brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)})/sample_size
print("User time:")
t1[[1]]
print("System time:")
t1[[2]]

## -----------------------------------------------------------------------------
sample_size = 1 #This value is 1 to keep the vignette quick to run, but should be larger to reduce the variance of the resulting times
t2 = system.time({for (i in 1:sample_size) brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel=TRUE)})/sample_size
print("User time:")
t2[[1]]
print("System time:")
t2[[2]]

## -----------------------------------------------------------------------------
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)

## -----------------------------------------------------------------------------
sample_size = 1 #This value is 1 to keep the vignette quick to run, but should be larger to reduce the variance of the resulting times
t3 = system.time({for (i in 1:sample_size) knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)})/sample_size
print("User time:")
t3[[1]]
print("System time:")
t3[[2]]


## -----------------------------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

## -----------------------------------------------------------------------------
n <- 1000000
knapsack_objects_extended <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE),
v=runif(n = n, 0, 10000)
)

## -----------------------------------------------------------------------------
sample_size = 1 #This value is 1 to keep the vignette quick to run, but should be larger to reduce the variance of the resulting times
t4 = system.time({for (i in 1:sample_size) greedy_knapsack(x = knapsack_objects_extended, W = 3500)})/sample_size
print("User time:")
t4[[1]]
print("System time:")
t4[[2]]

## ----eval = FALSE-------------------------------------------------------------
#  brute_force_df[["value"]] = lapply_func(
#    input = row.names(brute_force_df),
#    func = function(inp) {
#      brute_force_df[[inp, "total_value"]] * brute_force_df[[inp, "is_feasible"]]
#    }
#  )

## ----eval = FALSE-------------------------------------------------------------
#  brute_force_df[["value"]] = unlist(brute_force_df[["total_value"]]) * unlist(brute_force_df[["is_feasible"]])


#'Dynamic programming approach for Knapsack problem
#'
#'\code{knapsack_dynamic} uses a dynamic programming algorithm in order to solve the knapsack problem. 
#'Knapsack problem is a discrete optimization problem where we have a knapsack that can take a
#'limited weight W and we want to fill this knapsack with a number of items i = 1, ..., n, each with a weight wi and a value vi. 
#'The goal is to find the knapsack with the largest value of the elements added to
#'the knapsack. \cr \cr knapsack_dynamic uses dynamic programming, which is a method for solving
#'optimization problems.
#'The idea is to compute the solutions to the subsub-problems
#'once and store the solutions in a table, so that they
#'can be reused (repeatedly) later.
#'
#' @param x, data.frame with 2 variables inside, Weights and Values.
#'
#' @param W, numeric scalar, represents capacity.
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
#'knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#'knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#'
#' 
#' @source 
#' Read more at \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem.}
#'
#' @importFrom methods new
#' @export knapsack_dynamic

########################################################################################


knapsack_dynamic<-function(x,W)
{ 
  if(is.data.frame(x)!=TRUE)
  {
    stop("First input variable should be a dataframe")
  }
  
  
  if(is.numeric(W)!=TRUE || W <=0)
  {
    stop("Second input variable should be a positive numeric")
  }
  
  weights<- as.vector(x[[1]])
  values<- as.vector(x[[2]])
  
  #verify if weights are discrete   
  
  if(is.integer(weights)!=TRUE)
  {
    stop("Dataframe should contain weigths with dicrete values so that dynamic programming can be used")
  }

  n<- length(values)
  elements = vector(length = n)
  elements[] <- 0 
  
  dynamic_matrix<-matrix(nrow=n,
                         ncol=W+1)
  
  i=1
  j=1
  
  #calculates matrix with data 
  
  while(i<=n)
  { 
    j=1
    
    while(j<=W+1)
    { 
      if(j==1 )
      {
        dynamic_matrix[i,j]=0
        
      }
      
      
      else if(weights[i]<j)
      {
        res1 <- dynamic_matrix[i-1,j]
        res2 <- dynamic_matrix[i-1,j-weights[i]]
        res3 <- values[i]
        res4 <- res2+res3
        
        if (length(res1)==0)
        {
          res1<-0
        }
        
        if(length(res2)==0)
        {
          res4<-res3
        }
        
        res<- max(res1,res4)
        dynamic_matrix[i,j]=res
        
      }
      
      else
      { 
        
        if(length(dynamic_matrix[i-1,j])==0)
        {
          dynamic_matrix[i,j]=0
        }
        else
        {
          dynamic_matrix[i,j]=dynamic_matrix[i-1,j]
        }
        
      }
      
      j=j+1
      
    }
    
    
    i=i+1 
  }
  
  #calculates best solution
  
  max_value <- dynamic_matrix[n,W+1]
  
  value = max_value
  
  while(n>=1)
  {
    
    if(length(setdiff(value, dynamic_matrix[n-1,]))==1)
    {
      elements[n]=1
      value = value-values[n]
      
      if (value<=0)   break;
             
    }
    else
    {
      elements[n]=0
    }
    
    n=n-1
  }
 
  final_elements<-which(elements==1)
  
  return(list(value=max_value, elements=final_elements))
  
}





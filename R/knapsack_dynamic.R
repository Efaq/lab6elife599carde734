knapsack_dynamic<-function(x,W)
{ 
  
  weights<- as.vector(x[[1]])
  values<- as.vector(x[[2]])
  
  #verify if weights are discrete   
  
  if(typeof(weights)!="integer")
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





knapsack_dynamic<-function(x,W)
{ 
  #tirar a iniciação de valores
  W<-3500
  x<-knapsack_objects[1:6,]
  
  
  weights<- as.vector(x[[1]])
  values<- as.vector(x[[2]])
  
  
  weights<- c(5,3,4,2)
  values<-c(60,50,70,30)
  W<-5
  
  list_result<- list(value=0, elements=c(0))
  
  
  #i=1
 # j=1
  
  dynamic_matrix<-matrix(nrow=length(values),
                         ncol=W+1)
  
  #set first row and columns to zero
  
 # while(j <= W+1 )
 # {
 #  dynamic_matrix[1,j]=0
    
  #  j=j+1
  #}
  
 # while(i <= length(values) )
 # {
 #   dynamic_matrix[i,1]=0
    
  #  i=i+1
 # }
  
  
  i=1
  j=1

  while(i<=length(values))
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
        res2 <- dynamic_matrix[i-1,j-W[i]]
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
        
       
        #dynamic_matrix[i,j]=123
        
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
  
  return(dynamic_matrix)
  
  
}




# Computes the euclidian distance between two vectors
euc_dist <- function(x, y) {
  
  if(length(x) != length(y)){
    
    stop("Vectors must be the same size, ye wee cunt!")
    
    } else {
      
        dist_1 <- 0
        
        for(i in 1:length(x)) {
        
            dist_1 <- dist_1 + (x[i] - y[i])^2
          
        }
        
        dist <- sqrt(dist_1)
        return(dist)
        
  }

} 
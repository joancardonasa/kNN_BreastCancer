# Computes two versions of normalizing functions
normalize_z <- function(x) {
  
  if(class(x) != "factor"){
      return( (x - mean(x)) /  sd(x) )
  } else {
      return(x)
  }

  }

normalize <- function(x) {
  
  if(class(x) != "factor"){
     return( (x - min(x)) /  (max(x) - min(x)) )
  } else {
      return(x)
  }

  }


# Let's see which one gives us the best results
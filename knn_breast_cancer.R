# Breast Cancer Diagnosis using k - Nearest Neighbours
# ----------------------------------------------------

# Data collected from "Breast Cancer Wisconsin Diagnostic" dataset from the UCI Machine Learning Repository

wdbc <- read.csv("wdbc.data", header = F, sep = ",")

# Features names:

features <- c("id","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean",
              "concavity_mean","concave points_mean","symmetry_mean","fractal_dimension_mean","radius_se","texture_se","perimeter_se",
              "area_se","smoothness_se","compactness_se","concavity_se","concave points_se","symmetry_se","fractal_dimension_se","radius_worst",
              "texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave points_worst",
              "symmetry_worst","fractal_dimension_worst")

colnames(wdbc) <- features

# str(wdbc): 569 obs, 31 features, 1 output

wdbc <- wdbc[-1] # Forgets the first column, "id", since we don't actually need it

# 30 feaures, 1 output (Diagnosis). This variable is a factor (since many libraries in R require factors):

wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

table(wdbc$diagnosis) #357 benign, 212 malignant

# With this, we also know the proportion: Benign 62,7%, Malignant 37,3%

# The values are still not in the correct range, since some of them go up to thousands while others lie around 0,something. We must NORMALIZE
# k-NN is heavily dependant on the measurement scale of the input features. We will use a couple of normalizing functions in order to see which
# one works best for this case:

wdbc_n <- as.data.frame(sapply(wdbc[2:31], normalize)) # regular normalization (x-min/max-min)

wdbc_nz <- as.data.frame(sapply(wdbc[2:31], normalize_z)) # z-score standarization (x-mean/max-min)

# wdbc_n <- cbind(wdbc$diagnosis, wdbc_n)
# colnames(wdbc_n)[colnames(wdbc_n) == 'wdbc$diagnosis'] <- 'diagnosis' #changes the name "wdbc$diagnosis" to just "diagnosis"
# 
# wdbc_nz <- cbind(wdbc$diagnosis, wdbc_nz)
# colnames(wdbc_nz)[colnames(wdbc_nz) == 'wdbc$diagnosis'] <- 'diagnosis'

# Now we have two normalized dataframes, one using regular normalization and the other using z-score standarization

# Let's separate the data into the training data and the test data, to evaluate the algorithms "learning" capacity or accuracy:

wdbc_train <- wdbc_n[1:469,]

wdbc_test <- wdbc_n[470:569,]

# Let's store the outputs in a couple of vectors for each of the datasets:

wdbc_train_labels <- wdbc[1:469, 1]

wdbc_test_labels <- wdbc[470:569, 1]

# k-NN of wdbc_n:
# ---------------

# Choosing the right k: balance between overfitting and underfitting the training data: bias-variance tradeoff

# Choosing an appropriate k:

# Common practice is choosing k = sqrt(m). For now, we will do this, but we will try other k's and evaluate their performance

m <- length(wdbc_train_labels)

k <- ceiling(sqrt(m)) #ceiling rounds up to the higher value, in this case 22, so the decision will be voted by the 22 nearest neighbours 

test <- as.matrix(wdbc_test) # convert the training and test sets into matrices

train <- as.matrix(wdbc_train)

# Matrix creation: In this step, let's build a 469 x 100 matrix. Each column will contain the euclidian distance between a test example and a
# train example. Once it is done, we will build a 22 x 100 matrix, in which we will store the 22 lowest distances, which will vote the 
# classification output for each test example. It is VERY IMPORTANT to take into account the indexes, so we know the outputs in order to 
# classify.

# Also, I just realised how slow R is. Gonna have to think about using Python or C or some other language that doesn't take more than 5 minutes
# to compute a double for loop

mat <- matrix(nrow = 469, ncol = 100)

for(i in 1:100) {

  test_vec <- test[i,]
  
  for(j in 1:469) {
    
    train_vec <- train[j,]
    
    mat[j,i] <- euc_dist(test_vec, train_vec)
    
  }

}

# Seems like mat is returning us what we want - for now: The euclidian distance of each test example vs each training example

matt <- as.data.frame(mat)

matt <- cbind(matt, wdbc_train_labels) # matt now contains in its 101th column the output for training example

# We now want, for each column of matt, to return us a vector containing the 22 lowest values of that column. These are the 22-Nearest Neighbors.

# The Holy 22 will then proceed to vote whether their corresponding test example is Beningn or Malignant. If there is a tie, it will break randomly.

# It should be computing the total sum of distances, but we'll do that in some other case.

dec_vec <- array()

for(o in 1:100){
  
  decision <- matt[which(matt[,o] %in% sort(matt[,o])[1:k]),101] # Here we build a vector containing the output for the 22 lowest distances
  
  n_mal <- sum(decision == "Malignant") 
  
  if(n_mal > (k/2)){ # If more than 11 (50%) of these examples are Malignant, then:
    
    dec_vec[o] <- "Malignant" # We assign the output Malignant to that test example
    
  } else { # If they are not:
    
    dec_vec[o] <- "Benign" # We assign the output Benign to that test example
     
  }
  
}

accuracy <- sum(dec_vec == wdbc_test_labels)/100 # Doing this we obtain the total of examples in which we have predicted correctly (98/100)

print(paste("Using this k-NN algorithm of k =",k,"we obtain an accuracy of", accuracy*100, "%"))



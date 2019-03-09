#' LMSquareLossEarlyStoppingCV
#'
#' @param X.mat 
#' @param y.vec 
#' @param fold.vec 
#' @param max.iterations 
#'
#' @return Output a list with the following named elements:
#' mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
#' selected.steps weight.vec, the weight vector found by using gradient descent with selected.steps on the whole training data set.
#' predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
#' @export
#'
#' @examples
#'    library(codingProject2)
#'    data(ozone, package = "ElemStatLearn")
#'    X.mat<-ozone[1:20,-1]
#'    y.vec<-ozone[1:20, 1]
#'    max.iterations <- 100
#'    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
#'    res <- LMSquareLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations)
LMSquareLossEarlyStoppingCV <- function(
  X.mat,
  y.vec,
  fold.vec,
  max.iterations
){
  
  if(nrow(X.mat) <= 0 || ncol(X.mat) <= 0)  
  {
    error("Feature matrix has unexpected dimensions")
  }
  else if(nrow(y.vec) <= 0 || ncol(y.vec) <= 0)
  {
    error("Label vec has unexpected dimensions")
  }
  
  step_size <- 0.1
  
  #in 1:5 because nfolds is given as 5 just for trial need to look for ways how to find distinct elements in fold.vec
  train.loss.mat <- matrix(,max.iterations,4)
  validation.loss.mat <- matrix(,max.iterations,4)
  n.folds <- max(fold.vec)
  for(fold.i in 1:n.folds)
  {
    validation_indices <- which(fold.vec %in% c(fold.i))
    validation_set <- X.mat[validation_indices,]
    train_set <- X.mat[-validation_indices,]
    train_labels <- y.vec[-validation_indices]
    validation_labels <- y.vec[validation_indices] 
    n_rows_validation_set <- nrow(validation_set)
    n_rows_train_set <- nrow(train_set)
    
    W <- LMSquareLossIterations(train_set,train_labels, max.iterations, step_size )
    
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        to.be.predicted <- train_set
      }
      else{
        to.be.predicted <- validation_set
      }
      
      pred <- as.matrix(cbind(1,to.be.predicted)) %*% W 
      
      if(identical(prediction.set.name, "train")){
        loss.mat <-(pred - as.vector(train_labels))^2
        train.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
      else{
        loss.mat <- (pred - as.vector(validation_labels))^2
        validation.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
    }
  }
  mean.validation.loss.vec <- rowMeans(validation.loss.mat)
  mean.train.loss.vec <- rowMeans(train.loss.mat)
  selected.steps <- which.min(mean.validation.loss.vec)
  w.head <- LMSquareLossIterations(X.mat,y.vec, selected.steps, step_size)
  weight_vec <- w.head[,selected.steps]
  
  returnList <- list(mean.validation.loss = mean.validation.loss.vec,
                     mean.train.loss.vec =   mean.train.loss.vec,
                     selected.steps = selected.steps, weight.vec = weight_vec,
                     predict=function(X.test){return(X.test * weight_vec)})
  return(returnList)
}


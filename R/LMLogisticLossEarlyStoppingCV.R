#' LMLogisticLossEarlyStoppingCV
#'
#' This function uses cross fold validatoion to find the percision of the 
#' LMLogisticLossIterations function
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n]
#' @param fold.vec a vector of fold ids
#' @param max.iterations scalar integer, max number of iterations
#'
#' @return Output a list with the following named elements:
#' mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
#' selected.steps weight.vec, the weight vector found by using gradient descent with selected.steps on the whole training data set.
#' predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
#' 
#' @export
#' 
#' @examples
#'    library(codingProject2)
#'
#'    data(SAheart , package = "ElemStatLearn")
#'    X.mat<-SAheart [1:50,-9]
#'    y.vec<-SAheart [1:50, 9]
#'    max.iterations <- 100
#'    fold.vec <- sample(rep(1:5, l=nrow(X.mat)))
#'    
#'    result <- LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations)
LMLogisticLossEarlyStoppingCV <- function(
  X.mat,
  y.vec,
  fold.vec,
  max.iterations
){
  
  if(nrow(X.mat) <= 0 || ncol(X.mat) <= 0 || nrow(y.vec) <= 0 || ncol(y.vec) <= 0)  
  {
    error("Feature matrix or Label vec has unexpected dimensions")
  }

  step_size <- 0.1
  #in 1:5 because nfolds is given as 5 just for trial need to look for ways how to find distinct elements in fold.vec
  #need to ask how to find number of fold from fol.vec so that we can replace 1:5 with 1:n.folds
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
    
    W <- LMSquareLossIterations(train_set,train_labels, max.iterations, step_size)
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        to.be.predicted <- train_set
      }
      else{
        to.be.predicted <- validation_set
      }
      pred <- as.matrix(cbind(1,to.be.predicted)) %*% as.matrix(W) 
      if(identical(prediction.set.name, "train")){
        loss.mat <-ifelse(pred.mat>0.5, 1, 0) != train_labels
        train.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
      else{
        loss.mat <-ifelse(pred.mat>0.5, 1, 0) != validation_labels
        validation.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
    }
  }
  mean.validation.loss.vec <- rowMeans(validation.loss.mat)
  mean.train.loss.vec <- rowMeans(train.loss.mat)
  selected.steps <- which.min(mean.validation.loss.vec)
  w.head <- LMLogisticLossIterations(X.mat,y.vec, selected.steps, step_size)
  weight_vec <- w.head[,selected.steps]
  
  returnList <- list(mean.validation.loss = mean.validation.loss.vec,
                     mean.train.loss.vec =   mean.train.loss.vec,
                     selected.steps = selected.steps, weight.vec = weight_vec,
                     predict=function(X.test){return(as.matrix(X.test) %*% weight_vec)})
  return(returnList)
}
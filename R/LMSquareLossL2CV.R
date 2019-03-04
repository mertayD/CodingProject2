#' LMSquareLossL2CV
#'
#' This function uses cross fold validation to find the percision of the 
#' LMSquareLossL2CV function
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n]
#' @param fold.vec a vector of fold ids
#' @param penalty.vec vector of penalties
#'
#' @return Output a list with the following named elements:
#' mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty (for plotting train/validation loss curves)
#' weight.vec, the weight vector found by using gradient descent with selected.penalty on the whole training data set.
#' predict(testX.mat), a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
#' @export
#'
#' @examples

LMSquareLossL2CV <- function(
  X.mat, 
  y.vec, 
  fold.vec, 
  penalty.vec
  ){
  n.folds <- max(fold.vec)
  for (fold.i in n.folds){
    validation_indices <- which(fold.vec %in% c(fold.i))
    validation_set <- X.mat[validation_indices,]
    train_set <- X.mat[-validation_indices,]
    train_labels <- y.vec[-validation_indices]
    validation_labels <- y.vec[validation_indices] 
    n_rows_validation_set <- nrow(validation_set)
    n_rows_train_set <- nrow(train_set)
    
    W <- LMSquareLossL2Penalties(train_set,train_labels,penalty.vec)
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        to.be.predicted <- train_set
      }
      else{
        to.be.predicted <- validation_set
      }
      
      pred <- to.be.predicted %*% W 
      
      if(identical(prediction.set.name, "train")){
        loss.mat <- (sweep(pred,2, as.vector(validation_labels),"-"))^2
        train.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
      else{
        loss.mat <- (sweep(pred,2, as.vector(train_labels),"-"))^2
        validation.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
    }
  }
  #To be returned
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  mean.train.loss.vec <- colMeans(train.loss.mat)
  #don't forget to return penalty.vec for plot
  selected.penalty <- which.min(mean.validation.loss.vec)
  w.head <- LMSquareLossL2Penalties(X.mat,y.vec,penalty.vec)
  index <- which(penalty.vec %in% c(selected.penalty))
  weight_vec <- w.head[,index]
  
  returnList <- list(mean.validation.loss = mean.validation.loss.vec,
                     mean.train.loss.vec =   mean.train.loss.vec1, penalty.vec = penalty.vec, 
                     selected.penalty = selected.penalty, weight.vec = weight_vec,
                     predict=function(X.test){return(X.test * weight_vec)})
}
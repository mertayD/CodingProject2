#' LMLogisticLossL2CV
#'
#' This function uses cross fold validation to find the percision of the 
#' LMLogisticLossL2 function
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
#'   data(SAheart , package = "ElemStatLearn")
#'   X.mat<-SAheart [1:50,-9]
#'   y.vec<-SAheart [1:50, 9]
#'   max.iterations <- 100
#'   fold.vec <- sample(rep(1:5, l=nrow(X.mat)))
#'   result <- LMLogisticLossL2CV(X.mat, y.vec, fold.vec, max.iterations)
LMLogisticLossL2CV <- function(
  X.mat, 
  y.vec, 
  fold.vec, 
  penalty.vec
  ){
  if(nrow(X.mat) <= 0 || ncol(X.mat) <= 0 || nrow(y.vec) <= 0 || ncol(y.vec) <= 0)  
  {
    error("Feature matrix or Label vec has unexpected dimensions")
  }
  n.folds <- max(fold.vec)
  for (fold.i in n.folds){
    validation_indices <- which(fold.vec %in% c(fold.i))
    validation_set <- X.mat[validation_indices,]
    train_set <- X.mat[-validation_indices,]
    train_labels <- y.vec[-validation_indices]
    validation_labels <- y.vec[validation_indices] 
    n_rows_validation_set <- nrow(validation_set)
    n_rows_train_set <- nrow(train_set)
    
    W <- LMLogisticLossL2Penalties(train_set,train_labels,penalty.vec)
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        to.be.predicted <- train_set
      }
      else{
        to.be.predicted <- validation_set
      }
      
      pred <- to.be.predicted %*% W 
      
      if(identical(prediction.set.name, "train")){
        loss.mat <- ifelse(pred.mat>0.5, 1, 0) != train_labels
        train.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
      else{
        loss.mat <- ifelse(pred.mat>0.5, 1, 0) != validation_labels
        validation.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
    }
  }
  #To be returned
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  mean.train.loss.vec1 <- colMeans(train.loss.mat)
  #don't forget to return penalty.vec for plot
  selected.penalty <- which.min(mean.validation.loss.vec)
  w.head <- LMLogisticLossL2Penalties(X.mat,y.vec,penalty.vec)
  index <- which(penalty.vec %in% c(selected.penalty))
  weight_vec <- w.head[,index]
  
  returnList <- list(mean.validation.loss = mean.validation.loss.vec,
                     mean.train.loss.vec =   mean.train.loss.vec1, penalty.vec = penalty.vec, 
                     selected.penalty = selected.penalty, weight.vec = weight_vec,
                     predict=function(X.test){return(X.test * weight_vec)})
}
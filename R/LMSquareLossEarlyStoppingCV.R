#' Title
#'
#' @param X.mat 
#' @param y.vec 
#' @param fold.vec 
#' @param max.iterations 
#'
#' @return
#' @export
#'
#' @examples
#'    library(codingProject2)
#'    data(ozone, package = "ElemStatLearn")
#'    X.mat<-ozone[1:20,-1]
#'    y.vec<-ozone[1:20, 1]
#'    max.iterations <- 30
#'    step.size <- 0.1
#'    fold.vec <- sample(rep(1:5, l=nrow(X.mat)))
#'    res <- LMSquareLossIterations(X.mat, y.vec, fold.vec, max.iterations)
LMSquareLossEarlyStoppingCV <- function(
  X.mat,
  y.vec,
  fold.vec,
  max.iterations
){
  step_size <- 0.1
  for(fold.i in fold.vec)
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
      
      pred <- cbind(1,to.be.predicted) %*% W 
      
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
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  selected.steps <- which.min(mean.validation.loss.vec)
  w.head <- LMSquareLossIterations(X.mat,y.vec, selected.steps, step_size)
  weight_vec <- w.head[selected.steps,]
  
  returnList <- list(mean.validation.loss = mean.validation.loss.vec,
                     mean.train.loss.vec =   mean.train.loss.vec, penalty.vec = penalty.vec, 
                     selected.penalty = selected.penalty, weight.vec = weight_vec,
                     predict=function(X.test){return(X.test * weight_vec)})
  return(returnList)
}


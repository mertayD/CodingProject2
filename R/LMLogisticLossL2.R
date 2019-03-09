#' LMLogisticLossL2
#'
#' find the optimal weight vector that minimizes the following cost function: ∑i=1^n L[w^T x_i, y_i] + penalty * ||w||, 
#' where L is either the logistic loss.
#'
#' @param X.scaled.mat already has mean=0 and sd=1 for each of its columns
#' @param Y.vec numeric input label vetor [n]
#' @param penalty non-negative numeric scalar
#' @param opt.thresh positive numeric scalar
#' @param initial.weight.vec vector of weight
#'
#' @return optimal weight vector for the given penalty parameter
#' 
#' @export
#' @examples
#'    library(codingProject2)
#'    data(spam, package = "ElemStatLearn")
#'    X.mat<-spam[1:100,-58]
#'    X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
#'    y.vec<-spam[1:100, 58]
#'    penalty <- 2
#'    opt.thresh <- 0.0001
#'    initial.weight.vec <- rep(0,ncol(X.scaled.mat))
#'    
#'    optimalWeightVector <- LMLogisticLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec)
LMLogisticLossL2 <- function(
  X.scaled.mat, 
  y.vec, 
  penalty,
  opt.thresh,
  initial.weight.vec
){
  weight_vec <- initial.weight.vec
  index <- 0 
  max.iterations <- 100
  for(i in 1:max.iterations)
  {
    
    pred <- as.matrix(X.scaled.mat) %*% as.matrix(weight_vec)
    sig <- sigmoid(pred)
    cost <- sig - y.vec
    gradient <- t(X.scaled.matmnv) %*% cost
    weight_vec <- weight_vec - (step.size) * ((gradient/n) + 2*penalty*weight_vec) 
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
    if(gradient > opt.thresh )
    {
      index<- i
    }
    
  }
  
  return(weights_scaled_mat[,index + 1])
} 

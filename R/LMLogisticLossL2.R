#' LMLogisticLossL2
#'
#' find the optimal weight vector that minimizes the following cost function: ∑i=1^n L[w^T x_i, y_i] + penalty * ||w||, 
#' where L is either the logistic loss.
#'
#' @param X.scaled.mat already has mean=0 and sd=1 for each of its columns
#' @param Y.vec numeric input label vetor [n]
#' @param penalty a vector of fold ids
#' @param opt.thresh positive numeric scalar
#' @param initial.weight.vec vector of weight
#'
#' @return optimal weight vector for the given penalty parameter
#' 
#' @export
#' @examples
#'
LMLogisticLossL2 <- function(
  X.scaled.mat, 
  y.vec, 
  penalty,
  opt.thresh,
  initial.weight.vec
){
  weight_vec <- initial.weight.vec
  index <- 0 
  for(i in 1:max.iterations)
  {
    
    pred <- as.matrix(X.gd) %*% as.matrix(weight_vec)
    sig <- sigmoid(pred)
    cost <- sig - y.vec
    gradient <- t(X.gd) %*% cost
    if(gradient > opt.thresh )
    {
      weight_vec <- weight_vec - (step.size) * ((gradient/n) + 2*penalty) 
      weights_scaled_mat[,iteration] = as.vector(weight_vec)
    }
    index<- i
  }
  
  return(weights_scaled_mat[,index])
} 

sigmoid <- function(z)
{
  g <-exp(-z)  + 1
  inv <- g^-1
  return(inv)
}
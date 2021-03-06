#' LMSquareLossL2
#'
#' find the optimal weight vector that minimizes the following cost function: ∑i=1^n L[w^T x_i, y_i] + penalty * ||w||, 
#' where L is the square loss.
#'
#' @param X.scaled.mat already has mean=0 and sd=1 for each of its columns
#' @param Y.vec numeric input label vetor [n]
#' @param penalty a vector of fold ids
#' @param opt.thresh positive numeric scalar
#' @param initial.weight.vec vector of weight
#'
#' @return optimal weight vector for the given penalty parameter
#' @export
#'
#' @examples
LMSquareLossL2 <- function(
  X.scaled.mat, 
  y.vec, 
  penalty,
  opt.thresh,
  initial.weight.vec
){
  if(nrow(X.scaled.mat) <= 0 || ncol(X.scaled.mat) <= 0 || nrow(y.vec) <= 0 || ncol(y.vec) <= 0)  
  {
    error("Feature matrix or Label vec has unexpected dimensions")
  }
  
  weight_vec <- initial.weight.vec
  index <- 0
  max.iterations <- 100
  for(i in 1:max.iterations)
  {
    gradient <- t(X.scaled.mat) %*% (as.matrix(X.scaled.mat) %*% weight_vec - y.vec)
    # changed n to i
    weight_vec <- weight_vec - (step.size) * ((gradient/i) + 2*penalty*weight_vec) 
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
    if(gradient > opt.thresh ){
      index <- i
    }
  }
  
  return(weights_scaled_mat[,index + 1])
} 
#' Title
#'
#' @param X.scaled.mat 
#' @param y.vec 
#' @param penalty 
#' @param opt.thresh 
#' @param initial.weight.vec 
#' ∑i=1^n L[w^T x_i, y_i] + penalty * ||w||
#'
#' @return
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
  #How to decide max.iterations?
  weight_vec <- initial.weight.vec
  index <- 0
  for(i in 1:max.iterations)
  {
    gradient <- t(X.mat) %*% (as.matrix(X.mat) %*% weight_vec - y.vec) 
    if(gradient > opt.thresh ){
      weight_vec <- weight_vec - (step.size) * ((gradient/n) + 2*penalty) 
      weights_scaled_mat[,iteration] = as.vector(weight_vec)
    }
    index <- i
  }
  
  return(weights_scaled_mat[,index])
} 
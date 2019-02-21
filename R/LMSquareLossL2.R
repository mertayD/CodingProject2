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
  weight_vec <- initial.weight.vec
  for(i in 1:max.iterations)
  {
    gradient <- t(X.mat) %*% (as.matrix(X.mat) %*% weight_vec - y.vec) 
    weight_vec <- weight_vec - (step.size) * ((gradient/n) + 2*penalty) 
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
  }
  
  return(weights_scaled_mat)
} 
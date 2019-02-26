#' LMSquareLossL2Penalties
#'
#' calculates a matrix of penalties, that gets used as the weight matrix
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n]
#' @param penalty.vec vector of decreasing penalty values
#'
#' @return W.mat (n_features+1 x n_penalties), weight matrix on original scale, that can be used to get predictions via cbind(1, X.mat) %*% W.mat (the first row of W.mat should be the bias/beta/intercept)
#' @export
#'
#' @examples
LMLogisticLossL2Penalties <- function(
  X.mat, 
  y.vec, 
  penalty.vec 
){
  mean.mat <- matrix(rep(colMeans(X.mat)),nrow = ncol(X.mat), ncol = 1)
  zero_mean <- sweep(X.mat,2, as.vector(mean.mat),"-")
  squared_zero_mean <- (zero_mean)^2
  squared.means = colMeans(squared_zero_mean)
  S.vec <- sqrt((squared.means))
  S.diagonal.mat <- diag(S.vec^-1, nrow = ncol(X.mat), ncol = ncol(X.mat))
  X.scaled <- sweep(zero_mean,2,S.vec,"/")
  
  i <- 0
  for(penalty in penalty.vec)
  {
    i <- i + 1
    w.mat[,i] <- LMLogisticLossL2(X.scaled,y.vec, penalty)
  }
  
  W.mat <- t(w.mat) %*% S.diagonal.mat
  return(W.mat)
}
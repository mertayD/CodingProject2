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
#'   data(spam, package = "ElemStatLearn")
#'   X.mat<-spam[1:10,-58]
#'   y.vec<-spam[1:10, 58]
#'   penalty.vec <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
#'   res <- LMLogisticLossIterations(X.mat, y.vec, penalty.vec)
LMLogisticLossL2Penalties <- function(
  X.mat, 
  y.vec, 
  penalty.vec 
){
  if(nrow(X.mat) <= 0 || ncol(X.mat) <= 0 || nrow(y.vec) <= 0 || ncol(y.vec) <= 0)  
  {
    error("Feature matrix or Label vec has unexpected dimensions")
  }
  
  mean.mat <- matrix(rep(colMeans(X.mat)),nrow = ncol(X.mat), ncol = 1)
  zero_mean <- sweep(X.mat,2, as.vector(mean.mat),"-")
  squared_zero_mean <- (zero_mean)^2
  squared.means = colMeans(squared_zero_mean)
  S.vec <- sqrt((squared.means))
  S.diagonal.mat <- diag(S.vec^-1, nrow = ncol(X.mat), ncol = ncol(X.mat))
  X.scaled <- sweep(zero_mean,2,S.vec,"/")
  opt.thresh <- 0.0001
  i <- 0
  initial.weight.vec <- rep(0,ncol(X.scaled))
  
  for(penalty in penalty.vec)
  {
    i <- i + 1
    if(i > 1){
      initial.weight.vec <- w.mat[,i-1]
    }
    w.mat[,i] <- LMLogisticLossL2(X.scaled,y.vec, penalty,opt.thresh,initial.weight.vec)
  }
  
  W.mat <- t(w.mat) %*% S.diagonal.mat
  b <- -t(w.mat) %*% S.diagonal.mat %*% mean.mat
  W.mat <- cbind(b, W.mat)
  return(W.mat)
}
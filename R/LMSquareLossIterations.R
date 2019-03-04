#' LMSquareLossIterations
#' 
#' computes square loss with respect to step size
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n]
#' @param max.iterations scalar integer, max number of iterations
#' @param step.size 
#'
#' @return  W.mat, matrix of weight vectors, one for each iteration, n_features+1 x max.iterations. (the first element of the weight vector should be the intercept term). Should be able to get a matrix of predictions via X.mat %*% W.mat
#' @export
#'
#' @examples
#'    library(codingProject2)
#'    data(ozone, package = "ElemStatLearn")
#'    X.mat<-ozone[1:10,-1]
#'    y.vec<-ozone[1:10, 1]
#'    max.iterations <- 50
#'    step.size <- 0.1
#'    res <- LMSquareLossIterations(X.mat, y.vec, 50, 0.1)
#'
LMSquareLossIterations <- function(
  X.mat, 
  y.vec, 
  max.iterations,
  step.size
){
  
  mean.mat <- matrix(rep(colMeans(X.mat)),nrow = ncol(X.mat), ncol = 1)
  zero_mean <- sweep(X.mat,2, as.vector(mean.mat),"-")
  squared_zero_mean <- (zero_mean)^2
  squared.means = colMeans(squared_zero_mean)
  S.vec <- sqrt((squared.means))
  S.diagonal.mat <- diag(S.vec^-1, nrow = ncol(X.mat), ncol = ncol(X.mat))
  X.scaled <- as.matrix(zero_mean) %*% S.diagonal.mat
  weights_scaled_mat <- matrix(0, ncol(X.scaled) ,max.iterations)
  weight_vec <- seq(0,0, length.out = ncol(X.scaled))
  weights_scaled_mat[,1] = as.vector(weight_vec)
  n <- nrow(X.scaled)
  
  for (iteration in 1:max.iterations) { 
    gradient <- 2*t(X.scaled) %*% (as.matrix(X.scaled) %*% weight_vec - y.vec)
    weight_vec <- weight_vec - (step.size)/n * gradient  
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
  }
  weights_mat <- t(weights_scaled_mat) %*% S.diagonal.mat
  b <- -weights_mat %*% mean.mat
  weights_mat <- cbind(b,weights_mat)
  return(t(weights_mat))
} 


#' Title
#'
#' @param X.mat 
#' @param y.vec 
#' @param max.iterations 
#' @param step.size 
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
  X.scaled <- sweep(zero_mean,2,S.vec,"/")
  X.gd <- cbind(rep(1,nrow(X.scaled)), X.scaled)
  weights_scaled_mat <- matrix(0, ncol(X.gd) ,max.iterations)
  weight_vec <- seq(0,0, length.out = ncol(X.gd))
  weights_scaled_mat[,1] = as.vector(weight_vec)
  n <- nrow(X.gd)
  
  for (iteration in 1:max.iterations) { 
    gradient <- t(X.gd) %*% (as.matrix(X.gd) %*% weight_vec - y.vec)
    weight_vec <- weight_vec - (step.size/n) * gradient  
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
  }
  w <- weights_scaled_mat[-1,]
  weights_mat <- t(w) %*% S.diagonal.mat
  b <- -t(w) %*% S.diagonal.mat %*% mean.mat
  weights_mat <- cbind(b,weights_mat)
  return(t(weights_mat))
} 


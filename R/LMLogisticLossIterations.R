#' LMLogisticLossIterations
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
#'    data(spam, package = "ElemStatLearn")
#'    X.mat<-spam[1:10,-58]
#'    y.vec<-spam[1:10, 58]
#'    max.iterations <- 50
#'    step.size <- 0.1
#'    k <- sigmoid(0)
#'    res <- LMLogisticLossIterations(X.mat, y.vec, 50, 0.1)
LMLogisticLossIterations <- function(
  X.mat, 
  y.vec, 
  max.iterations,
  step.size
){
  
  #Scaling of the Input Matrix
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
  
  for (iteration in 2:max.iterations) { 
    pred <- as.matrix(X.gd) %*% as.matrix(weight_vec)
    sig <- sigmoid(pred)
    cost <- sig - y.vec
    gradient <- t(X.gd) %*% cost
    weight_vec <- weight_vec - (step.size) * gradient  
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
  }
  w <- weights_scaled_mat[-1,]
  weights_mat <- t(w) %*% S.diagonal.mat
  b <- -t(w) %*% S.diagonal.mat %*% mean.mat
  weights_mat <- cbind(b,weights_mat)
  return(t(weights_mat))
  } 

#' Title
#'
#' @param z 
#'
#' @return g
#' @export
#'
#' @examples
sigmoid <- function(z)
{
  g <-exp(-z)  + 1
  inv <- g^-1
  return(inv)
}

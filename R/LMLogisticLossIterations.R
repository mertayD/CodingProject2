#' LMLogisticLossIterations
#'
#' calculates the LM Logistic Loss Iterations
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param Y.vec numeric input label vetor [n]
#' @param max.iterations scalar integer > 1, max number of iterations
#' @param step.size 
#'
#' @return W.mat matrix of weight vectors, one for each iteration, n_features+1 x max.iterations. (the first element of the weight vector should be the intercept term).
#' @export
#'
#' @examples
#'    library(codingProject2)
#'    data(spam, package = "ElemStatLearn")
#'    X.mat<-spam[1:200,-58]
#'    y.vec<-spam[1:100, 58]
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
  weights_scaled_mat <- matrix(0, ncol(X.scaled) ,max.iterations)
  weight_vec <- seq(0,0, length.out = ncol(X.scaled))
  weights_scaled_mat[,1] = as.vector(weight_vec)
  n <- nrow(X.scaled)
  
  for (iteration in 2:max.iterations) { 
    pred <- as.matrix(X.scaled) %*% as.matrix(weight_vec)
    sig <- sigmoid(pred)
    cost <- sig - y.vec
    gradient <- t(X.scaled) %*% cost
    weight_vec <- weight_vec - (step.size) * gradient  
    weights_scaled_mat[,iteration] = as.vector(weight_vec)
  }
  weights_mat <- t(weights_scaled_mat) %*% S.diagonal.mat
  b <- -weights_mat %*% mean.mat
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

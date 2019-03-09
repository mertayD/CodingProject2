# write tests for that R function, in tests/testthat/test-LMLogisticLossL2.R:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMLogisticLossL2")


test_that("LMLogisticLossL2 computes the right demensions", {
  data(spam, package = "ElemStatLearn")
  X.mat<-spam[1:100,-58]
  X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
  y.vec<-spam[1:100, 58]
  penalty <- 2
  opt.thresh <- 0.0001
  initial.weight.vec <- rep(0,ncol(X.scaled.mat))
      
  optimalWeightVector <- LMLogisticLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec)
  
  # vector
  expect_equal(nrow(res), 1)
})

test_that("LMLogisticLossL2 throws errors", {
  data(spam, package = "ElemStatLearn")
  X.mat<-spam[1:100,-58]
  X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
  y.vec<-spam[1:100, 58]
  penalty <- 2
  opt.thresh <- 0.0001
  initial.weight.vec <- rep(0,ncol(X.scaled.mat))
  
  expect_error(optimalWeightVector <- LMLogisticLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec), "Feature matrix or Label vec has unexpected dimensions")
})
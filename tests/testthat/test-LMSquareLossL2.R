# write tests for that R function, in tests/testthat/test-LMSquareLossL2:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMSquareLossL2")


test_that("LMSquareLossL2 computes the right demensions", {
  data(spam, package = "ElemStatLearn")
  X.mat<-ozone[1:20,-1]
  y.vec<-ozone[1:20, 1]
  X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
  penalty <- 2
  opt.thresh <- 0.0001
  initial.weight.vec <- rep(0,ncol(X.scaled.mat))
  
  optimalWeightVector <- LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec)
  
  # vector
  expect_equal(nrow(res), 1)
})

test_that("LMSquareLossL2 throws errors", {
  data(spam, package = "ElemStatLearn")
  X.mat<-ozone[1:20,-1]
  y.vec<-ozone[1:20, 1]
  X.scaled.mat<-scale(X.mat, center = TRUE, scale = TRUE)
  penalty <- 2
  opt.thresh <- 0.0001
  initial.weight.vec <- rep(0,ncol(X.scaled.mat))
  
  expect_error(optimalWeightVector <- LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec), "Add error")
})
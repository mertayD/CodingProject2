# write tests for that R function, in tests/testthat/test-LMLogisticLossIterations:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMSquareLossIterations")


test_that("LMSquareLossIterations computes the right demensions", {
  data(ozone, package = "ElemStatLearn")
  X.mat<-ozone[1:10,-1]
  y.vec<-ozone[1:10, 1]
  penalty.vec <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
  res <- LMSquareLossL2Penalties(X.mat, y.vec, penalty.vec)
  
  # n_features+1 x max.iterations
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 6)
})

test_that("LMSquareLossIterations throws errors", {

  data(ozone, package = "ElemStatLearn")
  X.mat<-ozone[1:10,-1]
  y.vec<-ozone[1:10, 1]
  penalty.vec <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
  res <- LMSquareLossL2Penalties(X.mat, y.vec, penalty.vec)
  
  expect_error(LMSquareLossL2Penalties(X.mat, y.vec, penalty.vec), "Add error")
})
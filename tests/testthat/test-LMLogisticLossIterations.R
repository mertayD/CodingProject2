# write tests for that R function, in tests/testthat/test-LMLogisticLossIterations:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMLogisticLossIterations")


test_that("LMLogisticLossIterations computes the right demensions", {
  data(spam, package = "ElemStatLearn")
  X.mat<-spam[1:10,-58]
  y.vec<-spam[1:10, 58]
  max.iterations <- 50
  step.size <- 0.1
  k <- sigmoid(0)
  res <- LMLogisticLossIterations(X.mat, y.vec, 50, 0.1)
  
  # n_features+1 x max.iterations
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), max.iterations)
})

test_that("LMLogisticLossIterations throws errors", {
  data(spam, package = "ElemStatLearn")
  X.mat<-spam[1:10,-58]
  y.vec<-spam[1:10, 58]
  max.iterations <- 50
  step.size <- 0.1
  k <- sigmoid(0)
  
  expect_error(LMLogisticLossIterations(X.mat, y.vec, 50, 0.1), "Feature matrix or Label vec has unexpected dimensions")
})
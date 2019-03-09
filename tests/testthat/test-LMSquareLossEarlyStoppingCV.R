# write tests for that R function, in tests/testthat/test-LMSquareLossEarlyStoppingCV:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMSquareLossEarlyStopping")


test_that("LMSquareLossEarlyStoppingCV computes the right demensions", {
  data(ozone, package = "ElemStatLearn")
  X.mat<-ozone[1:20,-1]
  y.vec<-ozone[1:20, 1]
  max.iterations <- 100
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  result <- LMSquareLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations)
  
  # n_features+1 x max.iterations
  expect_equal(nrow(result$mean.validation.loss), 2)
  expect_equal(ncol(result$mean.validation.loss), 20)
  expect_equal(nrow(result$weight.vec, 1))
})

test_that("LMSquareLossEarlyStoppingCV throws errors", {
  data(ozone, package = "ElemStatLearn")
  X.mat<-ozone[1:20,-1]
  y.vec<-ozone[1:20, 1]
  max.iterations <- 100
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  
  expect_error(LMSquareLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations), "Feature matrix or Label vec has unexpected dimensions")
  
})
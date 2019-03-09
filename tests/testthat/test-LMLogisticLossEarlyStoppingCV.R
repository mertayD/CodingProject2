# write tests for that R function, in tests/testthat/test-LMLogisticLossEarlyStoppingLMSquareLossEarlyStoppingCV:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMLogisticLossEarlyStoppingCV")


test_that("LMLogisticLossEarlyStoppingCV computes the right demensions", {
  data(SAheart , package = "ElemStatLearn")
  X.mat<-SAheart [1:50,-9]
  y.vec<-SAheart [1:50, 9]
  max.iterations <- 100
  fold.vec <- sample(rep(1:5, l=nrow(X.mat)))
  result <- LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations)
  
  # n_features+1 x max.iterations
  expect_equal(nrow(result$mean.validation.loss), 8)
  expect_equal(ncol(result$mean.validation.loss), 50)
  expect_equal(nrow(result$weight.vec, 1))
})

test_that("LMLogisticLossEarlyStoppingCV throws errors", {
  data(SAheart , package = "ElemStatLearn")
  X.mat<-SAheart [1:50,-9]
  y.vec<-SAheart [1:50, 9]
  max.iterations <- 100
  fold.vec <- sample(rep(1:5, l=nrow(X.mat)))
  
  expect_error(LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations), "Feature matrix or Label vec has unexpected dimensions")
 
})
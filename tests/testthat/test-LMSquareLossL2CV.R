# write tests for that R function, in tests/testthat/test-LMSquareLossL2CV:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(codingProject2)
library(testthat)
context("test-LMSquareLossL2CV")


test_that("LMSquareLossL2CV computes the right demensions", {
  data(ozone, package = "ElemStatLearn")
  X.mat<-ozone[1:20,-1]
  y.vec<-ozone[1:20, 1]
  max.iterations <- 100
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  result <- LMSquareLossL2CV(X.mat, y.vec, fold.vec, max.iterations)
  
  # n_features+1 x max.iterations
  expect_equal(nrow(result$mean.validation.loss), 2)
  expect_equal(ncol(result$mean.validation.loss), 20)
  expect_equal(nrow(result$weight.vec, 1))
})

test_that("LMSquareLossL2CV throws errors", {
  data(ozone, package = "ElemStatLearn")
  X.mat<-ozone[1:20,-1]
  y.vec<-ozone[1:20, 1]
  max.iterations <- 100
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  
  expect_error(LMSquareLossL2CV(X.mat, y.vec, fold.vec, max.iterations), "Add error")
  
})
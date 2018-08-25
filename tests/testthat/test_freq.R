library(epifield)
context("Freq function")

test_that("Freq produce result for global", {
  assign("v",c(1,1,3,2),envir=.GlobalEnv)
  expect_equal(mode(r <- freq(v)),"numeric")
})

# test_that("Freq produce result for local", {
#   v <- c(1,1,3,2)
#   expect_equal(mode(r <- freq(v)),"numeric")
# })

library(epifield)
context("Freq function")

test_that("Freq produce result for global", {
  epiglob.one <<- c(1,1,3,2)
  expect_equal(mode(freq(epiglob.one)),"numeric")
})

test_that("Freq produce result for local", {
   vtest <- c(1,1,3,2)
   freq(vtest)
   expect_equal(mode(freq(vtest)),"numeric")
})


library(epifield)
context("Freq function")

test_that("Freq produce result for global", {
  epiglob.one <<- c(1,1,3,2)
  expect_equal(freq(epiglob.one)$total,4)
})

test_that("Freq produce result for local", {
   vtest <- c(1,1,3,2)
   freq(vtest)
   expect_equal(freq(epiglob.one)$total,4)
})


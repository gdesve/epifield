library(epifield)
context("Clear function")



test_that("Clear memory must ask", {
  assign("global.test",5,envir = .GlobalEnv)
  clear("global.test")
  expect_equal(exists("global.test"), TRUE)
})

test_that("Clear memory ok if no ask", {
  assign("global.test",5,envir = .GlobalEnv)
  clear("global.test",noask=TRUE)
  expect_equal(exists("global.test"), FALSE)
})

test_that("Clear memory warning if frame var", {
  expect_output(clear("test$xxx",noask=TRUE), "xxx" )
})

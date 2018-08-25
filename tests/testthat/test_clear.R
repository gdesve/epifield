library(epifield)
context("Clear function")


test_that("Clear memory must not ask if one", {
  globalepif.one <<- 5
  clear("globalepif.one")
  expect_equal(exists("globalepif.one"), FALSE)
})

test_that("Clear memory must ask if partial match", {
  assign("globalepif.one",5,envir = .GlobalEnv)
  clear("globalepif")
  expect_equal(exists("globalepif.one"), TRUE)
})


test_that("Clear memory must ask if more than one", {
  assign("globalepif.one",5,envir = .GlobalEnv)
  assign("globalepif.two",5,envir = .GlobalEnv)
  clear("global")
  expect_equal(exists("globalepif.one"), TRUE)
})

test_that("Clear memory ok if no ask", {
  assign("globalepif.one",5,envir = .GlobalEnv)
  assign("globalepif.two",5,envir = .GlobalEnv)
  clear("globalepif",noask=TRUE)
  expect_equal(exists("globalepif.one"), FALSE)
})

test_that("Clear memory warning if frame var", {
  expect_output(clear("test$xxx",noask=TRUE), "xxx" )
})

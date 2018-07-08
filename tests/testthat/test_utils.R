library(epifield)
context("Utils function")

test_that("char count ok", {
  expect_equal(charcount(";","test;test;"),2)
})

test_that("no char count ok", {
  expect_equal(charcount(";","test test"),0)
})

test_that("File ext found", {
  expect_equal(file.ext("tira.dta"),"dta")
})

test_that("File name found", {
  expect_equal(file.name("c:/test/tira.dta"),"tira")
})

test_that("File ext found in path", {
  expect_equal(file.ext("users/test/data.tira.dta"),"dta")
})


test_that("Clear memory", {
  assign("global.test",5,envir = .GlobalEnv)
  expect_equal(exists("global.test"), TRUE)
  clear()
  expect_equal(exists("global.test"), FALSE)
})

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

test_that("File dta load ", {
  gastro5 <- use("gastro5.dta")
  expect_equal(is.data.frame(gastro5),TRUE)
  expect_equal(exists("gastro5"),TRUE)
})


test_that("right of text correct", {
  expect_equal(right("dummy_test",4),"test")
})


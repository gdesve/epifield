library(epifield)
context("Utils function")

test_that("char count ok", {
  expect_equal(charcount(";","test;test;"),2)
})

test_that("no char count ok", {
  expect_equal(charcount(";","test test"),0)
})

test_that("File ext found", {
<<<<<<< HEAD
  expect_equal(file.ext("tira.dta"),"dta")
})

test_that("File name found", {
  expect_equal(file.name("c:/test/tira.dta"),"tira")
})

test_that("File ext found in path", {
  expect_equal(file.ext("users/test/data.tira.dta"),"dta")
=======
  expect_equal(fileext("tira.dta"),"dta")
})

test_that("File ext found in path", {
  expect_equal(fileext("users/test/data.tira.dta"),"dta")
>>>>>>> 8a7a9292a498d611490a762ba841d2b053caee37
})

test_that("Clear memory", {
  assign("global.test",5,envir = .GlobalEnv)
  expect_equal(exists("global.test"), TRUE)
  clear()
  expect_equal(exists("global.test"), FALSE)
})

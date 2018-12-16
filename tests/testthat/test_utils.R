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
  gastro5 <- read("gastro5.dta")
  expect_equal(is.data.frame(gastro5),TRUE)
  expect_equal(exists("gastro5"),TRUE)
})

test_that("File csv load ", {
  tira <- read("tira.csv")
  expect_equal(is.data.frame(tira),TRUE)
  expect_equal(is.factor(tira$sex),FALSE)
})

test_that("File csv load with factor ", {
  tira <- read("tira.csv",factorise=TRUE)
  expect_equal(is.data.frame(tira),TRUE)
  expect_equal(is.factor(tira$sex),TRUE)
})

test_that("right of text correct", {
  expect_equal(right("dummy_test",4),"test")
})


test_that("getvar with vector", {
  r <- getvar(c(1,2,3))
  expect_equal(mode(r),"numeric")
  expect_equal(get_option("last_varname"),"c(1, 2, 3)")
  expect_equal(get_option("last_df"),"formula")
})


test_that("getvar with long syntax", {
  data(test)
  r <- getvar(test$age)
  expect_equal(mode(r),"numeric")
  expect_equal(get_option("last_varname"),"age")
  expect_equal(get_option("last_df"),"test")
})


test_that("getvar with long syntax quotted", {
  data(test)
  r <- getvar("test$age")
  expect_equal(mode(r),"numeric")
  expect_equal(get_option("last_varname"),"age")
  expect_equal(get_option("last_df"),"test")
})


test_that("getvar with short syntax", {
  data(test)
  r <- getvar(age)
  expect_equal(mode(r),"numeric")
  expect_equal(get_option("last_varname"),"age")
  expect_equal(get_option("last_df"),"test")
})

test_that("getvar with short syntax and two df", {
  data(test)
  data(gastro)
  expect_warning(r <- getvar(age))
  expect_equal(mode(r),"NULL")
  expect_equal(get_option("last_varname"),"")
  expect_equal(get_option("last_df"),"")
})

test_that("getvar with short syntax and two df and setdata", {
  data(test)
  data(gastro)
  setdata(test)
  r <- getvar(age)
  expect_equal(mode(r),"numeric")
  expect_equal(get_option("last_varname"),"age")
  expect_equal(get_option("last_df"),"test")
})


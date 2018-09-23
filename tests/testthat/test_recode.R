library(epifield)
context("Recode function")

test_that("Recode value by value with fullname", {
  data(test)
  recode.value(test$age,19,20)
  expect_true(any(test$age==20))
})

test_that("Recode value by value", {
  data(test)
  recode.value(age,19,20)
  expect_true(any(test$age==20))
})


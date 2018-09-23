library(epifield)
context("Data management function")

test_that("Rename dataframe var", {
  data(test)
  rename(age,AGE)
  expect_true(is.element("AGE",colnames(test) ) )
})

test_that("Drop dataframe var", {
  data(test)
  dropvar(age)
  expect_true( ! is.element("age",colnames(test) ) )
})


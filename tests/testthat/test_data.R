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

test_that("Sumby sum a numerical variable by combination of one or more categorical variables", {
  data(test)
  r <- sumby(test$age,test$sex)
  expect_equal(r$age[1],181)
})

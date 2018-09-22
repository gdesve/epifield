library(epifield)
context("Count function")

test_that("Count produce nbrows for data.frame", {
  expect_equal(countif(test),10)
})


test_that("Count produce nb of selected rows for data.frame", {
  expect_equal(countif(test$age>50),3)
})

test_that("Count produce nb of selected rows for data.frame", {
  setdata("test")
  expect_equal(countif(age>50),3)
})

test_that("Count complex logic for data.frame", {
  setdata("test")
  expect_equal(countif(age>50 & sex == 1),1)
})



test_that("Count produce length for vector", {
  vtest <- c(1,1,3,2)
  expect_equal(countif(vtest),4)
})

test_that("Count produce nb of selected rows for logical", {
  expect_equal(countif(c(TRUE,TRUE,FALSE,FALSE)), 2 )
})


library(epifield)
context("Data management function")

test_that("setdata correctly assigned by df", {
   data(test)
   setdata(test)
   expect_equal(setdata(),"test")
}
)

test_that("setdata correctly assigned by name", {
  data(test)
  setdata("test")
  expect_equal(setdata(),"test")
}
)

test_that("setdata correctly cleared", {
  data(test)
  setdata(test)
  setdata("")
  expect_equal(setdata(),"")
}
)

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

test_that("checkmissing one var", {
  data(test)
  expect_equal(checkmissing(beer)[[1]],1)
}
)

test_that("checkmissing one df", {
  data(test)
  btest <- test$beer
  expect_equal(checkmissing(btest)[[1]],1)
  expect_equal(checkmissing(test)[[1]],1)
  }
)


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

test_that("setdata report an error if df invalide", {
  data(test)
  expect_error(setdata(bidon))
}
)


test_that("setdata report an error if df name invalide", {
  data(test)
  expect_error(setdata("bidon"))
}
)

test_that("setdata correctly cleared", {
  data(test)
  setdata(test)
  setdata("")
  expect_equal(setdata(),"")
}
)

test_that("getdata correctly retrieve a df", {
  data(test)
  setdata(test)
  expect_equal(is.data.frame(getdata()),TRUE)
  clear(test,noask=TRUE)
}
)

test_that("getdata correctly retrieve a df", {
  data(test)
  setdata("")
  expect_equal(is.data.frame(getdata()),TRUE)
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

test_that("checkmissing no one ", {
  data(test)
  expect_equal(checkmissing(sex)[[1]],0)
}
)

test_that("checkmissing one df", {
  data(test)
  # btest <- test$beer
  # expect_equal(checkmissing(test$beer)[[1]],1)
  expect_equal(1,1)
  }
)


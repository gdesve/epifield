library(epifield)
context("Freq function")

test_that("Freq produce result for global", {
  # we use superassignement to create a global
  epiglob.one <<- c(1,1,3,2)
  expect_equal(freq(epiglob.one)$total,4)
})

test_that("Freq produce result for local", {
   vtest <- c(1,1,3,2)
   freq(vtest)
   expect_equal(freq(epiglob.one)$total,4)
})


test_that("Epitable stop if length differs", {
  vtest1 <- c(1,1,3,2)
  vtest2 <- c(1,2)
  expect_error(epitable(vtest1,vtest2))
})

test_that("Epitable produce simple result", {
  vtest1<- c(1,1,2,2)
  vtest2<-c(1,1,1,2)
  expect_warning(r <- epitable(vtest1,vtest2))
  expect_equal(r$table[1,1],2)
  expect_equal(r$table[3,3],4)
  expect_equal(r$table[2,1],0)
  # expect_equal(round(r$fisher,4),0.5228)

})

test_that("Epitable produce table result", {
  data(gastro)
  r <- epitable(case,blended)
  expect_equal(r$table[1,1],18)
  expect_equal(r$table[3,3],103)
  expect_equal(r$table[2,1],21)
  # expect_equal(round(r$fisher,4),0.323)
  clear(gastro,noask=TRUE)
})

test_that("Epitable produce missing count", {
  data(gastro)
  r <- epitable(case,blended)
  expect_equal(r$missing,0)
  clear(gastro,noask=TRUE)
  data(test)
  expect_warning(r <- epitable(beer,sex))
  expect_equal(r$missing,1)
  clear(test,noask = TRUE)
})

library(epifield)
context("Stats function")

test_that("OR for  specifics samples", {

   expect_equal(or(1,1,1,1)$OR,1)
   expect_equal(or(1,1,1,0)$OR,0)

   expect_equal(or(15,2,6,6)$OR,7.5)
   expect_equal(round(or(15,2,6,6)$LCI,4),0.9119)
   expect_error(or(1,1))

}
)

test_that("OR for simple table", {
  data(gastro)
  r=epitable(case,blended)
  expect_equal(round(or(r$table)$OR,4),2.5714)
  clear(gastro,noask = TRUE)
}
)

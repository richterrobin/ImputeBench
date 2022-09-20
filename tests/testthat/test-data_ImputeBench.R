

test_that("errors", {
  testthat::expect_error(
    data_ImputeBench(methods = c(1,2,3))
  )
})

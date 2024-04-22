test_that("open errors", {
  expect_error(open_rs_doc("x.R", col = 1))
})

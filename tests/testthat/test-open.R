test_that("open_rs_doc() errors in non-interactive sessions", {
  expect_error(open_rs_doc("x.R", col = 1))
})

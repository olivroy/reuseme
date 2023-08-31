test_that("open erros", {
  expect_error(open_rs_doc("x.R", col = 1))
})

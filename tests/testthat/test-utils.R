test_that("Windows is recognized correctly.", {
  skip_on_ci()
  skip_on_cran()
  skip_on_os("linux")
  skip_on_os("mac")
  expect_true(is_windows())
})

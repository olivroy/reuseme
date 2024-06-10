test_that("Windows is recognized correctly.", {
  skip_on_ci()
  skip_on_cran()
  skip_on_os("linux")
  skip_on_os("mac")
  expect_true(is_windows())
})

test_that("basename_remove_ext() works", {
  expect_equal(basename_remove_ext("data/xx.R"), "xx")
})

test_that("basename_null() works", {
  expect_null(basename_null(NULL))
})

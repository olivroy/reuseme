test_that("min/max/unique_named() return named output", {
  vec <- c("x" = 1, "y" = 2, "x" = 1)
  expect_named(max_named(vec))
  expect_named(min_named(vec))
  expect_named(unique_named(vec))
})

test_that("max_named() and unique_named() work with unnamed vectors", {
  vec <- c("x" = 1, "y" = 2, "x" = 1)
  unnamed_vec <- unname(vec)

  expect_equal(max_named(unnamed_vec), max(vec))
  expect_equal(unique_named(unnamed_vec), unique(vec))
})

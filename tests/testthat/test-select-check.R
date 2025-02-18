test_that("select_check() works", {
  expect_snapshot(error = TRUE, select_check(mtcars, vs2))
})


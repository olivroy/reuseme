test_that("proj_file() works", {
  expect_snapshot(
    error = TRUE,
    proj_file()
  )
})

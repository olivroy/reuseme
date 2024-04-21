test_that("file_outline() works", {
  my_test_file <- test_path("_ref", "my-analysis.R")
  expect_snapshot(
    file_outline(path = my_test_file)
  )
})

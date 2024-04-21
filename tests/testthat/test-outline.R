test_that("file_outline() works", {
  my_test_file <- test_path("_ref", "my-analysis.R")
  rlang::local_interactive(TRUE)
  expect_snapshot(
    file_outline(path = my_test_file)
  )
})

test_that("Other arguments work", {
  my_test_file <- test_path("_ref", "my-analysis.R")
  rlang::local_interactive(TRUE)
  expect_snapshot(
    error = F,
    file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE)
  )
})

test_that("file_outline() works", {
  my_test_file <- test_path("_ref", "my-analysis.R")
  rlang::local_interactive(TRUE)
  expect_snapshot(
    file_outline(path = my_test_file),
    transform = ~ stringr::str_remove(.x, "_")
  )
})

test_that("Other arguments work", {
  my_test_file <- test_path("_ref/my-analysis.R")
  rlang::local_interactive(TRUE)
  # Somehow on r cmd check, strips _ref -> ref?
  expect_snapshot(
    error = F,
    file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE),
    transform = ~ stringr::str_remove(.x, "_")
  )
})

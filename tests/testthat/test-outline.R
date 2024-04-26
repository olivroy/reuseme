test_that("file_outline() works", {
  my_test_file <- test_path("_ref", "my-analysis.R")
  rlang::local_interactive(TRUE)
  expect_snapshot(
    file_outline(path = my_test_file),
    transform = ~ sub("_", "", .x, fixed = TRUE)
  )
})

test_that("Other arguments work", {
  my_test_file <- test_path("_ref/my-analysis.R")
  rlang::local_interactive(TRUE)
  # Somehow on r cmd check, strips _ref -> ref?
  expect_snapshot(
    error = FALSE,
    file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE),
    transform = ~ sub("_", "", .x, fixed = TRUE)
  )
})

test_that("file_outline() is a data frame", {
  file <- fs::path_package("reuseme", "example-file", "outline-script.R")
  outline <- file_outline(path = file)
  expect_s3_class(outline, "tbl_df")
  expect_s3_class(outline, "reuseme_outline")
  expect_snapshot(
    outline,
    transform = ~ sub(" `.+` ", " `outline-script.R` ", .x)
  )
})

test_that("dir_outline() works with no error", {
  expect_no_error(dir_outline(regex_outline = ".+", path = test_path("_ref")))
})

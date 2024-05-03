test_that("file_outline() works", {
  my_test_files <- test_path("_ref", c("my-analysis.R", "my-analysis.md"))
  rlang::local_interactive(TRUE)
  expect_snapshot(
    file_outline(path = my_test_files, alpha = TRUE),
    transform = ~ sub("_?ref/", "", .x)
  )
})

test_that("Other arguments work", {
  my_test_file <- test_path("_ref/my-analysis.R")
  rlang::local_interactive(TRUE)
  # Somehow on r cmd check, strips _ref -> ref?
  # it is just RStudio vs non-Rstudio
  expect_snapshot(
    error = FALSE,
    file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE),
    transform = ~ sub("_", "", .x, fixed = TRUE)
  )
})

test_that("file_outline() is a data frame", {
  file <- fs::path_package("reuseme", "example-file", "outline-script.R")
  outline <- file_outline(path = file)
  expect_s3_class(outline, c("outline_report", "tbl_df"))
  expect_snapshot(
    outline,
    # simplify path display to avoid snapshot failures.
    transform = ~ sub(" `[^`]+` ", " `outline-script.R` ", .x)
  )
})

test_that("file_outline() with only title doesn't error", {
  expect_no_error({file_outline(path = test_path("_ref", "single-title.md"))})
})

test_that("file_outline() contains function calls", {
  file <- fs::path_package("reuseme", "example-file", "outline-script.R")
  outline <- file_outline(path = file)
  expect_contains(outline$outline_el, c("f_example", "f2_example"))
  # excludes commented things
  expect_no_match(outline$outline_el, "f_commented_example")
})

test_that("dir_outline() works with no error", {
  expect_no_error(dir_outline(regex_outline = ".+", path = test_path("_ref")))
})

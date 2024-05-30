test_that("file_outline() works", {
  my_test_files <- test_path("_outline", c("my-analysis.R", "my-analysis.md", "title.md", "titles.md"))
  rlang::local_interactive(TRUE)
  expect_snapshot(
    file_outline(path = my_test_files, alpha = TRUE),
    transform = ~ sub("_?ref/", "", .x)
  )
})

test_that("alpha and work_only arguments work", {
  my_test_file <- test_path("_outline/my-analysis.R")
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

test_that("pattern works as expected", {
  # TODO change tests for data frame size when stable (efficiency). As still debugging, better to keep all snapshots.
  # The idea is to show doc title + regex outline match when relevant
  file <- fs::path_package("reuseme", "example-file", "outline-script.R")
  expect_snapshot(file_outline(pattern = "not found", path = file))
  expect_snapshot(
    {
      file_outline("Viz", path = file)
    },
    transform = ~ sub(" `[^`]+` ", " `outline-script.R` ", .x)
  )
  # will work also if the regex is only in title
  expect_snapshot(
    {
      file_outline("Example for", path = file)
    },
    transform = ~ sub(" `[^`]+` ", " `outline-script.R` ", .x)
  )
})

test_that("file_outline() with only title doesn't error", {
  # broken by change to before_and_after_empty
  expect_no_error({
    file <- file_outline(path = test_path("_outline", "title.md"))
  })
  expect_equal(nrow(file), 1L)
  expect_no_error({
    file <- file_outline(path = test_path("_outline", "titles.md"))
  })
  # Number of items in titles.md
  expect_equal(nrow(file), 5L)
})

test_that("file_outline() contains function calls", {
  file <- fs::path_package("reuseme", "example-file", "outline-script.R")
  outline <- file_outline(path = file)
  expect_contains(outline$outline_el, c("f_example", "f2_example"))
  # excludes commented things
  expect_no_match(outline$outline_el, "f_commented_example")
})

test_that("dir_outline() works with no error", {
  expect_no_error(dir_outline(pattern = ".+", path = test_path("_outline")))
})

test_that("file_outline() works well with figure captions", {
  skip_if_not_installed("lightparser")
  expect_snapshot(
    file_outline(path = test_path("_outline", "quarto-caps.md")),
    transform = ~ sub(" `[^`]+` ", " `quarto-caps.md` ", .x)
  )
})

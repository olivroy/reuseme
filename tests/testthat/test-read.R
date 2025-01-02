test_that("read_clean() works", {
  skip_if_not_installed("readr")
  skip_if_not_installed("readxl")
  skip_if_not_installed("janitor")
  skip_if_not_installed("labelled")
  dat <- read_clean(system.file("extdata",  "challenge.csv", package = "readr"), show_col_types = FALSE)
  skip_if_not(ncol(dat) == 2, "readr example data changed")
  expect_equal(
    labelled::get_variable_labels(dat),
    list(
      x = NULL,
      y = NULL
    )
  )

  dat <- read_clean(system.file("extdata",  "challenge.csv", package = "readr"), col_names = c("A bizzare name", "weird #3"), show_col_types = FALSE)

  expect_equal(
    labelled::get_variable_labels(dat),
    list(
      `a_bizzare_name` = "A bizzare name",
      `weird_number_3` = "weird #3"
    )
  )
  expect_named(dat, janitor::make_clean_names(c("A bizzare name", "weird #3")))

})


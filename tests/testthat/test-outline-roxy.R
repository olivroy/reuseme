test_that("roxy tags are parsed properly + object names are correct", {
  skip_if_not_installed("roxygen2")
  skip_if_not_installed("tidyr")
  file_to_map <- testthat::test_path("_outline", "roxy-general.R")
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map, \(x) roxygen2::parse_file(x, env = NULL))

  example_parsed |>
    extract_roxygen_tag_location("details") |>
    expect_no_error()

  expect_no_error(res <- join_roxy_fun(example_parsed))

  expect_s3_class(res, "tbl_df")
  # verify if topic name is well done.
  res_order <- dplyr::arrange(res, line)
  expect_setequal(
    res$topic,
    c("f_to_be_index_in_outline()", "topic-name-to-include", NA_character_, "dataset")
  )
  # strip code from roxygen2 tag
  expect_contains(res$content, "`First code` to be included")
})

test_that("roxy tags don't error", {
  file_to_map <- testthat::test_path("_outline", "roxy-general2.R")
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map, \(x) roxygen2::parse_file(x, env = NULL))
  expect_no_error(join_roxy_fun(example_parsed))
})

test_that("multiple roxy tags don't error.", {
  file_to_map <- testthat::test_path("_outline", "roxy-section.R")
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map, \(x) roxygen2::parse_file(x, env = NULL))
  expect_no_error(join_roxy_fun(example_parsed))
})

test_that("file_outline() works outside RStudio)", {
  skip_on_cran()
  local_mocked_bindings(
    is_rstudio = function(...) FALSE
  )
  expect_no_warning(
    file_outline(path = testthat::test_path("_outline", "roxy-cli.R"))
  )
  expect_equal(
    nrow(file_outline(path = testthat::test_path("_outline", "roxy-cli.R"))),
    nrow(file_outline(path = normalizePath(testthat::test_path("_outline", "roxy-cli.R"))))
  )
})

test_that("cli escaping goes well", {
  rlang::local_interactive(FALSE)
  # The fact that I need to do this is bizzare.
  file_to_map <- normalizePath(testthat::test_path("_outline", "roxy-cli.R"))
  expect_snapshot(
   file_outline(path = file_to_map)
  )
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map, \(x) roxygen2::parse_file(x, env = NULL))
  expect_no_error(join_roxy_fun(example_parsed))
})

test_that("roxy tags are parsed properly", {
  skip_if_not_installed("roxygen2")
  skip_if_not_installed("tidyr")
  file_to_map <- testthat::test_path("_outline", "roxy-general.R")
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map,  roxygen2::parse_file)

  example_parsed |>
    extract_roxygen_tag_location("details") |>
    expect_no_error()

    expect_no_error(res <- join_roxy_fun(example_parsed))

    expect_s3_class(res, "tbl_df")
})

test_that("roxy tags don't error", {
  file_to_map <- testthat::test_path("_outline", "roxy-general2.R")
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map,  roxygen2::parse_file)
  expect_no_error(join_roxy_fun(example_parsed))

})

test_that("multiple roxy tags don't error.", {
  file_to_map <- testthat::test_path("_outline", "roxy-section.R")
  names(file_to_map) <- file_to_map
  example_parsed <- purrr::map(file_to_map,  roxygen2::parse_file)
  expect_no_error(join_roxy_fun(example_parsed))
})

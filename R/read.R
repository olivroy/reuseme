#' Read and clean names and set variable labels
#'
#' Reads data from file, but clean names with [janitor::make_clean_names()]
#'
#'
#' @inheritParams readr::read_csv
#' @export
#' @examples
#' read_clean(system.file("extdata",  "challenge.csv", package = "readr"))
#'
read_clean <- function(file, ...) {
  file_ext <- fs::path_ext(file)
  f <- switch(
    file_ext,
    "csv" = readr::read_csv,
    "xls" = ,
    "xlsx" = readxl::read_excel,
    cli::cli_abort("Extension not yet supported")
  )

  dat <- f(file, ...)



}

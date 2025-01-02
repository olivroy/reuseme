#' Read and clean names and set variable labels
#'
#' Reads data from file, but clean names with [janitor::make_clean_names()].
#'
#' It keeps the variable labels attributes to have improved support in the RStudio IDE,
#' with gt tables and with ggplots (>= 3.6.0).
#'
#'
#' @inheritParams readr::read_csv
#' @export
#' @examples
#' read_clean(system.file("extdata",  "challenge.csv", package = "readr"))
#'
read_clean <- function(file, ...) {
  file_ext <- fs::path_ext(file)
  rlang::check_installed(c("labelled", "readr", "readxl", "janitor"))
  f <- switch(
    file_ext,
    "csv" = readr::read_csv,
    "xls" = ,
    "xlsx" = readxl::read_excel,
    cli::cli_abort("Extension not yet supported")
  )

  dat <- f(file, ...)
  labels <- names(dat)

  dat <- janitor::clean_names(dat)

  if (!identical(names(dat), labels)) {
    dat <- labelled::set_variable_labels(dat, .labels = labels)
  }

  dat


}

# Test for roxygen parsing for no error ----------------------------------------
## Use {.file tests/testthat/_ref/test-roxygen.R} for output testing -----------
# Dump of other things to test for expect_no_error (not necessary to verify)
# Mostly cases inspired by testing in the wild.

#' Title
#'
#' @examples
#' # Commented code not included
#'
#' ggplot2::ggplot(mtcars) +
#'   labs(
#'    title = "A title not to be included"
#'  )
#' @export
#' @family a family to include
f_to_be_index_in_outline <- function() {

}

#' An S3 method not to be include
#'
#' content
#' @export
f_not_to_index.xml <- function() {

}

# Keep this line last: content to test for new roxygen output should be put in test-roxygen.R

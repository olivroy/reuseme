# Test for roxygen parsing for no error ----------------------------------------
## Use {.file tests/testthat/_outline/roxy-general.R} for output testing -----------
# Dump of other things to test for expect_no_error (not necessary to verify)
# Mostly cases inspired by testing in the wild.

#' Title with `_things`
#'
#' @description
#' ## Section
#'
#'
#' ```r
#' # Commented code not include
#' title = "TITLE NOT INCLUDED"
#' ```
#'
#' @examples
#' # Commented code not included
#'
#' ggplot2::ggplot(mtcars) +
#'   labs(
#'     title = "A title not to be included"
#'   )
#' @export
#' @family a family to include
#' if {
#' }
f_to_be_index_in_outline <- function() {

}

#' An S3 method not to be include
#'
#' content
#' @export
f_not_to_index.xml <- function() {

}

#' A
#'
#' Very short title.
#' @export
f_not_to_index <- function() {

}
NULL

#' A t
#'
#' desc
#'
#' @section section AA REQUIRED ELEMENT:
#'
#' * DO this
#'
NULL

# Keep this line last: content to test for new roxygen output should be put in roxy-general.R

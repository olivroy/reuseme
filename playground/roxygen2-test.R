# pkgload::load_all()

#' Extract roxygen tag
#'
#' Tell me what this does
#'
#' # Section to extract
#'
#' Well this is a section
#'
#' @md
#' @param file A file
#' @returns A named list with name = file:line, and element is the section title
extract_roxygen_section_location <- function(file = reuseme::active_rs_doc()) {
  aa <- roxygen2::parse_file(file = file)

 # roxygen2::block_get_tag_value(aa[[1]], tag = "title")
}
x <- extract_roxygen_section_location()
.Last.value

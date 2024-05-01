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
extract_roxygen_section_location <- function(file = "R/outline-criteria.R") {
  aa <- roxygen2::parse_file(file)

 pos <- roxygen2::block_get_tags(aa[[1]], tags = "title")
 nam <- paste0(pos[[1]]$file, ":", pos[[1]]$line)
 val <- pos[[1]]$val
 rlang::set_names(val, nam)
}
x <- extract_roxygen_section_location()
.Last.value

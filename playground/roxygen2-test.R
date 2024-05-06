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
extract_roxygen_tag_location <- function(file = c("R/proj-list.R"), tag) {
  aa <- roxygen2::parse_file(file)

 pos <- purrr::map(aa, \(x) roxygen2::block_get_tags(x, tags = tag)) |>
   purrr::list_flatten()
  nam <- purrr::map(pos, \(x) paste0(x$file, ":", x$line))
 val <- purrr::map_chr(pos, "val")
  rlang::set_names(val, nam)
}
titles_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "title")) |>
  unlist()

section_list <-  purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "section")) |>
  unlist()

roxygen2::parse_file("R/rename-files.R")

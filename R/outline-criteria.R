#' outline_criteria
#'
#' * is test title
#' * is a todo item
#' * is_roxygen_line
#' * is_tab_title
#'
#' @noRd
o_is_roxygen_comment <- function(x, file_ext = NULL) {

  if (!is.null(file_ext)) {
    is_r_file <- tolower(file_ext) == "r"
  } else {
    is_r_file <- TRUE
  }

  if (!any(is_r_file)) {
    return(FALSE)
  }

  ifelse(is_r_file, stringr::str_starts(x, "#'\\s"), FALSE)
}

o_is_todo_fixme <- function(x) {
  stringr::str_detect(x, "TODO[^\\.]\\:?|FIXME|BOOK|(?<!\")WORK[^I``]") &
    !o_is_test_that(x) &
    !stringr::str_starts(x, "\\s*\"") &
    !stringr::str_detect(x, "extract_tag_in_text") &
    !o_is_roxygen_comment(x) & # don't put these tags in documentation :)
    !stringr::str_detect(x, "str_detect|str_remove|str_extract|regex_outline\\s|use_todo|,\\stodo\\)|TODO\\.R|TODO file|@param") &
    !stringr::str_detect(x, "[:upper:]\"") # eliminate false positives
}

o_is_work_item <- function(x) {
  o_is_todo_fixme(x) & stringr::str_detect(x, "(?<!\")# WORK")
}
o_is_test_that <- function(x) {
  # avoid generic like f works.
  stringr::str_detect(x, "test_that\\(\"")
}
o_is_generic_test <- function(x) {
  stringr::str_detect(x, "works\"|correctly\"|properly\"|expected\"")
}

# Returns table or plot titles.
o_is_object_title <- function(x) {
  stringr::str_detect(x, "(?<!\")title = [\"']|tab_header") &
    stringr::str_detect(x, "\\[", negate = TRUE) &
    !stringr::str_detect(x, "Foo|test|Title|TITLE|Subtitle|[eE]xample|x\\.x\\.|man_get_image_tab|table's")


}
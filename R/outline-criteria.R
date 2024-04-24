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

o_is_section_title <- function(x) {
  section_title <- stringr::str_detect(x, "^\\#+\\s")
  uninteresting_headings <- "(Tidy\\s?T(uesday|emplate)|Readme|Wrangle)$"
  section_title & !stringr::str_detect(x, uninteresting_headings)
}

# Add variable to outline data frame --------------------

define_outline_criteria <- function(.data, print_todo) {
  dplyr::mutate(
    .data,
    file_ext = fs::path_ext(file),
    is_md = file_ext %in% c("qmd", "md", "Rmd", "Rmarkdown"),
    # is_function_def = stringr::str_detect(file, "[(\\<\\-)=]\\s?function\\("),
    file_with_many_functions = stringr::str_detect(file, "Rprofile|r-profile"), # TODO readjust that... package for example
    is_test_file = stringr::str_detect(file, "tests/testthat"),
    # Problematic when looking inside functions
    # maybe force no leading space.
    is_cli_info = stringr::str_detect(content, "(^cli_)|([^_]cli_)") &
      !stringr::str_detect(content, "(warn|abort|div)|c\\(\\s?$") &
      !stringr::str_detect(content, "paste") &
      !stringr::str_detect(file, "outline.R") &
      !stringr::str_detect(file, "_snaps") &
      !stringr::str_detect(content, "\\^"), # Detect UI messages and remove them
    is_cli_info = is_cli_info & !file_with_many_functions, # TODO strip is_cli_info in Package? only valid for EDA
    is_doc_title = stringr::str_detect(content, "(?<!-)title\\:") & !stringr::str_detect(content, "Ttitle|Subtitle"),
    is_chunk_cap = stringr::str_detect(content, "\\#\\|.*cap:"),
    # deal with chunk cap
    # FIXME try to detect all the chunk caption, but would have to figure out the end of it maybe {.pkg lightparser}.
    is_chunk_cap_next = is_chunk_cap & stringr::str_detect(content, "\\s*[\\>\\|]$"),
    is_chunk_cap = dplyr::case_when(
      is_chunk_cap & is_chunk_cap_next ~ FALSE,
      dplyr::lag(is_chunk_cap_next, default = FALSE) ~ TRUE,
      .default = is_chunk_cap
    ),
    is_chunk_cap_next = is_chunk_cap,
    is_test_name = is_test_file & o_is_test_that(content) & !o_is_generic_test(content),
    is_section_title = o_is_section_title(content),
    is_tab_or_plot_title = o_is_object_title(content) & !is_section_title,
    is_a_comment_or_code = stringr::str_detect(content, "!=|\\|\\>|\\(\\.*\\)"),
    is_todo_fixme = print_todo & o_is_todo_fixme(content) & !o_is_roxygen_comment(content, file_ext) & !stringr::str_detect(file, "_snaps"),
    is_section_title_source = stringr::str_detect(content, "\\#+\\s") & stringr::str_detect(content, "[-\\=]{3,}") & !stringr::str_detect(content, "\\@param") & stringr::str_starts(content, "\\s*\"", negate = TRUE),
    before_and_after_empty = line_id == 1 | !nzchar(dplyr::lead(content)) & !nzchar(dplyr::lag(content)),
    n_leading_hash = nchar(stringr::str_extract(content, "\\#+")),
    n_leading_hash = dplyr::coalesce(n_leading_hash, 0),
    is_second_level_heading_or_more = (is_section_title_source | is_section_title) & n_leading_hash > 1,
    is_cross_ref = stringr::str_detect(content, "docs_links?\\(") & !stringr::str_detect(content, "@param|\\{\\.")
  )

}

# it is {.file R/outline-criteria.R} ------

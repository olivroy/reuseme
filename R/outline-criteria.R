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

  ifelse(rep(is_r_file, length.out = length(x)), stringr::str_starts(x, "#'\\s"), FALSE)
}

o_is_todo_fixme <- function(x) {
  has_todo <- stringr::str_detect(x, "(?<!\"#\\s)(TODO[^\\.]\\:?|FIXME|BOOK|(?<!\")WORK[^I``])") &
    !o_is_test_that(x) &
    !stringr::str_starts(x, "\\s*\"\\s*") &
    !stringr::str_detect(x, "extract_tag_in_text") &
    !o_is_roxygen_comment(x) & # don't put these tags in documentation :)
    !stringr::str_detect(x, "grepl?\\(|g?sub\\(|str_detect|str_remove|str_extract|regex_outline\\s|use_todo|,\\stodo\\)|TODO\\.R|TODO file|@param") &
    !stringr::str_detect(x, "[:upper:]\"|[:upper:]{4,} item") # eliminate false positives

  has_todo & !stringr::str_detect(x, "\".*(TODO|FIXME|WORK)") # remove some true negs for now.
}

o_is_work_item <- function(x) {
  o_is_todo_fixme(x) & stringr::str_detect(x, "(?<!\")# WORK")
}

o_is_test_that <- function(x) {
  # avoid generic like f works.
  stringr::str_detect(x, "(?<!['\"])test_that\\(\"")
}

o_is_generic_test <- function(x) {
  stringr::str_detect(x, "works\"|correctly\"|properly\"|expected\"")
}

# Returns table or plot titles.
o_is_object_title <- function(x) {
  stringr::str_detect(x, "(?<!(\"|abbr\\s))title = [\"']|tab_header") &
    !grepl("[", x, fixed = TRUE) &
    !stringr::str_detect(x, "Foo|test|Title|TITLE|Subtitle|[eE]xample|x\\.x\\.|man_get_image_tab|table's")
}

o_is_section_title <- function(x) {
  section_title <- stringr::str_detect(x, "^\\s{0,4}\\#+\\s+(?!\\#)|^\\#'\\s\\#+\\s") # remove commented  add roxygen
  uninteresting_headings <- "(Tidy\\s?T(uesday|emplate)|Readme|Wrangle)$"
  section_title & !stringr::str_detect(x, uninteresting_headings) & !o_is_todo_fixme(x)
}

# Add variable to outline data frame --------------------

define_outline_criteria <- function(.data, print_todo) {
  x <- .data
  x$file_ext <- fs::path_ext(x$file)
  x$is_md <- x$file_ext %in% c("qmd", "md", "Rmd", "Rmarkdown")
  x$is_test_file <- grepl("tests/testthat", x$file, fixed = TRUE)
  x$is_snap_file <- grepl("_snaps", x$file, fixed = TRUE)

  x <- dplyr::mutate(
    x,
    # Problematic when looking inside functions
    # maybe force no leading space.
    # TODO strip is_cli_info in Package? only valid for EDA
    is_cli_info = stringr::str_detect(content, "(^cli_)|([^_]cli_)") &
      stringr::str_detect(content, "\\([\"']") &
      !is_snap_file &
      !stringr::str_detect(content, "(text|inform|bullets|warn|abort|div)|c\\(\\s?$") &
      !grepl("paste", content, fixed = TRUE) &
      !grepl("outline.R", file, fixed = TRUE) &
      !grepl("^", content, fixed = TRUE), # Detect UI messages and remove them
    is_doc_title = stringr::str_detect(content, "(?<![-(#\\s?)])title\\:") & !stringr::str_detect(content, "Ttitle|Subtitle"),
    is_chunk_cap = stringr::str_detect(content, "\\#\\|.*(cap|title):"),
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
    is_section_title_source = o_is_section_title(content) & stringr::str_detect(content, "[-\\=]{3,}|^\\#'") & !stringr::str_detect(content, "\\@param"),
    is_tab_or_plot_title = o_is_object_title(content) & !is_section_title,
    is_a_comment_or_code = stringr::str_detect(content, "!=|\\|\\>|\\(\\.*\\)"),
    is_todo_fixme = print_todo & o_is_todo_fixme(content) & !o_is_roxygen_comment(content, file_ext) & !is_snap_file,
    before_and_after_empty = line_id == 1 | !nzchar(dplyr::lead(content)) & !nzchar(dplyr::lag(content)),
    n_leading_hash = nchar(stringr::str_extract(content, "\\#+")),
    n_leading_hash = dplyr::coalesce(n_leading_hash, 0),
    is_second_level_heading_or_more = (is_section_title_source | is_section_title) & n_leading_hash > 1,
    is_cross_ref = stringr::str_detect(content, "docs_links?\\(") & !stringr::str_detect(content, "@param|\\{\\."),
    is_function_def = grepl("<- function(", content, fixed = TRUE) & !stringr::str_starts(content, "\\s*#")
  )
  x
}

# it is {.file R/outline.R} ------

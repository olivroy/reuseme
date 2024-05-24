# Extract standard package version from NEWS.md
extract_pkg_version <- function(x, is_news, is_heading) {
  y <- rep(NA_character_, length.out = length(x))
  if (!any(is_news)) {
    return(y)
  }
  reg_pkg_version <- .standard_regexps()$valid_package_version

  y[is_news & is_heading] <- stringr::str_extract(x[is_news & is_heading], reg_pkg_version)
  y
}

#' outline_criteria
#'
#' * is test title
#' * is a todo item
#' * is_roxygen_line
#' * is_tab_plot_title
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

  ifelse(rep(is_r_file, length.out = length(x)), stringr::str_detect(x, "^#'\\s|^#'$"), FALSE)
}

o_is_todo_fixme <- function(x) {
  has_todo <- stringr::str_detect(x, "(?<!\"#\\s)(TODO[^\\.]\\:?|FIXME|BOOK|(?<!\")WORK[^I``])")

  if (!any(has_todo)) {
    return(has_todo)
  }
  # only check for potential candidates
  p <- which(has_todo)
  candidates <- x[has_todo]
  # Eliminate candidates
  has_todo[p] <-
    !o_is_test_that(candidates) &
      !stringr::str_starts(candidates, "\\s*\"\\s*") &
      !stringr::str_detect(candidates, "extract_tag_in_text") &
      !o_is_roxygen_comment(candidates) & # don't put these tags in documentation :)
      !stringr::str_detect(candidates, "grepl?\\(|g?sub\\(|str_detect|str_remove|str_extract|use_todo|,\\stodo\\)|TODO\\.R|TODO file|@param") &
      !stringr::str_detect(candidates, "[:upper:]\"|[:upper:]{4,10} item") & # eliminate false positives
      !stringr::str_detect(candidates, "\".{0,100}(TODO|FIXME|WORK)") # remove some true negs for now.
  has_todo
}

o_is_work_item <- function(x) {
  res <- stringr::str_detect(x, "(?<!\")# WORK")
  if (!any(res)) {
    return(res)
  }
  res[which(res)] <- o_is_todo_fixme(x[which(res)])
  res
}

o_is_test_that <- function(x) {
  # avoid generic like f works.
  stringr::str_detect(x, "(?<!['\"])test_that\\(\"")
}

o_is_generic_test <- function(x) {
  stringr::str_detect(x, "works\"|correctly\"|properly\"|expected\"")
}

# Returns table or plot titles.
o_is_tab_plot_title <- function(x) {
  stringr::str_detect(x, "(?<!(\"|abbr\\s))title = [\"']|tab_header") &
    !grepl("[", x, fixed = TRUE) &
    !stringr::str_detect(x, "Foo|test|Title|TITLE|Subtitle|[eE]xample|x\\.x\\.|man_get_image_tab|table's")
}

o_is_section_title <- function(x) {
  is_section_title <- stringr::str_detect(x, "^\\s{0,4}\\#+\\s+(?!\\#)|^\\#'\\s\\#+\\s") # remove commented  add roxygen
  if (!any(is_section_title)) {
    return(is_section_title)
  }

  uninteresting_headings <- "(Tidy\\s?T(uesday|emplate)|Readme|Wrangle|Devel)$|error=TRUE|url\\{|Error before installation"
  # potential section titles
  p_s_title <- which(is_section_title)
  is_section_title[p_s_title] <- !stringr::str_detect(x[p_s_title], uninteresting_headings) & !o_is_todo_fixme(x[p_s_title]) & !o_is_commented_code(x[p_s_title])
  is_section_title
}

o_is_commented_code <- function(x) {
  stringr::str_detect(x, "#.+\\(.+\\=.+[\\),\"']$")
}

o_is_cli_info <- function(x, is_snap_file = FALSE, file = "file") {
  has_cli <- grepl("cli_", x, fixed = TRUE)

  if (!any(has_cli)) {
    return(has_cli)
  }
  # Potential cli
  p_cli <- which(has_cli)

  has_cli[p_cli] <-
    stringr::str_detect(x[p_cli], "\\([\"']") &
      !is_snap_file[p_cli] &
      !grepl("outline.R", file[p_cli], fixed = TRUE) &
      !stringr::str_detect(x[p_cli], "(text|inform|bullets|warn|abort|div)|\"cli|c\\(\\s?$") &
      !grepl("paste", x[p_cli], fixed = TRUE) &
      !grepl("^", x[p_cli], fixed = TRUE) # Detect UI messages and remove them
  has_cli
}

# Add variable to outline data frame --------------------

define_outline_criteria <- function(.data, print_todo) {
  x <- .data
  x$file_ext <- s_file_ext(x$file)
  x$is_md <- x$file_ext %in% c("qmd", "md", "Rmd", "Rmarkdown")
  x$is_news <- x$is_md & grepl("NEWS.md", x$file, fixed = TRUE)
  x$is_roxygen_comment <- o_is_roxygen_comment(x$content, x$file_ext)
  x$is_md <- (x$is_md | x$is_roxygen_comment) & !x$is_news # treating news and other md files differently.
  x$is_test_file <- grepl("tests/testthat", x$file, fixed = TRUE)
  x$is_snap_file <- grepl("_snaps", x$file, fixed = TRUE)
  if (any(x$is_roxygen_comment)) {
    rlang::check_installed(c("roxygen2", "tidyr"), "to create roxygen2 comments outline.")
    files_with_roxy_comments <- unique(x[x$is_roxygen_comment, "file", drop = TRUE])
    files_with_roxy_comments <- rlang::set_names(files_with_roxy_comments, files_with_roxy_comments)
    parsed_files <- suppressMessages( # roxygen2 messages
      # TRICK purrr::safely creates an error object, while possible is better.
      purrr::map(files_with_roxy_comments, purrr::possibly(roxygen2::parse_file))
    )
    # if roxygen2 cannot parse a file, let's just forget about it.
    unparsed_files <- files_with_roxy_comments[which(is.null(parsed_files))]
    # browser()
    if (length(unparsed_files) > 0) {
      cli::cli_inform("Could not parse roxygen comments in {.file {unparsed_files}}")
    }
    parsed_files <- purrr::compact(parsed_files)
    outline_roxy <- join_roxy_fun(parsed_files)
  } else {
    outline_roxy <- NULL
  }

  x <- dplyr::mutate(
    x |> dplyr::filter(!is_roxygen_comment) |> dplyr::bind_rows(outline_roxy),
    # Problematic when looking inside functions
    # maybe force no leading space.
    # TODO strip is_cli_info in Package? only valid for EDA (currently not showcased..)
    is_cli_info = o_is_cli_info(content, is_snap_file, file),
    is_doc_title = stringr::str_detect(content, "(?<![-(#\\s?)_])title\\:.{4,100}") & !stringr::str_detect(content, "Ttitle|Subtitle") &
      !stringr::str_detect(dplyr::lag(content, default = "nothing to detect"), "```yaml"),
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
    pkg_version = extract_pkg_version(content, is_news, is_section_title),
    is_section_title_source = o_is_section_title(content) & stringr::str_detect(content, "[-\\=]{3,}|^\\#'") & !stringr::str_detect(content, "\\@param"),
    is_tab_or_plot_title = o_is_tab_plot_title(content) & !is_section_title,
    is_a_comment_or_code = stringr::str_detect(content, "!=|\\|\\>|\\(\\.*\\)"),
    is_todo_fixme = print_todo & o_is_todo_fixme(content) & !o_is_roxygen_comment(content, file_ext) & !is_snap_file,
    n_leading_hash = nchar(stringr::str_extract(content, "\\#+")),
    n_leading_hash = dplyr::coalesce(n_leading_hash, 0),
    is_second_level_heading_or_more = (is_section_title_source | is_section_title) & n_leading_hash > 1,
    is_cross_ref = stringr::str_detect(content, "docs_links?\\(") & !stringr::str_detect(content, "@param|\\{\\."),
    is_function_def = grepl("<- function(", content, fixed = TRUE) & !stringr::str_starts(content, "\\s*#")
  )
  if (!"before_and_after_empty" %in% names(x)) {
    x$before_and_after_empty <- NA
  }
  x <- dplyr::mutate(
    x,
    before_and_after_empty = ifelse(
      !is.na(before_and_after_empty),
      line == 1 | !nzchar(dplyr::lead(content, default = "")) & !nzchar(dplyr::lag(content)),
      before_and_after_empty
      ),
    .by = "file"
  )
  #browser()
  x
}

# it is {.file R/outline.R} ------

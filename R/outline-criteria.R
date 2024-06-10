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

o_is_notebook <- function(x, file, file_ext, line) {
  # Like roxy comments and first line = --, 2nd title.
  # x$is_notebook <- grepl("notebook.*\\.R", x$file)
  # Detect #' ---
  any_notebooks <- grep("^#' ---", x[line == 1 & file_ext == "R"], fixed = FALSE)
  if (length(any_notebooks) > 0L) {
    is_notebook <- file %in% file[line == 1 & file_ext == "R"][any_notebooks]
  } else {
    is_notebook <- FALSE
  }
  is_notebook
}

o_is_todo_fixme <- function(x, is_roxygen_comment = FALSE) {
  has_todo <- !is_roxygen_comment &
    grepl("(?<!\"#\\s)(TODO[^\\.]\\:?|FIXME\\s|BOOK|(?<!\")WORK[^I``])", x, perl = TRUE)
  if (!any(has_todo)) {
    return(has_todo)
  }
  if (length(is_roxygen_comment) == 1) {
    is_roxygen_comment <- rep(is_roxygen_comment, length.out = length(x))
  }
  # only check for potential candidates
  p <- which(has_todo)
  candidates <- x[has_todo]
  # Eliminate candidates
  has_todo[p] <-
    !o_is_test_name(candidates) &
    !stringr::str_starts(candidates, "\\s*\"\\s*") &
    !grepl("extract_tag_in_text", candidates, fixed = TRUE) &
    !is_roxygen_comment[p] & # don't put these tags in documentation :)
    !stringr::str_detect(candidates, "grepl?\\(|g?sub\\(|str_detect|str_remove|str_extract|use_todo|,\\stodo\\)|TODO\\.R|TODO file|@param") &
    !stringr::str_detect(candidates, "[:upper:]\"|[:upper:]{4,10} item") & # eliminate false positives
    !stringr::str_detect(candidates, "\".{0,100}(TODO|FIXME|WORK)") # remove some true negs for now.
  has_todo
}

o_is_test_name <- function(x) {
  # avoid generic like f works.
  potential_test <- grepl("{", x, fixed = TRUE)
  if (!any(potential_test)) {
    return(potential_test)
  }
  test_that_name_regex <- "(?<!['\"])test_that\\(\"(?!\")"
  if (any(grepl('describe(', x, fixed = TRUE))) {
    potential_test & (grepl(test_that_name_regex, x, perl = TRUE) |
      grepl("(?<!['\"])describe\\(\"(?!\")", x, perl = TRUE))
  } else {
    potential_test & grepl(test_that_name_regex, x, perl = TRUE)
  }
}

o_is_generic_test <- function(x) {
  # remove " detection to avoid discovering snapshot files.
  stringr::str_detect(x, "works|correctly|properly|expected")
}

# Returns table or plot titles.
o_is_tab_plot_title <- function(x) {
  generic_title_regex <- paste(
    "Foo|test|Title|TITLE|Subtitle|[eE]xample|x\\.x\\.",
    "man_get_image_tab|table's|list\\(|bla\"|\", \"|use_.+\\(",
    sep = "|"
  )

  stringr::str_detect(x, "(?<!(_|:|\"|abbr\\s))title = [\"'](?![\"'])[^\"]{5,}") &
    !grepl("[", x, fixed = TRUE) &
    !grepl(generic_title_regex, x) &
    !stringr::str_ends(x, "\\(|\"\",?|'',?|\\(") &
    # not guide_*(title = ) as this is not a title.
    !stringr::str_detect(x, "expect_error|header\\(\\)|```\\{|guide_")
}

o_is_section_title <- function(x, is_roxygen_comment = FALSE, is_todo_fixme = FALSE) {
  is_section_title <- !is_roxygen_comment & !is_todo_fixme & stringr::str_detect(x, "^\\s{0,4}\\#+\\s+(?!\\#)") & !is_roxygen_comment # remove commented  add roxygen
  if (!any(is_section_title)) {
    return(is_section_title)
  }
  if (length(is_roxygen_comment) == 1) {
    rep(is_roxygen_comment, length.out = length(is_section_title))
  }
  uninteresting_headings <- paste(
    "(Tidy\\s?T(uesday|emplate)|Readme|Wrangle|Devel)$|error=TRUE",
    "url\\{|Error before installation|unreleased|Function ID$|Function Introduced",
    "Usage$|Examples?$|Newly broken$|Newly fixed$|In both$|Installation$|MIT License|nocov|With cli$|sourceCode|Detect #'",
    sep = "|"
  )
  # potential section titles
  p_s_title <- which(is_section_title)
  is_section_title[p_s_title] <-
    !grepl(uninteresting_headings, x[p_s_title]) &
      # to exclude md tables from outline
      stringr::str_count(x[p_s_title], "\\|") < 4
  is_section_title
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
  x$is_md <- x$is_md & !x$is_news # treating news and other md files differently.
  x$is_test_file <- grepl("tests/testthat/test", x$file, fixed = TRUE)
  x$is_snap_file <- grepl("_snaps", x$file, fixed = TRUE)
  x$is_roxygen_comment <- o_is_roxygen_comment(x$content, x$file_ext)
  if (any(x$is_roxygen_comment)) {
    # detect knitr notebooks
    x$is_notebook <- o_is_notebook(x = x$content, x$file, x$file_ext, x$line)
    x$content[x$is_notebook] <- sub("^#'\\s?", "", x$content[x$is_notebook])
    x$is_md <- (x$is_md | x$is_roxygen_comment | x$is_notebook) & !x$is_news # treating news and other md files differently.
    x$is_roxygen_comment <- x$is_roxygen_comment & !x$is_notebook
    # TODO extract title in roxy comments (@title too.L)
    # x <- dplyr::mutate(
    #   x,
    #   # Remove any files that contain noRd roxy tag to avoid false positive (this limitation can be overcome if / when I use roxygen2 parser)
    #   is_roxygen_comment = is_roxygen_comment & !any(startsWith(content, "#' @noRd")),
    #   .by = file
    # )
    # x$is_object_title <- x$is_roxygen_comment & (x$line == 1 | dplyr::lag(x$ccontent, "Nothing") %in% c("", " ", "  ") & dplyr::lead(x$ccontent) %in% c("#'", "#'  ", "#'   "))
  } else {
    x$is_notebook <- FALSE
  }
  x <- dplyr::mutate(
    x,
    # Problematic when looking inside functions
    # maybe force no leading space.
    # TODO strip is_cli_info in Package? only valid for EDA
    is_cli_info = o_is_cli_info(content, is_snap_file, file),
    is_doc_title = stringr::str_detect(content, "(?<![-(#\\s?)_[:alpha:]'\"])title\\:.{4,100}") &
      !stringr::str_detect(content, "No Description|Ttitle|Subtitle|[Tt]est$|\\\\n") & line < 50 &
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
    is_test_name = is_test_file & o_is_test_name(content) & !o_is_generic_test(content),
    is_todo_fixme = print_todo & o_is_todo_fixme(content, is_roxygen_comment) & !is_snap_file,
    is_section_title = o_is_section_title(content, is_roxygen_comment, is_todo_fixme),
    pkg_version = extract_pkg_version(content, is_news, is_section_title),
    is_section_title_source = is_section_title &
      stringr::str_detect(content, "[-\\=]{3,}|^\\#'") &
      stringr::str_detect(content, "[:alpha:]"),
    n_leading_hash = nchar(stringr::str_extract(content, "\\#+")),
    n_leading_hash = dplyr::coalesce(n_leading_hash, 0),
    # Make sure everything is second level in revdep/.
    n_leading_hash = n_leading_hash + grepl("revdep/", file, fixed = TRUE),
    is_second_level_heading_or_more = (is_section_title_source | is_section_title) & n_leading_hash > 1,
    is_cross_ref = stringr::str_detect(content, "docs_links?\\(.") & !stringr::str_detect(content, "@param|\\{\\."),
    is_function_def = grepl("<- function(", content, fixed = TRUE) & !stringr::str_starts(content, "\\s*#"),
    is_tab_or_plot_title = o_is_tab_plot_title(content) & !is_section_title & !is_function_def,
  )
  x <- dplyr::mutate(
    x,
    before_and_after_empty =
      line == 1 | !nzchar(dplyr::lead(content, default = "")) & !nzchar(dplyr::lag(content)),
    .by = "file"
  )
  x
}

# it is {.file R/outline.R} ------

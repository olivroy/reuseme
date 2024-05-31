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
o_is_roxygen_comment <- function(x, file_ext = NULL, is_test_file = FALSE) {
  if (!is.null(file_ext)) {
    is_r_file <- tolower(file_ext) == "r" & !is_test_file
  } else {
    is_r_file <- !is_test_file
  }

  if (!any(is_r_file)) {
    return(FALSE)
  }

  ifelse(rep(is_r_file, length.out = length(x)), stringr::str_detect(x, "^#'\\s|^#'$"), FALSE)
}

o_is_todo_fixme <- function(x) {
  has_todo <- stringr::str_detect(x, "(?<!\"#\\s)(TODO[^\\.]\\:?|FIXME\\s|BOOK|(?<!\")WORK[^I``])")

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
  potential_test <- grepl("{", x, fixed = TRUE)
  if (!any(potential_test)) {
    return(potential_test)
  }
  potential_test & stringr::str_detect(x, "(?<!['\"])test_that\\(\"(?!\")")
}

o_is_generic_test <- function(x) {
  stringr::str_detect(x, "works\"|correctly\"|properly\"|expected\"")
}

# Returns table or plot titles.
o_is_tab_plot_title <- function(x) {
  stringr::str_detect(x, "(?<!(_|\"|abbr\\s))title = [\"'](?![\"'])[^\"]{5,}") &
    !grepl("[", x, fixed = TRUE) &
    !stringr::str_detect(x, "Foo|test|Title|TITLE|Subtitle|[eE]xample|x\\.x\\.|man_get_image_tab|table's|list\\(|bla\"|\", \"|use_.+\\(") &
    !stringr::str_ends(x, "\\(|\"\",?|'',?|\\(") &
    # not guide_*(title = ) as this is not a title.
    !stringr::str_detect(x, "expect_error|header\\(\\)|```\\{|guide_")
}

o_is_section_title <- function(x, roxy_section = FALSE) {
  is_section_title <- stringr::str_detect(x, "^\\s{0,4}\\#+\\s+(?!\\#)|^\\#'\\s\\#+\\s") | roxy_section # remove commented  add roxygen
  if (!any(is_section_title)) {
    return(is_section_title)
  }
  if (roxy_section) {
    x <- stringr::str_remove(x, ":$")
  }
  uninteresting_headings <- "(Tidy\\s?T(uesday|emplate)|Readme|Wrangle|Devel)$|error=TRUE|url\\{|Error before installation|unreleased|^Function ID$|^Function Introduced$|^Examples$|Newly broken$|Newly fixed$|In both$|Installation$|MIT License|nocov|With cli$|sourceCode"
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

define_outline_criteria <- function(.data, exclude_todos, dir_common) {
  x <- .data
  x$file_ext <- s_file_ext(x$file)
  x$is_md <- x$file_ext %in% c("qmd", "md", "Rmd", "Rmarkdown")
  x$is_news <- x$is_md & grepl("NEWS.md", x$file, fixed = TRUE)
  x$is_test_file <- grepl("tests/testthat", x$file, fixed = TRUE)
  # TODO Would have to look for notebooks that don't contain notebook in their name
  # Like roxy comments and first line = --, 2nd title.
  x$is_notebook <- grepl("notebook.*\\.R", x$file)
  x$is_roxygen_comment <- o_is_roxygen_comment(x$content, x$file_ext, x$is_test_file | x$is_notebook)
  x$content[x$is_notebook] <- sub("^#' ", "", x$content[x$is_notebook])
  x$is_md <- (x$is_md | x$is_roxygen_comment) & !x$is_news # treating news and other md files differently.
  x$is_snap_file <- grepl("_snaps", x$file, fixed = TRUE)

  should_parse_roxy_comments <-
    !isFALSE(getOption("reuseme.roxy_parse", default = TRUE)) && # will not parse if option is set to FALSE
      any(x$is_roxygen_comment)
  if (should_parse_roxy_comments) {
    if (!is.null(dir_common)) {
      withr::local_dir(dir_common)
    }
    rlang::check_installed(c("roxygen2", "tidyr"), "to create roxygen2 comments outline.")
    files_with_roxy_comments <- unique(x[x$is_roxygen_comment, "file", drop = TRUE])
    files_with_roxy_comments <- rlang::set_names(files_with_roxy_comments, files_with_roxy_comments)
    # roxygen2 messages
    # TRICK purrr::safely creates an error object, while possible is better.
    # Suppresss roxygen2 message, suppress callr output, suppress asciicast warnings.
    invisible(capture.output(parsed_files <- purrr::map(files_with_roxy_comments, purrr::possibly(\(x) roxygen2::parse_file(x, env = NULL))))) |> suppressMessages() |> suppressWarnings()
    # if roxygen2 cannot parse a file, let's just forget about it.
    unparsed_files <- files_with_roxy_comments[which(is.null(parsed_files))]
    # browser()
    if (length(unparsed_files) > 0) {
      cli::cli_inform("Could not parse roxygen comments in {.file {unparsed_files}}")
    }
    parsed_files <- purrr::compact(parsed_files)
    processed_roxy <- join_roxy_fun(parsed_files)
    outline_roxy <- define_outline_criteria_roxy(processed_roxy)
  } else {
    outline_roxy <- NULL
  }

  x <- dplyr::mutate(
    x |> dplyr::filter(!is_roxygen_comment),
    # Problematic when looking inside functions
    # maybe force no leading space.
    # TODO strip is_cli_info in Package? only valid for EDA (currently not showcased..)
    is_cli_info = o_is_cli_info(content, is_snap_file, file),
    # TODO long enough to be meanignful?
    is_doc_title = stringr::str_detect(content, "(?<![-(#\\s?)_])title\\:.{4,100}") & !stringr::str_detect(content, "No Description|Ttitle|Subtitle|[Tt]est$|\\\\n") & line < 50 &
      !stringr::str_detect(dplyr::lag(content, default = "nothing to detect"), "```yaml"),
    is_chunk_cap = stringr::str_detect(content, "\\#\\|.*(cap|title):|```\\{r.*cap\\s?\\="),
    is_test_name = is_test_file & o_is_test_that(content) & !o_is_generic_test(content),
    is_section_title = o_is_section_title(content),
    pkg_version = extract_pkg_version(content, is_news, is_section_title),
    is_section_title_source = o_is_section_title(content) & stringr::str_detect(content, "[-\\=]{3,}|^\\#'") & stringr::str_detect(content, "[:alpha:]"),
    is_function_def = grepl("<- function(", content, fixed = TRUE) & !stringr::str_starts(content, "\\s*#"),
    is_tab_or_plot_title = o_is_tab_plot_title(content) & !is_section_title & !is_function_def,
    # roxygen2 title block
    is_object_title = FALSE,
    tag = NA_character_,
    topic = NA_character_,
    is_todo_fixme = !exclude_todos & o_is_todo_fixme(content) & !o_is_roxygen_comment(content, file_ext) & !is_snap_file,
    n_leading_hash = nchar(stringr::str_extract(content, "\\#+(?!\\|)")), # don't count hashpipe
    n_leading_hash = dplyr::coalesce(n_leading_hash, 0),
    # Make sure everything is second level in revdep/.
    n_leading_hash = n_leading_hash + grepl("revdep/", file, fixed = TRUE),
    is_second_level_heading_or_more = (is_section_title_source | is_section_title) & n_leading_hash > 1,
    is_cross_ref = stringr::str_detect(content, "docs_links?\\(") & !stringr::str_detect(content, "@param|\\{\\."),
  )
  x <- dplyr::mutate(
    x,
    before_and_after_empty =
      line == 1 | !nzchar(dplyr::lead(content, default = "")) & !nzchar(dplyr::lag(content)),
    .by = "file"
  )
  # browser()
  res <- dplyr::bind_rows(x, outline_roxy)
  res <- dplyr::filter(
    res,
    content != "NULL"
  )
  res <- dplyr::arrange(res, .data$file, .data$line)
}


define_outline_criteria_roxy <- function(x) {
  # TODO merge with define_outline_criteria
  x$is_md <- x$tag %in% c("subsection", "details", "description", "section")
  # short topics are likely placeholders.
  x$is_object_title <- x$tag == "title" & nchar(x$content) > 4
  x$line <- as.integer(x$line)
  x$file_ext <- "R"
  # x$content <- paste0("#' ", x$content) # maybe not?
  x$is_news <- FALSE
  x$is_roxygen_comment <- TRUE
  x$is_test_file <- FALSE
  x$is_snap_file <- FALSE
  x$before_and_after_empty <- TRUE
  x$is_section_title <- (x$tag %in% c("section", "subsection") & stringr::str_ends(x$content, ":") & o_is_section_title(x$content, roxy_section = TRUE)) |
    (x$tag %in% c("details", "description") & stringr::str_detect(x$content, "#\\s"))
  x$is_section_title_source <- x$is_section_title
  x$is_chunk_cap <- FALSE
  x$is_test_name <- FALSE
  x$pkg_version <- NA_character_
  # a family or concept can be seen as a plot subtitle?
  x$is_tab_or_plot_title <- x$tag %in% c("family", "concept")
  x$is_cli_info <- FALSE
  x$is_cross_ref <- FALSE
  x$is_function_def <- FALSE
  x$is_todo_fixme <- FALSE
  x$is_doc_title <- x$line == 1 & x$tag == "title"
  x$n_leading_hash <- nchar(stringr::str_extract(x$content, "\\#+"))
  x$n_leading_hash <- dplyr::case_when(
    x$n_leading_hash > 0 ~ x$n_leading_hash,
    x$tag == "section" & x$is_section_title_source ~ 1,
    x$tag == "subsection" & x$is_section_title_source ~ 2,
    .default = 0
  )
  x$content <- dplyr::case_when(
    !x$is_section_title ~ x$content,
    x$tag == "section" ~ paste0("# ", stringr::str_remove(x$content, ":")),
    x$tag == "subsection" ~ paste0("## ", stringr::str_remove(x$content, ":")),
    .default = x$content
  )
  x$is_second_level_heading_or_more <- ((x$is_section_title_source | x$is_section_title) & x$n_leading_hash > 1)
  # x$has_inline_markup <- FALSE # let's not mess with inline markup
  x
}

# it is {.file R/outline.R} or {.file R/outline-roxy.R} ------

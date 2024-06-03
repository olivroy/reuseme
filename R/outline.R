## `proj_outline()` -------------
#' Print interactive outline of file sections
#'
#' @description
#' THe outline functions return a data frame that contains details of file location.
#'
#' It also includes a print method that will provide a console output that will include [clickable hyperlinks](https://cli.r-lib.org/reference/links.html)
#' in RStudio (or if your terminal supports it). It works with both (qR)md and R files.
#'
#' Outline elements include
#' * Any code section
#' * function definition (not shown in console by default)
#' * `TODO` items
#' * Parse cli hyperlinks
#' * Plot or table titles
#' * Figures caption in Quarto documents
#' * test names
#' * Indicator of recent modification
#' * Colored output for
#' * URL and gh issue detection and link creation.
#'
#'
#' If `work_only` is set to `TRUE`, the function will only return outline of the `# WORK` comment
#' in `path`. `work_only = TRUE` will have an effect on `pattern`.
#'
#' By default
#' * `file_outline()` prints the outline the [active document][active_rs_doc()] if in RStudio
#' * `proj_outline()` prints the outline of the [active project][usethis::proj_get()] if in RStudio
#' * `dir_outline()` prints the outline of the [active working directory][getwd()] by default or
#'
#' @details
#' `proj_outline()` and `dir_outline()` are wrapper of `file_outline()`.
#'
#' The parser is very opinionated and based on lightparser, roxygen2 and regexps.
#'
#' Will show TODO items and will offer a link to [mark them as
#' complete][complete_todo()].
#'
#'
#' @param path,proj A character vector of file paths, a [project][proj_list()].
#'   Defaults to active file, project or directory. `rstudioapi::documentPath()`
#' @param pattern A string or regex to search for in the outline. If
#'   specified, will search only for elements matching this regular expression.
#'   The print method will show the document title for context. Previously `regex_outline`.
#' @param exclude_tests Should tests be displayed? (Not sure if this argument will stay, Will have to think)
#' @param work_only If `TRUE`, (the default), will only show you work items
#'   first. Set to `FALSE` if you want to see the full outline. `WORK` will
#'   combine with `pattern`
#' @param exclude_todos Should include TODOs in the file outline? If `FALSE`, will
#'   print a less verbose output with sections.
#' @param alpha Whether to show in alphabetical order
#' @param dir_tree If `TRUE`, will print the [fs::dir_tree()] or non-R files in
#'   the directory
#' @param recent_only Show outline for recent files
#' @param dir_common (Do not use it)
#' @param print_todo `r lifecycle::badge("deprecated")`. Use `exclude_todos` instead.
#' @inheritParams fs::dir_ls
#' @returns A `outline_report` object that contains the information. Inherits
#' `tbl_df`.
#'
#' A symbol will show for recently modified files.
#' @name outline
#' @examples
#' file <- fs::path_package("reuseme", "example-file", "outline-script.R")
#' file_outline(path = file)
#'
#' # Remove todo items
#' file_outline(path = file, exclude_todos = TRUE, alpha = TRUE)
#'
#' # interact with data frame
#' file_outline(path = file) |> dplyr::as_tibble()
#'
#' @examplesIf interactive()
#' # These all work on the active file / project or directory.
#'
#' file_outline()
#' proj_outline()
#' dir_outline()
#' # Like proj_switch(), proj_outline() accepts a project
#'
NULL
## `file_outline()` ------
#' @export
#' @rdname outline
file_outline <- function(pattern = NULL,
                         path = active_rs_doc(),
                         work_only = TRUE,
                         alpha = FALSE,
                         dir_common = NULL,
                         exclude_todos = FALSE,
                         recent_only = FALSE,
                         print_todo = deprecated()) {
  # To contribute to this function, take a look at .github/CONTRIBUTING.md

  if (length(path) == 1L && rlang::is_interactive() && is_rstudio()) {
    is_active_doc <- identical(path, active_rs_doc())
  } else {
    is_active_doc <- FALSE
  }
  if (length(path) == 0L) {
    cli::cli_abort("No path specified.")
  }

  if (lifecycle::is_present(print_todo)) {
    exclude_todos <- !print_todo
    lifecycle::deprecate_warn(
      when = "0.0.3",
      what = "file_outline(print_todo)",
      with = "file_outline(exclude_todos)"
    )
  }

  # active_rs_doc() returns `NULL` if the active document is unsaved.
  is_saved_doc <- !is.null(path)
  if (is_saved_doc) {
    # little help temporarily
    if (any(stringr::str_detect(path, "~/rrr|~/Requests"))) {
      path <- fs::path_expand_r(path)
      tryCatch(
        fs::path_real(path = path),
        error = function(e) {
          cli::cli_abort("Error trying to normalize paths.")
        }
      )
    } else {
      fs::path_real(path = path) # verify if the files exist
    }

    # if (rstudioapi::rstudio)
    dir_common <- get_dir_common_outline(dir_common, path)

    path <- stringr::str_sort(path)
    file_content <- rlang::set_names(path)
    # Not warn
    file_content <- purrr::map(file_content, function(x) readLines(fs::path_real(x), encoding = "UTF-8", warn = FALSE))
    # Combine everything into a tibble that contains file, line, content
    file_content <- purrr::map(file_content, function(x) tibble::enframe(x, name = "line", value = "content"))
    file_content <- dplyr::bind_rows(file_content, .id = "file")
  } else {
    file_content <-
      purrr::map(
        .x = list("unsaved-doc.R" = rstudioapi::getSourceEditorContext()$contents),
        .f = function(x) tibble::enframe(x, name = "line", value = "content")
      )
    file_content <- dplyr::bind_rows(file_content, .id = "file")
  }

  in_active_project <- suppressMessages(
    tryCatch(
      identical(suppressWarnings(proj_get2()), dir_common),
      error = function(e) FALSE
    )
  )
  # After this point we have validated that paths exist.

  # Handle differently if in showing work items only
  if (work_only) {
    # Detect special tag for work item
    should_show_only_work_items <- any(o_is_work_item(file_content$content))
    # Check if there are work items in files
  } else {
    should_show_only_work_items <- FALSE
  }

  # Will append Work to the regexp outline, if it was provided.
  # otherwise sets pattern to anything
  if (should_show_only_work_items) {
    cli::cli_inform("Use {.code work_only = FALSE} to display the full file/project outline.")
    pattern <- paste(c("work\\s", pattern), collapse = "|")
  } else {
    pattern <- pattern %||% ".+"
  }

  check_string(pattern, arg = "You may have specified path Internal error")

  file_sections00 <- define_outline_criteria(file_content, exclude_todos = exclude_todos, dir_common)

  # filter for interesting items.
  file_sections0 <- keep_outline_element(file_sections00)

  if (!grepl(".+", pattern, fixed = TRUE)) {
    # keep files where pattern was detected (not the generic .+)
    file_sections0 <- dplyr::filter(
      file_sections0,
      any(grepl(pattern, content, ignore.case = TRUE)),
      .by = "file"
    )
  }

  if (nrow(file_sections0) == 0) {
    if (is_active_doc && !identical(pattern, ".+")) {
      msg <- c("{.code pattern = {.val {pattern}}} did not return any results looking in the active document.",
        "i" = "Did you mean to use {.run reuseme::file_outline(path = {.str {pattern}})}?"
      )
    } else if (!identical(pattern, ".+")) {
      msg <- c(
        "{.code pattern = {.val {pattern}}} did not return any results looking in {length(path)} file{?s}.",
        "i" = "Run {.run [{.fn proj_file}](reuseme::proj_file(\"{pattern}\"))} to search in file names too."
      )
    } else {
      msg <- "Empty outline."
    }
    cli::cli_inform(msg)
    return(invisible())
  }
  if (exists("link_doc")) {
    file_sections0$content <- purrr::map_chr(file_sections0$content, link_doc)
  }
  # File outline ===================
  # strip outline element .data$outline = `# Section 1` becomes `Section 1`
  file_sections1 <- display_outline_element(file_sections0, dir_common)

  # Create hyperlink in console
  file_sections <- construct_outline_link(file_sections1, is_saved_doc, dir_common, pattern)

  if (alpha) {
    # remove inline markup first before sorting alphabetically
    file_sections <- arrange_outline(file_sections)
  }

  # take most important first!
  file_sections <-
    dplyr::arrange(
      file_sections,
      grepl("README|NEWS|vignettes", file)
    )
  file_sections$recent_only <- recent_only

  if (anyDuplicated(file_sections$outline_el) > 0L) {
    file_sections <- scrub_duplicate_outline(file_sections)
  }
  file_sections <- dplyr::relocate(
    file_sections,
    "outline_el", "title_el", "title_el_line",
    .after = content
  )
  class(file_sections) <- c("outline_report", class(file_sections))

  file_sections
}
#' @rdname outline
#' @export
proj_outline <- function(pattern = NULL, proj = proj_get2(), work_only = TRUE, exclude_tests = FALSE, exclude_todos = FALSE, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE) {
  is_active_proj <- identical(proj, proj_get2())

  if (is_active_proj && !is.null(pattern) && pattern %in% names(proj_list())) {
    # only throw warning if proj is supplied
    cli::cli_warn(c(
      "You specified {.arg pattern} = {.val {pattern}}",
      i = "Did you mean to use `proj = {.val {pattern}}?"
    ))
  }

  if (is_active_proj) {
    return(dir_outline(
      pattern = pattern,
      work_only = work_only,
      exclude_tests = exclude_tests,
      exclude_todos = exclude_todos,
      dir_tree = dir_tree,
      alpha = alpha,
      recent_only = recent_only,
      recurse = TRUE
    ))
  }

  if (fs::dir_exists(proj)) {
    if (!is_active_proj) {
      cli::cli_warn("Use {.fn dir_outline} for that.")
    }
    proj_dir <- proj
  } else { # when referring to a project by name.
    proj_dir <- proj_list(proj)
  }

  if (!rlang::has_length(proj_dir, 1)) {
    cli::cli_abort("Cannot process more than one project/directory at once.")
  }

  if (!fs::dir_exists(proj_dir)) {
    cli::cli_abort("Internal errors due to path processing. Maybe use fs's path processing ")
  }

  if (!is_proj(proj_dir)) {
    cli::cli_abort("Not in a project. Use {.fn reuseme::dir_outline} instead.")
  }

  is_active_proj <- identical(fs::path(proj_dir), proj_get2())
  if (!is_active_proj) {
    # Add an outline that enables switching projects if searching outside
    cli::cli_h1(paste0("{.run [", proj, "](reuseme::proj_switch('", proj, "'))}"))
  }

  dir_outline(
    pattern = pattern,
    path = proj_dir,
    work_only = work_only,
    exclude_tests = exclude_tests,
    exclude_todos = exclude_todos,
    dir_tree = dir_tree,
    alpha = alpha,
    recurse = TRUE
  )
}
#' @rdname outline
#' @export
dir_outline <- function(pattern = NULL, path = ".", work_only = TRUE, exclude_tests = FALSE, exclude_todos = FALSE, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE, recurse = FALSE) {
  dir <- fs::path_real(path)
  file_exts <- c("R", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")

  file_list_to_outline <- fs::dir_ls(
    path = dir,
    type = "file",
    glob = file_exts_regex,
    recurse = recurse
  )
  file_list_to_outline <- fs::path_filter(
    file_list_to_outline,
    regexp = "vignette-dump|renv/",
    invert = TRUE
  )

  if (recurse && !identical(Sys.getenv("TESTTHAT"), "true")) {
    # Remove examples from outline and test example files to avoid clutter
    # examples don't help understand a project.
    file_list_to_outline <- exclude_example_files(file_list_to_outline)
  }
  if (exclude_tests) {
    file_list_to_outline <- fs::path_filter(
      file_list_to_outline,
      regexp = "tests/",
      invert = TRUE
    )
  }
  if (any(grepl("README.Rmd", file_list_to_outline))) {
    file_list_to_outline <- stringr::str_subset(file_list_to_outline, "README.md", negate = TRUE)
  }
  if (dir_tree) {
    cli::cli_h2("Here are the non-R files of {.file {path}}")

    fs::dir_tree(
      path = dir,
      regexp = "R/.+|qmd|Rmd|_files|~\\$|*.Rd|_snaps|tests/testthat.R|Rmarkdown|docs/",
      recurse = recurse,
      invert = TRUE
    )
  }
  file_outline(path = file_list_to_outline, pattern = pattern, exclude_todos = exclude_todos, work_only = work_only, dir_common = dir, alpha = alpha, recent_only = recent_only)
}

exclude_example_files <- function(path) {
  # styler tests examples may not work..

  regexp_exclude <- paste(
    "vignettes/test/", # test vignettes
    "LICENSE.md", # avoid indexing this.
    "tests/(performance-monitor|gt-examples/|testthat/scope-|testthat/assets|testthat/_outline|testthat/testTestWithFailure|testthat/testTest/|testthat/test-parallel/|testthat/test-list-reporter/)", # example files in usethis, pkgdown, reuseme, devtools, etc.
    "inst/((rmarkdown/)?templates/|example-file/|examples/rmd/|tutorials/)", # license templates in usethis
    "revdep/", # likely don't need to outline revdep/, use dir_outline() to find something in revdep/
    "themes/hugo-theme-console/", # protect blogdown
    "vignettes/.+\\.R$", # generated files
    "RcppExports.R",
    "pkgdown/assets",
    sep = "|"
  )

  fs::path_filter(
    path,
    regexp = regexp_exclude,
    invert = TRUE
  )
}
# Print method -------------------

#' @export
print.outline_report <- function(x, ...) {
  # https://github.com/r-lib/cli/issues/607
  # Make output faster with cli!
  withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))

  if (sum(!x$is_function_def) == 0) {
    cli::cli_inform("Empty {.help [outline](reuseme::file_outline)}.")
    return(invisible(x))
  }
  custom_styling <- c(
    # 500 is the max path length.
    # green todo
    "(?<!(complete_todo.{1,500}))(?<![\\w'])([:upper:]{4,5})\\:?($|\\s)" = "\\{.field \\2\\} ", # put/work todo as emphasis
    "\\{\\.pkg \\{\\(?pkg\\$package\\}\\}\\)?" = "{.pkg {package}}", # until complex markup is resolved.
    # Workaround r-lib/cli#693
    "\\[([[:alpha:]\\s]+)\\]\\s" = "{cli::bg_white(cli::col_black('\\1'))} "
  )
  file_sections <- dplyr::as_tibble(x)
  recent_only <- x$recent_only[1]
  file_sections$link_rs_api <- stringr::str_replace_all(file_sections$link_rs_api, custom_styling)

  summary_links_files <- file_sections |>
    dplyr::filter(!is_function_def) |>
    dplyr::summarise(
      first_line = unique(title_el_line),
      first_line_el = unique(title_el),
      link = list(rlang::set_names(
        link_rs_api,
        purrr::map_chr(
          paste0("{.file ", file, ":", line, "}"),
          cli::format_inline
        )
      )),
      .by = c("file_hl", "file")
    )
  if (anyDuplicated(summary_links_files$file) > 0) {
    cli::cli_abort(c("Expected each file to be listed once."), .internal = TRUE)
  }
  # At the moment, especially `active_rs_doc()`, we are relying on path inconsistencies by RStudio.
  in_vscode <- FALSE # to do create it. # Sys.getenv("TERM_PROGRAM") == "vscode" when in vscode!
  if (in_vscode) {
    which_detect <- stringr::str_which(tolower(summary_links_files$file_hl), "file://\\~|file://c\\:", negate = TRUE)
    summary_links_files$file_hl[which_detect] <-
      stringr::str_replace(
        summary_links_files$file_hl[which_detect],
        "{.href [",
        "{.href [./"
      )
  }
  dat <- tibble::deframe(summary_links_files[, c("file_hl", "link")])
  # dat <- purrr::map_depth(dat, 1, \(x) rlang::set_names(x, "xd"))
  # browser()
  # current_time <- Sys.time()
  mod_date <- file.mtime(summary_links_files$file)
  # five most recent get a little ching
  if (length(mod_date) > 0) {
    suppressWarnings(is_recently_modified <- kit::topn(mod_date, n = 5))
  } else {
    is_recently_modified <- 1L
  }
  if (!recent_only && length(is_recently_modified) == length(dat)) {
    # don't show emojis if all are recently modified.
    is_recently_modified <- character(0)
  }

  for (i in seq_along(dat)) {
    base_name <- c(cli::col_blue(names(dat)[[i]]), " ")

    if (i %in% is_recently_modified) {
      # may decide to just color the name after all
      # was cli::bg_br_green("*")
      # Une crevette
      emoji_recent <- getOption("reuseme.recent_indicator", "\U0001f552")
      base_name <- c(base_name, emoji_recent)
    }

    # add first line to title and remove
    has_title <- !is.na(summary_links_files$first_line[[i]])
    if (has_title) {
      title_el <- withCallingHandlers(
        cli::format_inline(escape_markup(summary_links_files$first_line_el[[i]])),
        error = function(e) {
          thing <- summary_links_files$first_line_el[[i]]
          print(thing)
          print(escape_markup(thing))
          cli::cli_abort("Failed to parse in first line of file {.file {summary_links_files$file[[i]]}}.", parent = e)
        }
      )
      base_name <- c(base_name, " ", title_el)
    }
    # TRICK need tryCatch when doing something, withCallingHandlers when only rethrowing?
    tryCatch(
      cli::cli_h3(base_name),
      error = function(e) {
        # browser()
        cli::cli_h3(escape_markup(base_name))
        # print(base_name)
        # rlang::abort("Could not parse by cli", parent = e)
      }
    )

    if (recent_only) {
      if (i %in% is_recently_modified) {
        purrr::walk(dat[[i]], \(y) {
          y <- escape_markup(y[!is.na(y)])
          if (length(y)) cat(cli::format_inline(y), sep = "\n")
        })
      }
    } else {
      purrr::walk(dat[[i]], \(y) {
        y <- escape_markup(y[!is.na(y)])
        if (length(y)) cat(cli::format_inline(y), sep = "\n")
      })
    }
  }

  invisible(x)
}
# Step: tweak outline look as they show ---------
keep_outline_element <- function(.data) {
  # could use filter_if_any?
  .data$simplify_news <- sum(!is.na(.data$pkg_version)) >= 10
  if (any(.data$simplify_news)) {
    # only keep dev, latest and major versions of NEWS.md in outline.
    all_versions <- .data$pkg_version[!is.na(.data$pkg_version)]
    all_versions_norm <- package_version(all_versions)
    keep <- all_versions_norm == max(all_versions_norm, na.rm = TRUE) |
      endsWith(all_versions, "-0") | endsWith(all_versions, ".0")
    versions_to_drop <- all_versions[!keep]
  } else {
    versions_to_drop <- character(0L)
  }
  # browser()
  # For debugging.
  needed_elements <- which(grepl("REQUIRED ELEMENT", .data$content, fixed = TRUE) & !grepl("grep|keyword", .data$content, fixed = FALSE))

  dat <- dplyr::filter(
    .data,
    (is_news & (
      (!simplify_news & is_section_title & before_and_after_empty) |
        (simplify_news & is_section_title & !pkg_version %in% versions_to_drop & !is_second_level_heading_or_more & before_and_after_empty)
    )) |
      # still regular comments in .md files
      # what to keep in .md docs

      (is_md & (is_obj_caption | (is_section_title & before_and_after_empty))) |
      # What to keep in .R files
      (!is_md & is_section_title_source) |
      # What to keep anywhere
      is_tab_or_plot_title | is_todo_fixme | is_test_name | is_cross_ref | is_function_def | is_object_title | is_doc_title # | is_cli_info # TODO reanable cli info
  )

  dat$simplify_news <- NULL
  if (length(needed_elements) != length(grep("REQUIRED ELEMENT", dat$content, fixed = TRUE))) {
    zz <<- dplyr::slice(.data, needed_elements)
    cli::cli_abort(
      "Debugging mode. An important element is absent from the outline. Review filters, regex detection etc."
    )
  }
  dat
}

#
# Includes removing headings comments
# Remove title =
# Removing quotes, etc.
display_outline_element <- function(.data, dir_common) {
  x <- .data
  org_repo <- find_pkg_org_repo(dir_common, unique(x$file))
  if (!is.null(org_repo)) {
    x$outline_el <- link_local_gh_issue(x$content, org_repo)
  } else {
    x$outline_el <- x$content
  }
  x$outline_el <- purrr::map_chr(x$outline_el, \(x) link_gh_issue(x, org_repo)) # to add link to GitHub.
  x$outline_el <- purrr::map_chr(x$outline_el, markup_href)
  if (any(x$is_obj_caption)) {
    x$outline_el[x$is_obj_caption] <- extract_object_captions(x$file[x$is_obj_caption])
  }
  x <- dplyr::mutate(
    x,
    outline_el = dplyr::case_when(
      is_todo_fixme ~ stringr::str_extract(outline_el, "(TODO.+)|(FIXME.+)|(WORK.+)|(BOOK.+)"),
      is_test_name ~ stringr::str_extract(outline_el, "test_that\\(['\"](.+)['\"],\\s?\\{", group = 1),
      is_cli_info ~ stringr::str_extract(outline_el, "[\"'](.{5,})[\"']") |> stringr::str_remove_all("\""),
      # Add related topic if available
      tag == "title" & !is.na(topic) ~ paste0(outline_el, " [", topic, "]"),
      # family or concept!
      is_tab_or_plot_title & !is.na(tag) ~ outline_el,
      is_tab_or_plot_title ~ stringr::str_extract(outline_el, "title =[^\"']*[\"']([^\"]{5,})[\"']", group = 1),
      is_cross_ref ~ stringr::str_remove_all(outline_el, "^(i.stat\\:\\:)?.cdocs_lin.s\\(|[\"']\\)$|\""),
      is_doc_title ~ stringr::str_remove_all(outline_el, "subtitle\\:\\s?|title\\:\\s?|\"|\\#\\|\\s?"),
      is_section_title & !is_md ~ stringr::str_remove(outline_el, "^\\s{0,4}\\#+\\s+|^\\#'\\s\\#+\\s+"), # Keep inline markup
      is_section_title & is_md ~ stringr::str_remove_all(outline_el, "^\\#+\\s+|\\{.+\\}|<(a href|img src).+$"), # strip cross-refs.
      is_function_def ~ stringr::str_extract(outline_el, "(.+)\\<-", group = 1) |> stringr::str_trim(),
      .default = stringr::str_remove_all(outline_el, "^\\s*\\#+\\|?\\s?(label:\\s)?|\\s?[-\\=]{4,}")
    ),
    outline_el = dplyr::case_when(
      is_tab_or_plot_title ~ stringr::str_remove_all(outline_el, "(gt\\:\\:)?tab_header\\(|\\s*(sub)?title\\s\\=\\s['\"]|['\"],?$"),
      is_md & is_todo_fixme ~ stringr::str_remove(outline_el, "\\s?-{2,}\\>.*"),
      .default = outline_el
    ),
    outline_el = stringr::str_remove(outline_el, "[-\\=]{3,}") |> stringr::str_trim(), # remove trailing bars
    is_subtitle = (is_tab_or_plot_title | is_doc_title) & (
      grepl("subt", content, fixed = TRUE) |
        tag %in% c("family", "concept"))
  )

  if (anyNA(x$outline_el)) {
    zz <<- x |> dplyr::filter(is.na(outline_el))
    indices <- which(is.na(x$outline_el))
    all_na <- x |>
      dplyr::select(!dplyr::where(\(x) !is.logical(x) & all(is.na(x)))) |>
      dplyr::slice(dplyr::all_of(indices)) |>
      dplyr::select(dplyr::where(\(x) all(is.na(x)))) |>
      names()
    all_true_or_single_value <- x |>
      dplyr::slice(dplyr::all_of(indices)) |>
      dplyr::select(dplyr::where(\(x) dplyr::n_distinct(x) == 1)) |>
      dplyr::select(!dplyr::where(\(x) all(is.na(x)))) |>
      dplyr::select(!dplyr::where(\(x) is.logical(x) & !suppressWarnings(any(x, na.rm = FALSE)))) |>
      names()
    if (length(all_na) > 0) {
      msg <- c("The following places have all NAs {.var {all_na}}")
    } else {
      msg <- NULL
    }
    if (length(all_true_or_single_value) > 0) {
      msg <- c(msg, "Likely problems in creating or displaying {.var {all_true_or_single_value}}.")
    }
    cli::cli_abort(c(
      "Internal error, outline elements can't be NA. Please review.", msg,
      paste0("{.file ", zz$file, ":", zz$line, "}"),
      "Criteria are created in {.fn define_outline_criteria} and {.fn define_outline_criteria_roxy}.
                     `outline_el` is defined in {.fn display_outline_element}. Investigate `zz` for debugging."
    ))
  }

  y <- dplyr::mutate(
    x,
    has_title_el =
      ((line == 1 & !is_todo_fixme & !is_test_name & !is_snap_file) |
        (is_doc_title & !is_subtitle & !is_snap_file & !is_second_level_heading_or_more)) & !is_news,
    .by = "file"
  )
  y <- withCallingHandlers(
    dplyr::mutate(y,
      title_el_line = ifelse(has_title_el, line[
        (line == 1 & !is_todo_fixme & !is_test_name & !is_snap_file) |
          (is_doc_title & !is_subtitle & !is_snap_file & !is_second_level_heading_or_more)
      ][1], # take  the first element to avoid problems (may be the reason why problems occur)
      NA_integer_
      ),
      title_el = outline_el[line == title_el_line],
      .by = "file"
    ),
    error = function(e) {
      # browser()
      cli::cli_abort("Failed to do outline", parent = e)
    }
  )
  y <- dplyr::relocate(
    y,
    "outline_el", "line", "title_el", "title_el_line", "has_title_el",
    .after = "content"
  )


  y$outline_el <- ifelse(y$has_title_el, NA_character_, y$outline_el)
  na_if0 <- function(x, which) {
    if (length(x) == 0) {
      if (which == "title") {
        x <- NA_character_
      } else {
        x <- NA_integer_
      }
    }
    if (length(x) != 1) {
      cli::cli_inform("{x} are detected as document title. Internal error")
    }
    x
  }
  if (!all(is.na(y$title_el))) {
    # browser()
    y <- dplyr::mutate(
      y,
      title_el = na_if0(title_el[!is.na(title_el)], "title"),
      title_el_line = na_if0(title_el_line[!is.na(title_el_line)], "line"),
      .by = "file"
    )
  }
  y
}

# With files with detected object captions, like fig.cap, title, tab.cap, tbl.cap.
extract_object_captions <- function(file) {
  rlang::check_installed("lightparser", "to parse qmd files add chunk caption to outline.")
  # we want fig-cap, tbl-cap and title
  caps <- NULL
  unique_file <- unique(file)
  # ThinkR-open/lightparser#8
  for (i in seq_along(unique_file)) {
    # FIXME find a way to be as consistent as lightparser, but faster.
    dat <- tryCatch(
      lightparser::split_to_tbl(unique_file[i]),
      error = function(e) {
        # workaround ThinkR-open/lightparser#11
        tmp <- withr::local_tempfile(
          lines = c(
            "---",
            "title: dummy",
            "---",
            readLines(unique_file[i], warn = FALSE)
          )
        )
        lightparser::split_to_tbl(tmp)
      }
    )
    dat <- dplyr::filter(dat, type == "block")$params
    # Remove NA..
    dat <- purrr::discard(dat,\(x) isTRUE(is.na(x)))
    # tidyverse/purrr#1081
    if (length(dat) > 0) {
      # We use `format()` in case a variable is used to name the caption.
      tryCatch(caps <- c(caps, dat |> purrr::map_chr(\(x) format(x[["fig-cap"]] %||% x[["tbl-cap"]] %||% x[["title"]] %||% x[["fig.cap"]] %||% x[["tbl.cap"]] %||% x[["tab.cap"]] %||% x[["cap"]] %||% "USELESS THING"))), error = function(e) {
        cli::cli_abort("Error in {.file {unique_file[i]}}", parent = e)
      })
    }
  }
  # used as a default to make sure purrr doesn't complain
  caps <- caps[caps != "USELESS THING"]
  if (length(caps) != length(file)) {
    cli::cli_abort("error! :(, caps = {length(caps)}, file = {length(file)} in file {.file {unique_file}}")
  }
  caps |> stringr::str_squish()
}

define_important_element <- function(.data) {
  dplyr::mutate(
    .data,
    importance = dplyr::case_when(
      is_second_level_heading_or_more | is_obj_caption | is_cli_info | is_todo_fixme | is_subtitle | is_test_name ~ "not_important",
      .default = "important"
    )
  )
}

construct_outline_link <- function(.data, is_saved_doc, dir_common, pattern) {
  rs_avail_file_link <- is_rstudio("2023.09.0.375") # better handling after
  .data <- define_important_element(.data)

  if (is.null(dir_common) || !nzchar(dir_common)) {
    dir_common <- "Don't remove anything if not null"
  }
  .data$rs_version <- ifelse(!is_rstudio("2023.12.0.274") && is_rstudio(), ".", "")
  .data$has_inline_markup <- dplyr::coalesce(stringr::str_detect(.data$outline_el, "\\{|\\}"), FALSE)
  .data$is_saved_doc <- is_saved_doc
  .data <- dplyr::mutate(
    .data,
    condition_to_truncate = !is.na(outline_el) & !has_title_el & (is_todo_fixme) & is_saved_doc & !has_inline_markup,
    condition_to_truncate2 = !is.na(outline_el) & !has_title_el & !is_todo_fixme & (is_second_level_heading_or_more | is_subtitle | is_obj_caption) & is_saved_doc & !has_inline_markup
  )
  # r-lib/cli#627, add a dot before and at the end (Only in RStudio before 2023.12)
  .data$outline_el2 <- NA_character_
  width <- cli::console_width()

  cn <- .data$condition_to_truncate
  # Not showing up are the longer items.
  # truncating to make sure the hyperlink shows up.
  .data$outline_el2[cn] <- paste0(
    as.character(trim_outline(.data$outline_el[cn], width - 8L)),
    "- {.run [Done{cli::symbol$tick}?](reuseme::complete_todo(",
    # Removed ending dot. (possibly will fail with older versions)
    .data$line[cn], ", '", .data$file[cn], "', '",
    # modify regex twice if needed (see below)
    stringr::str_sub(stringr::str_replace_all(.data$content[cn], "\\^|\\$|'|\\{|\\}|\\)|\\(|\\[\\]|\\+", "."), start = -15L), "'))}",
    .data$rs_version[cn]
  )
  # truncate other elements
  cn2 <- .data$condition_to_truncate2
  .data$outline_el2[cn2] <- paste0(
    as.character(trim_outline(.data$outline_el[cn2], width - 1L)),
    # Removed ending dot. (possibly will fail with older versions)
    .data$rs_version[cn2]
  )
  .data <- dplyr::mutate(
    .data,
    outline_el2 = ifelse(
      is.na(outline_el2) & !is.na(outline_el) & !has_title_el & is_todo_fixme & is_saved_doc,
      paste0(
        outline_el,
        "- {.run [Done{cli::symbol$tick}?](reuseme::complete_todo(",
        # Removed ending dot. (possibly will fail with older versions)

        # modify regex twice if needed (see above)
        line, ", '", file, "', '", stringr::str_sub(stringr::str_replace_all(content, "\\^|\\$|'|\\{|\\}|\\)|\\(|\\[\\]|\\+", "."), start = -15L), "'))}",
        rs_version
      ),
      outline_el2
    ),
    outline_el2 = dplyr::coalesce(outline_el2, outline_el)
  )

  .data$link <- paste0(.data$outline_el2, " {.path ", .data$file, ":", .data$line, "}")
  # rstudioapi::documentOpen works in the visual mode!! but not fully.
  .data$file_path <- .data$file
  .data$is_saved_doc <- is_saved_doc
  # May have caused CI failure
  .data$text_in_link <- sub(as.character(dir_common), "", .data$file_path)
  .data$text_in_link <- sub("^/", "", .data$text_in_link)
  .data$style_fun <- dplyr::case_match(.data$importance,
    "not_important" ~ "cli::style_italic('i')", # cli::style_inverse for bullets
    "important" ~ "cli::style_inverse('i')",
    .default = NA_character_
  )

  if (anyNA(.data$style_fun)) {
    cli::cli_abort("Define this in {.fn define_important_element}", .internal = TRUE)
  }

  # Tweak n_leasing hash for todos or fixme..
  .data$n_leading_hash <- dplyr::case_when(
    .data$is_todo_fixme ~ dplyr::lead(.data$n_leading_hash, default = 0) + 1,
    .default = .data$n_leading_hash
  )
  .data$leading_space <- purrr::map_chr(.data$n_leading_hash, \(x) paste(rep(" ", length.out = max(min(x - 1, 1), 0)), collapse = ""))
  dplyr::mutate(.data,
    # link_rs_api = paste0("{.run [", outline_el, "](reuseme::open_rs_doc('", file_path, "', line = ", line, "))}"),
    link_rs_api = dplyr::case_when(
      is.na(outline_el2) ~ NA_character_,
      !is_saved_doc ~ paste0("line ", line, " -", outline_el2),
      rs_avail_file_link ~ paste0(
        leading_space,
        "{cli::style_hyperlink(", style_fun, ', "',
        paste0("file://", file_path), '", params = list(line = ', line, ", col = 1))} ", outline_el2
      ),
      .default = paste0(rs_version, "{.run [i](reuseme::open_rs_doc('", file_path, "', line = ", line, "))} ", outline_el2)
    ),
    file_hl = dplyr::case_when(
      !is_saved_doc ~ file_path,
      rs_avail_file_link ~ paste0("{.href [", text_in_link, "](file://", file_path, ")}"),
      .default = paste0("{.run [", text_in_link, "](reuseme::open_rs_doc('", file_path, "'))}")
    ),
    rs_version = NULL,
    outline_el2 = NULL,
    condition_to_truncate = NULL,
    condition_to_truncate2 = NULL,
    style_fun = NULL,
    is_saved_doc = NULL,
    is_roxygen_comment = NULL,
    is_notebook = NULL,
    is_news = NULL,
    # I may put it back ...
    importance = NULL,
    # may be useful for debugging
    before_and_after_empty = NULL,
    # may be useful for debugging
    has_inline_markup = NULL
  ) |>
    dplyr::filter(is.na(outline_el) | grepl(pattern, outline_el, ignore.case = TRUE))
}

trim_outline <- function(x, width) {
  # problematic in case_when
  cli::ansi_strtrim(x, width = width)
}
# Remove duplicated entries from outline
# for example, snapshots will have priority and will not return both the snapshot and the original test
scrub_duplicate_outline <- function(x) {
  x$order <- seq_len(nrow(x))
  # outline = NA (title)
  x$outline_el_count <- dplyr::coalesce(x$outline_el, x$title_el)
  #
  x <- dplyr::mutate(x, n_dup = dplyr::n(), .by = "outline_el_count")
  if (FALSE) {
    # TODO Improve performance with vctrs tidyverse/dplyr#6806
    mtcars$vs
    count <- vctrs::vec_count(mtcars$vs)
    res <- vctrs::vec_match(mtcars$vs, count$key)
    res[0]

    count$count

    factor(res, labels = c(count$key))
    match
  }
  x <- dplyr::mutate(
    x,
    # higher is better
    points = 1L + !is_test_name + is_section_title
  )

  x <- dplyr::slice_max(
    x,
    n = 1L,
    order_by = .data$points,
    with_ties = TRUE,
    by = "outline_el_count"
  )
  # use the previous order
  x <- dplyr::arrange(x, .data$order)
  x$points <- NULL
  x$order <- NULL
  x$n_dup <- NULL
  x$outline_el_count <- NULL
  x
}

arrange_outline <- function(x) {
  # extract first letter after removing inline markup
  var_to_order_by <- gsub("TODO|BOOK|FIXME|\\{.[:alpha:]{2,6}", "", x$outline_el)

  # Extract first letters
  var_to_order_by <- stringr::str_extract(
    var_to_order_by,
    "[:alpha:]+"
  )
  # if no letter, place it randomly
  var_to_order_by <- dplyr::coalesce(var_to_order_by, sample(letters, size = 1))

  ordered_rows <- order(var_to_order_by)

  x[ordered_rows, ]
}

get_dir_common_outline <- function(dir_common, path) {
  if (!is.null(dir_common)) {
    dir_common <- tryCatch(
      fs::path_real(dir_common),
      error = function(e) {
        cli::cli_abort("Don't specify `dir_common`, leave it as default", .internal = TRUE, parent = e)
      }
    )
  } else if (rlang::has_length(path, 1)) {
    # If a single path
    root_path <- tryCatch(
      rprojroot::find_root_file(
        path = path,
        criterion = rprojroot::is_rstudio_project
      ),
      silent = TRUE,
      error = function(e) fs::path_dir(fs::path_dir(path)) # parent path
    )
    dir_common <- fs::path(root_path)
  } else {
    dir_common <- fs::path_common(path)
  }

  dir_common
}

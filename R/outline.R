## `proj_outline()` -------------
#' Print interactive outline of file sections
#'
#' RStudio project, or directories
#' This will fail if you are trying to map an unsaved file.
#'
#' If `work_only` is set to `TRUE`, the function will only return outline of the `# WORK` comment
#' in `path`. `work_only = TRUE` will have an effect on regex_outline.
#' These functions are more
#' By default
#' * `file_outline()` prints the outline the [`active document`](active_document) if in RStudio
#' * `proj_outline()` prints the outline of the active project if in RStudio
#' * `dir_outline()` prints the outline of the active working directory by default or
#'
#' @details
#' proj_* and dir_ call file_outline.
#'
#' The parser is very opinioneted and is not very robust as it is based on regexps.
#' For a better file parser, explore other options, like [lightparser](https://thinkr-open.github.io/lightparser/), `{roxygen2}`
#'
#' Will show TODO items and will offer a link to [mark them as complete][complete_todo()]
#' @param path,proj A character vector of file paths, a [project][proj_list()]. Defaults to active file, project or directory. `rstudioapi::documentPath()`
#' @param regex_outline A string or regex to search for in the outline
#' @param work_only If `TRUE`, (the default), will only show you work items first. Set to `FALSE` if you want to see the full outline. `WORK` will combine with `regex_outline`
#' @param print_todo Should include TODOs in the file outline?
#'   If `FALSE`, will print a less verbose output with sections.
#' @param alpha Whether to show in alphabetical order
#' @param dir_tree If `TRUE`, will print the [fs::dir_tree()] or non-R files in the directory
#' @param recent_only Show outline for recent files
#' @param dir_common (Do not use it)
#' @param width Width (internal)
#' @param n_colors Number colours (Internal)
#'
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
#' file_outline(path = file, print_todo = FALSE, alpha = TRUE)
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
#' @export
#' @rdname outline
file_outline <- function(regex_outline = NULL,
                         path = active_rs_doc(),
                         work_only = TRUE,
                         alpha = FALSE,
                         dir_common = NULL,
                         print_todo = TRUE,
                         recent_only = FALSE,
                         width = cli::console_width(), # TODO put this in ... when I understand {.fn rlang::check_dots_used} as it may not be needed with 2023.12
                         n_colors = NULL) {
  # To contribute to this function, take a look at .github/CONTRIBUTING
  # https://github.com/r-lib/cli/issues/607
  # Make output faster with cli!
  if (is.null(n_colors)) {
    withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))
  } else {
    withr::local_options(list(cli.num_colors = n_colors))
  }

  if (length(path) == 1 && interactive() && rstudioapi::isAvailable()) {
    is_active_doc <- identical(path, active_rs_doc())
  } else {
    is_active_doc <- FALSE
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
    dir_common <- if (!is.null(dir_common)) {
      tryCatch(
        fs::path_real(dir_common),
        error = function(e) {
          cli::cli_abort("Don't specify `dir_common`, leave it as default", .internal = TRUE, parent = e)
        }
      )
    } else if (rlang::has_length(path, 1)) {
      # If a single path
      tryCatch(
        rprojroot::find_root_file(
          path = path,
          criterion = rprojroot::is_rstudio_project
        ),
        silent = TRUE,
        error = function(e) fs::path_dir(path) |> fs::path_dir()
      ) |>
        fs::path()
    } else {
      fs::path_common(path)
    }
    file_content <- purrr::set_names(path)
    # Not warn
    file_content <- purrr::map(file_content, function(x) readLines(fs::path_real(x), encoding = "UTF-8", warn = FALSE))
    # Combine everything into a tibble that contains file, line_id, content
    file_content <- purrr::map(file_content, function(x) tibble::enframe(x, name = "line_id", value = "content"))
    file_content <- dplyr::bind_rows(file_content, .id = "file")
  } else {
    file_content <-
      purrr::map(
        .x = list("unsaved-doc.R" = rstudioapi::getSourceEditorContext()$contents),
        .f = function(x) tibble::enframe(x, name = "line_id", value = "content")
      )
    file_content <- dplyr::bind_rows(file_content, .id = "file")
  }

  suppressMessages(in_active_project <- tryCatch(identical(proj_get2(), dir_common), error = function(e) FALSE))
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
  # otherwise sets regex_outline to anything
  if (should_show_only_work_items) {
    cli::cli_inform("Use {.code work_only = FALSE} to display the full file/project outline.")
    regex_outline <- paste(c("work\\s", regex_outline), collapse = "|")
  } else {
    regex_outline <- regex_outline %||% ".+"
  }

  check_string(regex_outline, arg = "You may have specified regex Internal error")

  file_sections0 <- define_outline_criteria(file_content, print_todo = print_todo)

  # filter for interesting items.
  file_sections0 <- keep_outline_element(file_sections0)

  if (exists("link_doc")) {
    file_sections0$content <- purrr::map_chr(file_sections0$content, link_doc)
  }
  # File outline ===================
  # strip outline element .data$outline = `# Section 1` becomes `Section 1`
  file_sections1 <- display_outline_element(file_sections0)

  # Create hyperlink in console
  file_sections <- construct_outline_link(file_sections1, is_saved_doc, dir_common, regex_outline)

  if (nrow(file_sections) == 0 && !identical(regex_outline, ".+")) {
    if (is_active_doc) {
      msg <- c("{.code regex_outline = {.val {regex_outline}}} did not return any results looking in the active document.",
        "i" = "Did you mean to use {.run reuseme::file_outline(path = {.str {regex_outline}})}?"
      )
    } else {
      msg <- c(
        "{.code regex_outline = {.val {regex_outline}}} did not return any results looking in {length(path)} file{?s}.",
        "i" = "Run {.run [{.fn proj_file}](reuseme::proj_file(\"{regex_outline}\"))} to search in file names too."
      )
    }
    cli::cli_abort(msg)
  }



  if (alpha) {
    # remove inline markup first before sorting alphabetically
    file_sections <- arrange_outline(file_sections)
  }

  # take most important first!
  file_sections <- file_sections |>
    dplyr::arrange(
      stringr::str_detect(file, "README|NEWS|vignettes")
    )
  file_sections$recent_only <- recent_only

  if (any(duplicated(file_sections$outline_el))) {
    file_sections <- scrub_duplicate_outline(file_sections)
  }
  file_sections <- file_sections |> dplyr::relocate(
    outline_el, title_el, title_el_line, .after = content
  )
  class(file_sections) <- c("outline_report", class(file_sections))

  file_sections
}
#' @rdname outline
#' @export
proj_outline <- function(regex_outline = NULL, proj = proj_get2(), work_only = TRUE, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE) {
  is_active_proj <- identical(proj, proj_get2())

  if (is_active_proj && !is.null(regex_outline) && regex_outline %in% names(reuseme::proj_list())) {
    # only throw warning if proj is supplied
    cli::cli_warn(c(
      "You specified {.arg regex_outline} = {.val {regex_outline}}",
      i = "Did you mean to use `proj = {.val {regex_outline}}?"
    ))
  }

  if (is_active_proj) {
    return(dir_outline(
      regex_outline = regex_outline,
      work_only = work_only,
      dir_tree = dir_tree,
      alpha = alpha,
      recent_only = recent_only
    ))
  }

  if (!fs::dir_exists(proj)) { # when referring to a project by name.
    all_projects <- reuseme::proj_list()
    rlang::arg_match0(proj, values = names(all_projects))
    proj_dir <- all_projects[proj]
  } else {
    if (!is_active_proj) {
      cli::cli_warn("Use {.fn dir_outline} for that.")
    }

    proj_dir <- proj
  }

  if (!rlang::has_length(proj_dir, 1)) {
    cli::cli_abort("Cannot process more than one project/directory at once.")
  }

  if (!fs::dir_exists(proj_dir)) {
    cli::cli_abort("Internal errors due to path processing. Maybe use fs's path processing ")
  }

  is_active_proj <- identical(fs::path(proj_dir), proj_get2())
  if (!is_active_proj) {
    # Add an outline that enables switching projects if searching outside
    cli::cli_h1(paste0("{.run [", proj, "](reuseme::proj_switch('", proj, "'))}"))
  }

  dir_outline(
    regex_outline = regex_outline,
    path = proj_dir,
    work_only = work_only,
    dir_tree = dir_tree,
    alpha = alpha
  )
}
#' @rdname outline
#' @export
dir_outline <- function(regex_outline = NULL, path = ".", work_only = TRUE, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE) {
  n_colors <- cli::num_ansi_colors()
  dir <- fs::path_real(path)
  file_exts <- c("R", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")

  file_list_to_outline <- fs::dir_ls(
    path = dir,
    type = "file",
    glob = file_exts_regex,
    recurse = TRUE
  )
  file_list_to_outline <- fs::path_filter(file_list_to_outline, regexp = "vignette-dump", invert = TRUE)
  if (any(grepl("README.Rmd", file_list_to_outline))) {
    file_list_to_outline <- stringr::str_subset(file_list_to_outline, "README.md", negate = TRUE)
  }
  if (dir_tree) {
    cli::cli_h2("Here are the non-R files of {.file {path}}")

    fs::dir_tree(
      path = dir,
      regexp = "R/.+|qmd|Rmd|_files|~\\$|*.Rd|_snaps|testthat.R|Rmarkdown|docs/",
      recurse = TRUE,
      invert = TRUE
    )
  }
  file_outline(path = file_list_to_outline, regex_outline = regex_outline, work_only = work_only, dir_common = dir, alpha = alpha, n_colors = n_colors, recent_only = recent_only)
}

# Methods -------------------

#' @export
print.outline_report <- function(x, ...) {
  if (nrow(x) == 0) {
    cli::cli_inform("Empty {.help [outline](reuseme::file_outline)}.")
    return(invisible(x))
  }
  custom_styling <- c(
    # 500 is the max path length.
    # green todo
    "(?<!(as_complete.{1,500}))(?<![\\w'])([:upper:]{4,5})\\:?($|\\s)" = "\\{.field \\2\\} " # put/work todo as emphasis
  )
  file_sections <- dplyr::as_tibble(x)
  recent_only <- x$recent_only[1]
  file_sections$link_rs_api <- stringr::str_replace_all(file_sections$link_rs_api, custom_styling)

  summary_links_files <- file_sections |>
    dplyr::summarise(
      first_line = unique(title_el_line),
      first_line_el = unique(title_el),
      link = list(purrr::set_names(
        link_rs_api,
        purrr::map_chr(
          paste0("{.file ", file, ":", line_id, "}"),
          cli::format_inline
        )
      )),
      .by = c(file_hl, file)
    )
  if (any(duplicated(summary_links_files$file))) {
    cli::cli_abort(c("Expected each file to be listed once."), .internal = TRUE)
  }
  # At the moment, especially `active_rs_doc()`, we are relying on path inconsistencies by RStudio.
  in_vscode <- FALSE # to do create it.
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
  # dat <- purrr::map_depth(dat, 1, \(x) purrr::set_names(x, "xd"))
  # browser()
  # current_time <- Sys.time()
  mod_date <- file.mtime(summary_links_files$file)
  # five most recent get a little ching
  if (length(mod_date) > 0) {
    suppressWarnings(is_recently_modified <- kit::topn(mod_date, n = 5))
  } else {
    is_recently_modified <- 1L
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
    if (!is.na(summary_links_files$first_line[[i]])) {
      base_name <- c(base_name, " ", cli::format_inline(escape_markup(summary_links_files$first_line_el[[i]])))
    }

    cli::cli_h3(base_name)

    if (recent_only) {
      if (i %in% is_recently_modified) {
        purrr::walk(dat[[i]], \(y) {
          y <- escape_markup(y)
          cat(cli::format_inline(y), sep = "\n")
        })
      }
    } else {
      purrr::walk(dat[[i]], \(y) {
        y <- escape_markup(y)
        cat(cli::format_inline(y), sep = "\n")
      })
    }
  }

  invisible(x)
}
# Step: tweak outline look as they show ---------
keep_outline_element <- function(.data) {
  # could use filter_if_any?
  dplyr::filter(
    .data,
    # still regular comments in .md files
    # what to keep in .md docs
    (is_md & (is_chunk_cap | is_doc_title)) |
      (is_md & (is_section_title & before_and_after_empty & !is_a_comment_or_code)) |
      # What to keep in .R files
      (!is_md & is_section_title_source) |
      # What to keep anywhere
      is_cli_info | is_tab_or_plot_title | is_todo_fixme | is_test_name | is_cross_ref
  )
}

#
# Includes removing headings comments
# Remove title =
# Removing quotes, etc.
display_outline_element <- function(.data) {
  x <- dplyr::mutate(
    .data,
    outline_el = purrr::map_chr(content, link_issue), # to add link to GitHub.
    outline_el = dplyr::case_when(
      is_todo_fixme ~ stringr::str_extract(outline_el, "(TODO.+)|(FIXME.+)|(WORK.+)"),
      is_test_name ~ stringr::str_extract(outline_el, "test_that\\(['\"](.+)['\"]", group = 1),
      is_cli_info ~ stringr::str_extract(outline_el, "[\"'](.{5,})[\"']") |> stringr::str_remove_all("\""),
      is_tab_or_plot_title ~ stringr::str_extract(outline_el, "title = [\"']([^\"]{5,})[\"']", group = 1),
      is_chunk_cap_next ~ stringr::str_remove(outline_el, "\\s?\\#\\|\\s+"),
      is_chunk_cap ~ stringr::str_remove_all(stringr::str_extract(outline_el, "cap:(.+)", group = 1), "\"|'"),
      is_cross_ref ~ stringr::str_remove_all(outline_el, "^(instat\\:\\:)?gcdocs_links\\(|\"\\)$"),
      is_doc_title ~ stringr::str_remove_all(outline_el, "subtitle\\:\\s?|title\\:\\s?|\"|\\#\\|\\s?"),
      is_section_title & !is_md ~ stringr::str_remove(outline_el, "^\\s{0,4}\\#+\\s+|^\\#'\\s\\#+\\s+"), # Keep inline markup
      is_section_title & is_md ~ stringr::str_remove_all(outline_el, "^\\#+\\s+|\\{.+\\}"), # strip cross-refs.
      .default = stringr::str_remove_all(outline_el, "^\\s*\\#+\\|?\\s?(label:\\s)?|\\s?[-\\=]{4,}")
    ),
    outline_el = dplyr::case_when(
      is_tab_or_plot_title ~ stringr::str_remove_all(outline_el, "(gt\\:\\:)?tab_header\\(|\\s*(sub)?title\\s\\=\\s['\"]|['\"],?$"),
      .default = outline_el
    ),
    outline_el = stringr::str_remove(outline_el, "[-\\=]{3,}") |> stringr::str_trim(), # remove trailing bars
    is_subtitle = (is_tab_or_plot_title | is_doc_title) & stringr::str_detect(content, "subt"),
  )
  y <- dplyr::mutate(
    x,
    has_title_el =
      (line_id == 1 & !is_todo_fixme & !is_test_name & !is_snap_file) |
        (is_doc_title & !is_subtitle),
    title_el_line = ifelse(has_title_el, line_id[(line_id == 1 & !is_todo_fixme & !is_test_name & !is_snap_file) | is_doc_title], NA),
    title_el = outline_el[title_el_line],
    .by = file
  )
  y$outline_el <- ifelse(y$has_title_el, NA, y$outline_el)
  na_if0 <- function(x) {
    if (length(x) == 0) {
      x <- NA
    }
    if (length(x) == 2) {
      cli::cli_inform("{x} are detected as document title. Internal error")
    }
    x
  }
  if (!all(is.na(y$title_el))) {
    y <- dplyr::mutate(
      y,
      title_el = na_if0(title_el[!is.na(title_el)]),
      title_el_line = na_if0(title_el_line[!is.na(title_el_line)]),
      .by = c(file)
    )
    y <- y[!is.na(y$outline_el), ]
  }
  y
}

define_important_element <- function(.data) {
  dplyr::mutate(
    .data,
    importance = dplyr::case_when(
      is_second_level_heading_or_more | is_chunk_cap | is_cli_info | is_todo_fixme | is_subtitle | is_test_name ~ "important",
      .default = "not_important"
    )
  )
}

construct_outline_link <- function(.data, is_saved_doc, dir_common, regex_outline) {
  rs_avail_file_link <- rstudioapi::isAvailable("2023.09.0.375") # better handling after
  .data <- define_important_element(.data)

  if (is.null(dir_common) || !nzchar(dir_common)) {
    dir_common <- "Don't remove anything if not null"
  }
  .data <- dplyr::mutate(
    .data,
    # r-lib/cli#627, add a dot before and at the end (Only in RStudio before 2023.12)
    rs_version = ifelse(!rstudioapi::isAvailable("2023.12.0.274") && rstudioapi::isAvailable(), ".", ""),
    outline_el = dplyr::case_when(
      # Not showing up are the longer items.
      # truncating to make sure the hyperlink shows up.
      is_todo_fixme & is_saved_doc ~ paste0(
        outline_el,
        "- {.run [Done{cli::symbol$tick}?](reuseme::complete_todo(",
        # Removed ending dot. (possibly will fail with older versions)
        line_id, ", '", file, "', '", stringr::str_sub(stringr::str_replace_all(content, "'|\\{|\\}|\\)|\\(|\\[\\]", "."), start = -15L), "'))}",
        rs_version
      ),
      .default = outline_el
    ),
    link = paste0(outline_el, " {.path ", file, ":", line_id, "}"),
    # rstudioapi::documentOpen works in the visual mode!! but not fully.
    file_path = .data$file,
    is_saved_doc = .env$is_saved_doc,

    # May have caused CI failure
    text_in_link = stringr::str_remove(file_path, as.character(.env$dir_common)) |> stringr::str_remove("^/"),
    # decide which is important
    style_fun = dplyr::case_match(importance,
      "important" ~ "style_italic", # cli::style_inverse for bullets
      "not_important" ~ "style_inverse",
      .default = NA
    )
  )

  if (anyNA(.data$style_fun)) {
    cli::cli_abort("Define this in {.fn define_important_element}", .internal = TRUE)
  }

  dplyr::mutate(.data,
    # link_rs_api = paste0("{.run [", outline_el, "](reuseme::open_rs_doc('", file_path, "', line = ", line_id, "))}"),
    link_rs_api = dplyr::case_when(
      !is_saved_doc ~ paste0("line ", line_id, " -", outline_el),
      rs_avail_file_link ~ paste0("{cli::style_hyperlink(cli::", style_fun, '("i"), "', paste0("file://", file_path), '", params = list(line = ', line_id, ", col = 1))} ", outline_el),
      .default = paste0(rs_version, "{.run [i](reuseme::open_rs_doc('", file_path, "', line = ", line_id, "))} ", outline_el)
    ),
    file_hl = dplyr::case_when(
      !is_saved_doc ~ file_path,
      rs_avail_file_link ~ paste0("{.href [", text_in_link, "](file://", file_path, ")}"),
      .default = paste0("{.run [", text_in_link, "](reuseme::open_rs_doc('", file_path, "'))}")
    ),
    rs_version = NULL
  ) |>
    dplyr::filter(tolower(outline_el) |> stringr::str_detect(tolower(regex_outline)))
}

# Remove duplicated entries from outline
# for example, snapshots will have priority and will not return both the snapshot and the original test
scrub_duplicate_outline <- function(x) {
  x$order <- seq_len(nrow(x))
  x <- dplyr::mutate(x, n_dup = dplyr::n(), .by = outline_el)

  x <- dplyr::mutate(
    x,
    # higher is better
    points = !is_test_name + is_section_title
  )

  x <- dplyr::slice_max(
    x,
    n = 1,
    order_by = points,
    with_ties = TRUE,
    by = outline_el
  )
  # use the previous order
  x <- dplyr::arrange(x, order)
  x$points <- NULL
  x$order <- NULL
  x$n_dup <- NULL
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

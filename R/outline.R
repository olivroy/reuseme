## `proj_outline()` -------------
#' Gives the outlines of file, RStudio project, or directories
#'
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
#' proj_* and dir_ call file_outline
#'
#' @param path a character vector of file paths (file_outline) or directory by default `rstudioapi::documentPath()`
#' @param proj The project name (See [proj_list()])
#' @param regex_outline A string or regex to search for in the outline
#' @param work_only If `TRUE`, (the default), will only show you work items first. Set to `FALSE` if you want to see the full outline. `WORK` will combine with `regex_outline`
#' @param print_todo Should include TODOs in the file outline?
#'   If `FALSE`, will print a less verbose output with sections.
#' @param alpha Whether to show in alphabetical order
#' @param dir_tree If `TRUE`, will print the [fs::dir_tree()] or non-R files in the directory
#' @param dir_common (Do not use it)
#'
#' @returns A list / tree of the file outline
#' @name outline
#' @examples
#' file_outline()
#' proj_outline()
#' dir_outline()
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
  # https://github.com/r-lib/cli/issues/607
  # Make output faster with cli!
  if (is.null(n_colors)) {
    withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))
  } else {
    withr::local_options(list(cli.num_colors = n_colors))
  }
  # active_document() returns `NULL` if the active document is unsaved.
  is_unsaved_doc <- is.null(path)
  if (!is_unsaved_doc) {
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
          cli::cli_abort("Don't specify `dir_common`, leave it as default", .internal = TRUE)
        }
      )
    } else if (rlang::has_length(path, 1)) {
      # If a single path,
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

  suppressMessages(in_active_project <- tryCatch(identical(usethis::proj_get(), dir_common), error = function(e) FALSE))
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

  file_sections0 <- file_content |>
    dplyr::mutate(
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
      is_cli_info = is_cli_info & !file_with_many_functions,
      is_doc_title = stringr::str_detect(content, "title\\:"),
      is_tab_or_plot_title = stringr::str_detect(content, "[^(\")]title = \"|tab_header") &
        stringr::str_detect(content, "\\[", negate = TRUE),
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
      is_test_name = is_test_file & o_is_test_that(content),
      is_section_title = stringr::str_detect(content, "^\\#+\\s"),
      is_a_comment_or_code = stringr::str_detect(content, "!=|\\|\\>|\\(\\.*\\)"),
      is_todo_fixme = print_todo & o_is_todo_fixme(content) & !o_is_roxygen_comment(content, file_ext) & !stringr::str_detect(file, "_snaps"),
      is_section_title_source = stringr::str_detect(content, "\\#+\\s") & stringr::str_detect(content, "[\\-\\=]{3,}") & !stringr::str_detect(content, "\\@param") & stringr::str_starts(content, "\\s*\"", negate = TRUE),
      before_and_after_empty = !nzchar(dplyr::lead(content)) & !nzchar(dplyr::lag(content)),
      n_leading_hash = nchar(stringr::str_extract(content, "\\#+")),
      n_leading_hash = dplyr::coalesce(n_leading_hash, 0),
      is_second_level_heading_or_more = (is_section_title_source | is_section_title) & n_leading_hash > 1,
      is_cross_ref = stringr::str_detect(content, "docs_links?\\(") & !stringr::str_detect(content, "@param|\\{\\.")
    ) |>
    # TODO improve to the full outline of the file containing a works item
    # Maybe suggest
    # dplyr::mutate(
    #   is_work_item = any(stringr::str_detect(content, regex_outline_work)),
    #   .by = file
    # ) |>
    dplyr::filter(
      # still regular comments in .md files
      # what to keep in .md docs
      (is_md & (is_chunk_cap | is_doc_title | (is_section_title & before_and_after_empty & !is_a_comment_or_code))) |
        # What to keep in .R files
        (!is_md & is_section_title_source) | is_cli_info | is_tab_or_plot_title | is_todo_fixme | is_test_name | is_cross_ref
    )

  if (exists("link_doc")) {
    file_sections0$content <- purrr::map_chr(file_sections0$content, link_doc)
  }

  file_sections0 <- file_sections0 |>
    dplyr::mutate(
      # FIXME creates issues for mark_todo_as_complete.
      content = purrr::map_chr(content, link_issue), # to add link to GitHub.
      outline_el = dplyr::case_when(
        is_todo_fixme ~ stringr::str_extract(content, "(TODO.+)|(FIXME.+)|(WORK.+)"),
        is_test_name ~ stringr::str_extract(content, "test_that\\(['\"](.+)['\"]", group = 1),
        is_cli_info | is_tab_or_plot_title ~ stringr::str_extract(content, "[\"'](.{5,})[\"']", group = 1),
        is_chunk_cap_next ~ stringr::str_remove(content, "\\s?\\#\\|\\s+"),
        is_chunk_cap ~ stringr::str_remove_all(stringr::str_extract(content, "cap:(.+)", group = 1), "\"|'"),
        is_cross_ref ~ stringr::str_remove_all(content, "^(instat\\:\\:)?gcdocs_links\\(|\"\\)$"),
        is_doc_title ~ stringr::str_remove_all(content, "title\\:\\s?|\""),
        .default = stringr::str_remove_all(content, "^\\s*\\#+\\|?\\s?(label:\\s)?|\\s?[\\-\\=]{4,}")
      ),
      outline_el = stringr::str_remove(outline_el, "\\-{3,}"),
      is_subtitle = (is_tab_or_plot_title | is_doc_title) & stringr::str_detect(content, "subt"),
      important = dplyr::case_when(
        is_second_level_heading_or_more | is_chunk_cap | is_cli_info | is_todo_fixme | is_subtitle | is_test_name ~ FALSE,
        .default = TRUE
      )
    )
  rs_avail_file_link <- rstudioapi::isAvailable("2023.09.0.375") # better handling after

  file_sections <- file_sections0 |>
    dplyr::mutate(
      # r-lib/cli#627, add a dot before and at the end (Only in RStudio before 2023.12)
      rs_version = ifelse(!rstudioapi::isAvailable("2023.12.0.274") && rstudioapi::isAvailable(), ".", ""),
      outline_el = dplyr::case_when(
        # Not showing up are the longer items.
        # truncating to make sure the hyperlink shows up.
        # Mark as done caused issues as it ends with inline markup/.(tracked in r-lib/cli#627)
        is_todo_fixme & !is_unsaved_doc ~ paste0(
          outline_el,
          "- {.run [Done{cli::symbol$tick}?](reuseme::mark_todo_as_complete(",
          # Removed ending dot. (possibly will fail with older versions)
          line_id, ", '", (file), "', '", cli::ansi_strip(stringr::str_trim(stringr::str_sub(stringr::str_replace_all(outline_el, "[:punct:]", "."), start = -15L), side = "right")), "'))}",
          rs_version
        ),
        .default = outline_el
      ),
      link = paste0(outline_el, " {.path ", file, ":", line_id, "}"),
      # rstudioapi::documentOpen works in the visual mode!!
      file_path = .data$file,
      is_unsaved_doc = .env$is_unsaved_doc,
      text_in_link = file_path |> stringr::str_remove(as.character(.env$dir_common %||% "Don't remove anything if NULL")) |> stringr::str_remove("^/"),
      style_fun = dplyr::case_when(
        !important ~ "style_italic",
        .default = "style_inverse"
      ),
      # link_rs_api = paste0("{.run [", outline_el, "](reuseme::open_rs_doc('", file_path, "', line = ", line_id, "))}"),
      link_rs_api = dplyr::case_when(
        is_unsaved_doc ~ paste0("line ", line_id, " -", outline_el),
        rs_avail_file_link ~ paste0("{cli::style_hyperlink(cli::", style_fun, '("i"), "', paste0("file://", file_path), '", params = list(line = ', line_id, ", col = 1))} ", outline_el),
        .default = paste0(rs_version, "{.run [i](reuseme::open_rs_doc('", file_path, "', line = ", line_id, "))} ", outline_el)
      ),
      file_hl = dplyr::case_when(
        is_unsaved_doc ~ file_path,
        rs_avail_file_link ~ paste0("{.href [", text_in_link, "](file://", file_path, ")}"),
        .default = paste0("{.run [", text_in_link, "](reuseme::open_rs_doc('", file_path, "'))}")
      ),
      rs_version = NULL
    ) |>
    dplyr::filter(tolower(outline_el) |> stringr::str_detect(tolower(regex_outline)))

  if (nrow(file_sections) == 0 && !identical(regex_outline, ".+")) {
    msg <- "{.code regex_outline = {.val {regex_outline}}} did not return any results looking in {length(path)} file{?s}."
    cli::cli_abort(msg)
  }

  custom_styling <- c(
    # 500 is the max path length.
    "(?<!(as_complete.{1,500}))(?<![\\w'])([:upper:]{4,5})($|\\s)" = "\\{.strong \\2\\} ",  # put/work todo as emphasis
    "\\{+gt\\}+" = "{{gt}}" # little patch, but should look into how escape_markup would better work.
  )
  # browser()
  # TODO filter or provide n-max

  if (alpha) {
    # browser()
    # remove inline markup first before sorting alphabetically
    file_sections <-
      dplyr::arrange(
        file_sections,
        dplyr::coalesce(stringr::str_extract(stringr::str_remove_all(outline_el, "TODO|BOOK|FIXME|\\{.[:alpha:]{2,6}"), "[:alpha:]"), sample(letters, size = 1))
      )
  }

  summary_links_files <- file_sections |>
    dplyr::mutate(
      link_rs_api = stringr::str_replace_all(link_rs_api, custom_styling)) |>
    dplyr::group_by(file_hl, file) |>
    # dplyr::summarise(link = list(link_rs_api)) # reinstate iff other is too slow.
    dplyr::summarise(link = list(purrr::set_names(link_rs_api, purrr::map_chr(paste0("{.file ", file, ":", line_id, "}"), cli::format_inline))), .groups = "drop")
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
    if (i %in% is_recently_modified) {
      # may decide to just color the name after all
      # was cli::bg_br_green("*")
      cli::cli_h3(c(names(dat)[[i]], " ", cli::style_no_blurred("🦐")))
    } else {
      cli::cli_h3(names(dat)[[i]])
    }


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

  # rm(a_useles_value)
}
#' @rdname outline
#' @export
proj_outline <- function(regex_outline = NULL, proj = usethis::proj_get(), work_only = TRUE, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE) {
  is_active_proj <- identical(proj, usethis::proj_get())

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

  is_active_proj <- identical(fs::path(proj_dir), usethis::proj_get())
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
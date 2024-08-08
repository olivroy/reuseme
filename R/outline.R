## `proj_outline()` -------------
#' Print interactive outline of file sections
#'
#' @description
#' The outline functions return a data frame that contains details of file location.
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
#' * FIgures caption in Quarto documents (limited support for multiline caption currently)
#' * test names
#' * Indicator of recent modification
#' * Colored output for
#' * URL and gh issue detection and link creation.
#'
#' By default
#' * `file_outline()` prints the outline the [active document][active_rs_doc()] if in RStudio
#' * `proj_outline()` prints the outline of the [active project][usethis::proj_get()] if in RStudio
#' * `dir_outline()` prints the outline of the [active working directory][getwd()] by default or
#'
#' @details
#' `proj_outline()` and `dir_outline()` are wrapper of `file_outline()`.
#'
#' In `proj_outline()`, `path` accepts project names, see [proj_list()] for how to
#' set up reuseme to regognize your projects' locations.
#'
#' The parser is very opinionated and is not very robust as it is based on regexps.
#' For a better file parser, explore other options, like [lightparser](https://thinkr-open.github.io/lightparser/) for Quarto,  `{roxygen2}`
#'
#' Will show TODO items and will offer a link to [mark them as
#' complete][complete_todo()].
#'
#' Note that `proj_outline()` strips some test files from the outline, as example
#' test files (like in usethis repo) don't help understand a project's outline.
#' Use `dir_outline(recurse = TRUE)` to make sure these are included in your outline.
#'
#' @param path A character vector of file paths, a [project][proj_list()].
#'   Defaults to the [active file][active_rs_doc()], project or directory.
#' @param pattern A string or regex to search for in the outline. If
#'   specified, will search only for elements matching this regular expression.
#'   The print method will show the document title for context. Previously `regex_outline`
#' @param print_todo Should include TODOs in the file outline? If `FALSE`, will
#'   print a less verbose output with sections.
#' @param alpha Whether to show in alphabetical order
#' @param dir_tree If `TRUE`, will print the [fs::dir_tree()] or non-R files in
#'   the directory
#' @param recent_only Show outline for recent files
#' @inheritParams fs::dir_ls
#' @returns A `outline_report` object that contains the information. Inherits
#' `tbl_df`.
#'
#' A symbol will show for recently modified files.
#' @name outline
#' @examples
#' file <- fs::path_package("reuseme", "example-file", "outline-script.R")
#' file_outline(file)
#'
#' # Remove todo items
#' file_outline(file, print_todo = FALSE, alpha = TRUE)
#'
#' # interact with data frame
#' file_outline(file) |> dplyr::as_tibble()
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
file_outline <- function(path = active_rs_doc(),
                         pattern = NULL,
                         alpha = FALSE,
                         print_todo = TRUE,
                         recent_only = FALSE) {
  # To contribute to this function, take a look at .github/CONTRIBUTING
  check_string(pattern, allow_null = TRUE)

  if (length(path) == 1L && rlang::is_interactive() && is_rstudio()) {
    is_active_doc <- identical(path, active_rs_doc())
  } else {
    is_active_doc <- FALSE
  }
  # active_rs_doc() returns `NULL` if the active document is unsaved.
  is_saved_doc <- !is.null(path)
  if (length(path) == 0L && is_saved_doc) {
    cli::cli_abort("No path specified.")
  }

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

  # After this point we have validated that paths exist.

  file_sections00 <- define_outline_criteria(file_content, print_todo = print_todo)

  # filter for interesting items.
  # Also scrub duplicated items, as they are likely to be uninteresting.
  file_sections0 <- keep_outline_element(file_sections00)

  if (!is.null(pattern)) {
    # keep files where pattern was detected (not the generic .+)
    file_sections0 <- dplyr::filter(
      file_sections0,
      any(grepl(pattern, content, ignore.case = TRUE)),
      .by = "file"
    )
  }

  if (nrow(file_sections0) == 0) {
    if (is_active_doc && !is.null(pattern)) {
      msg <- c("{.code pattern = {.val {pattern}}} did not return any results looking in the active document.")
    } else if (!is.null(pattern)) {
      msg <- c(
        "{.code pattern = {.val {pattern}}} did not return any results looking in {length(path)} file{?s}.",
        "i" = "Run {.run [{.fn proj_file}](reuseme::proj_file(\"{pattern}\"))} to search in file names too."
      )
    } else {
      msg <- c("{.path {path}}","Empty outline.")
    }
    cli::cli_inform(msg)
    return(invisible())
  }
  if (exists("link_doc")) {
    file_sections0$content <- purrr::map_chr(file_sections0$content, link_doc)
  }
  # File outline ===================
  # strip outline element .data$outline = `# Section 1` becomes `Section 1`
  file_sections1 <- display_outline_element(file_sections0)

  if (is.null(pattern)) {
    #file_sections1 <- file_sections1[!is.na(file_sections1$outline_el), ]
  } else {
    file_sections1 <- file_sections1[is.na(file_sections1$outline_el) | grepl(pattern, file_sections1$outline_el, ignore.case = TRUE), ]
  }

  if (alpha) {
    # remove inline markup first before sorting alphabetically
    file_sections1 <- arrange_outline(file_sections1)
  }

  # take most important first!
  file_sections1 <- dplyr::arrange(
    file_sections1,
    grepl("README|NEWS|vignettes", file)
  )
  file_sections1 <- dplyr::relocate(
    file_sections1,
    "outline_el", "title_el", "title_el_line",
    .after = "content"
  )
  # Create hyperlink in console
  file_sections <- remove_outline_columns(
    file_sections1
  )

  file_sections$recent_only <- recent_only

  class(file_sections) <- c("outline_report", class(file_sections))
  file_sections
}
#' @rdname outline
#' @export
proj_outline <- function(path = active_rs_proj(), pattern = NULL, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE) {
  check_proj(path, allow_null = TRUE)
  path_proj <- proj_list(path)

  if (!rlang::has_length(path_proj, 1)) {
    cli::cli_abort("Cannot process more than one project/directory at once.")
  }

  if (!fs::dir_exists(path_proj)) {
    cli::cli_abort("Internal errors due to path processing. Maybe use fs's path processing problem.")
  }

  if (!is_proj(path_proj)) {
    cli::cli_abort("Not in a project. Use {.fn reuseme::dir_outline} instead.")
  }

  if (!is.null(path)) {
    proj <- basename(path)
    # Add an outline that enables switching projects if searching outside
    cli::cli_h1(paste0("{.run [", proj, "](reuseme::proj_switch('", proj, "'))}"))
  }

  dir_outline(
    path = path_proj,
    pattern = pattern,
    dir_tree = dir_tree,
    alpha = alpha,
    recurse = TRUE
  )
}
#' @rdname outline
#' @export
dir_outline <- function(path = ".", pattern = NULL, dir_tree = FALSE, alpha = FALSE, recent_only = FALSE, recurse = FALSE) {
  dir <- fs::path_real(path)
  file_exts <- c("R", "RProfile", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")

  file_list_to_outline <- fs::dir_ls(
    path = dir,
    type = "file",
    glob = file_exts_regex,
    recurse = recurse
  )

  if (recurse && !identical(Sys.getenv("TESTTHAT"), "true")) {
    # Remove examples from outline and test example files to avoid clutter
    # examples don't help understand a project.
    file_list_to_outline <- exclude_example_files(file_list_to_outline)
  }

  # TODO expand this to apply to most generated files
  if (any(grepl("README.Rmd", file_list_to_outline))) {
    file_list_to_outline <- stringr::str_subset(file_list_to_outline, "README.md", negate = TRUE)
  }

  if (dir_tree) {
    cli::cli_h2("Here are the non-R files of {.file {path}}")
    regexp_exclude <- paste(
      "vignettes/test/", # test vignettes
      "LICENSE.md", # avoid indexing this.
      "cran-comments.md",
      "tests/(performance-monitor|gt-examples/|testthat/(dummy_|exclusions-test/|scope-|assets|_outline|testTestWithFailure|testTest/|test-parallel/|test-list-reporter/|serialize_tests/|line_breaks_and_other/|indentation_multiple/|public-api/|rmd/|examples/))", # example files in usethis, pkgdown, reuseme, devtools, etc.
      "inst/((rmarkdown/)?templates/|example-file/|examples/rmd/|tutorials/)", # license templates in usethis
      "revdep/", # likely don't need to outline revdep/, use dir_outline() to find something in revdep/
      "themes/hugo-theme-console/", # protect blogdown
      "vignettes/.+\\.R$", # generated files
      "vignette-dump|renv/",
      "RcppExports.R",
      "malformed", # likely for tests
      "pkgdown/assets",
      sep = "|"
    )
    fs::dir_tree(
      path = dir,
      regexp = paste0("R/.+|qmd|Rmd|_files|~\\$|*.Rd|_snaps|tests/testthat.R|Rmarkdown|docs/|", regexp_exclude),
      recurse = recurse,
      invert = TRUE
    )
  }
  file_outline(path = file_list_to_outline, pattern = pattern, alpha = alpha, recent_only = recent_only)
}

exclude_example_files <- function(path) {
  # styler tests examples may not work..

  regexp_exclude <- paste(
    "vignettes/test/", # test vignettes
    "LICENSE.md", # avoid indexing this.
    "cran-comments.md",
    "tests/(performance-monitor|gt-examples/|testthat/(dummy_|exclusions-test/|scope-|assets|_outline|testTestWithFailure|testTest/|test-parallel/|test-list-reporter/|serialize_tests/|line_breaks_and_other/|indentation_multiple/|public-api/|rmd/|examples/))", # example files in usethis, pkgdown, reuseme, devtools, etc.
    "inst/((rmarkdown/)?templates/|example-file/|examples/rmd/|tutorials/)", # license templates in usethis
    "revdep/", # likely don't need to outline revdep/, use dir_outline() to find something in revdep/
    "themes/hugo-theme-console/", # protect blogdown
    "vignettes/.+\\.R$", # generated files
    "vignette-dump|renv/",
    "RcppExports.R",
    "malformed", # likely for tests
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
print.outline_report <- function(x, ..., sort = F, collapse_nodes = T, prune_nodes = T, theme = NULL) {
  # https://github.com/r-lib/cli/issues/607
  # Make output faster with cli!
  withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))

  if (sum(!x$is_function_def) == 0) {
    cli::cli_inform("Empty {.help [outline](reuseme::file_outline)}.")
    return(invisible(x))
  }
  file_sections <- dplyr::as_tibble(x)
  recent_only <- x$recent_only[1]
  # add links and truncate elements
  file_sections$outline_el[!is.na(file_sections$outline_el)] <-
    escape_markup(file_sections$outline_el[!is.na(file_sections$outline_el)])
  file_sections <- construct_outline_link(file_sections)

  custom_styling <- c(
    # 500 is the max path length.
    # green todo
    "(?<!(complete_todo.{1,500}))(?<![\\w'])([:upper:]{4,5})\\:?($|\\s)" = "\\{.field \\2\\} ", # put/work todo as emphasis
    "\\{\\.pkg \\{\\(?pkg\\$package\\}\\}\\)?" = "{.pkg {package}}", # until complex markup is resolved.
    # Workaround r-lib/cli#693
    "\\[([[:alpha:]\\s]+)\\]\\s" = "{cli::bg_white(cli::col_black('\\1'))} "
  )

  file_sections$link_rs_api <- stringr::str_replace_all(file_sections$link_rs_api, custom_styling)

  if (anyDuplicated(stats::na.omit(file_sections$outline_el)) > 0L) {
    # Remove all things that appear more than 4 times in a file.
    # this typically indicates a placeholder
    file_sections <- dplyr::filter(
      file_sections,
      dplyr::n() < 4,
      .by = c("file", "outline_el")
    )
  }
  outline <- file_sections

   # Make outline data match new API ----
  # outline_new <- reuseme::file_outline(path = "test.qmd") %>%
  # outline_new <- reuseme::file_outline(path = "motebook.qmd") %>%
  outline_new <- outline %>%
    # Convert the many is_ columns into mutually exclusive "outline row types"
    pivot_longer(
      names_to = "type", names_prefix = "is_", c(
        starts_with("is_"), -is_md, -is_second_level_heading_or_more
      )
    ) %>%
    # # Double check that types are mututally exclusive
    # filter(sum(value) != 1, .by = c(file, line))
    filter(value == T) %>%
    # We drop these because they don't serve to add much context to TODOs (they don't affect hierarchy)
    filter(type != "tab_or_plot_title") %>%

    # Some useful definitions!
    mutate(
      # title = coalesce(outline_el, title_el),
      title = coalesce(outline_el, title_el),
      n_leading_hash = type %>% case_match(
        # these items should inheirit the last indent +1
        c("todo_fixme", "tab_or_plot_title") ~ NA,
        # headings use hashes
        .default = n_leading_hash
      )
    ) %>%

    # For each file, stick a item at the top of the outline
    group_by(file) %>%
    group_modify(\(data, group) data %>% add_row(
      .before = 0,
      n_leading_hash = -1,
      title = fs::path_file(group$file),
      type = "file"
    )) %>%

    mutate(
      # Assign TODO items (and other items missing n_leading_hash)
      # to be indented under the last seen header level
      indent = coalesce(n_leading_hash, zoo::na.locf0(n_leading_hash+1)),

      # If there are any headers that skip an intermediate level,
      # step thru and refine the indenting
      # TODO: break indent cleaning into separate function and also apply after file_outline()
      indent %>% reduce(
        .init = tibble(orig_indent = integer(), stack = list(), adjust = integer(), indent = integer()),
        \(temp, orig_indent) {
          if (last(temp$stack) %>% is.null()) {
            # If we're at the top level, make sure indent starts at 0 not -1 or anything
            new_stack <- tibble(adjust = -orig_indent, pop_adjust_at = orig_indent)
          } else {
            new_stack <- last(temp$stack) %>%
              # If we reach a point on the outline where we're back up in
              # the hierachy, stop adjusting for those items
              filter(pop_adjust_at < orig_indent)
          }

          if (orig_indent > last(new_stack$pop_adjust_at)) {
            # All the items below on the outline should be adjusted backwards
            new_stack <- new_stack %>% add_row(
              adjust = last(new_stack$pop_adjust_at)+1 - orig_indent,
              pop_adjust_at = orig_indent
            )
          }

          temp %>%
            add_row(
              orig_indent = orig_indent,
              stack = list(new_stack),
              adjust = sum(new_stack$adjust),
              indent = orig_indent + adjust
            )
        })

    ) %>%
    ungroup() %>%
    select(file, title, type, line, indent)

  colors <- c(
    cli::col_blue,
    cli::col_magenta,
    cli::col_cyan,
    cli::col_green,
    cli::col_yellow,
    cli::col_red
  )

  # Style outline ----
  outline_styled <- outline_new %>%
    rowwise() %>%
    mutate(
      # Select only fields we mention
      .keep = "used",

      link_to_line = cli::style_hyperlink(
        "#", str_c("file://", file), params = list(line = line, col = 1)),

      # Processing how title displays based on type
      print_title = type %>% case_match(

        # By default, just rainbow color titles by indent
        .default = title %>% colors[[(indent %% length(colors)) + 1]](),

        # For TODO items, highlight special and provide link completion link
        "todo_fixme" ~ str_c(
          str_extract(
            title, "(?<!\"#\\s)(TODO|FIXME|BOOK|(?<!\")WORK[^I``])\\:?\\s*(.+)",
            group = 1) %>%
            cli::style_bold(),
          " ",
          str_extract(
            title, "(?<!\"#\\s)(TODO|FIXME|BOOK|(?<!\")WORK[^I``])\\:?\\s*(.+)",
            group = 2) %>%
            cli::style_italic()
        ) %>%
          cli::col_silver()

      ),
      print_line = str_c(link_to_line, " ", print_title)
    ) %>%
    ungroup()

  # Print outline data tree ----
  outline_styled %>%
    mutate(
      # Give items IDs so titles do not have to be unique
      item_id = row_number() %>% as.character(),
      indent_wider = indent,
      x = T
    ) %>%

    # We need these wide cumsum `header1` type fields to determine which items belong to which parents
    pivot_wider(names_from = indent_wider, values_from = x, values_fill = F, names_prefix = "header") %>%
    mutate(across(starts_with("header"), cumsum)) %>%

    # For each row, pick the IDs of all direct children from the outline
    pmap(function(...) with(list(..., childdata = .), tibble(
      # file,
      # file_short,
      # title,
      print_line,
      # indent,
      item_id,
      # type,
      root_id = header0,
      parent_level_id = get(str_c("header", indent)),
      children_ids = childdata %>%
        # TODO: use data prefix? get around with() in general
        rename(childindent = indent) %>%
        filter(
          childindent == indent+1,
          cumsum(childindent == indent) == parent_level_id
        ) %>%
        pull(item_id) %>% list()
    ))) %>%
    list_rbind() %>%
    # View()
    select(item_id, children_ids, print_line, everything()) %>%
    # Print tree for each root node
    group_by(root_id) %>%
    group_walk(\(data, group) {
      data %>%
        select(item_id, children_ids, print_line) %>%
        cli::tree() %>%
        # cli::tree(style = list(h = "━━━", v = "┃", l = "┗", j = "┣")) %>%
        cat(sep = "\n")
    })

  invisible(outline)

  # test2 <- test
  # needs_collapse <- T
  # while(needs_collapse) {
  #
  #   row_to_collapse <- test2 %>%
  #     filter(map_int(children_ids, length) == 1) %>%
  #     unnest(children_ids) %>%
  #
  #     left_join(
  #       test %>%
  #         select(item_id, child_type = type, child_title = title, child_children = children_ids),
  #       join_by(children_ids == item_id)
  #     ) %>%
  #     filter(child_type != "todo_fixme") %>%
  #     head(1)
  #
  #   row_child <- test2 %>%
  #     filter(item_id == row_to_collapse$children_ids)
  #
  #   # if (row_child$type == "todo_fixme") {}
  #
  #   row_parent <- test2 %>%
  #     unnest(children_ids) %>%
  #     filter(children_ids == row_to_collapse$item_id)
  #
  #   test2 <- test2 %>%
  #     filter(item_id != row_to_collapse$item_id) %>%
  #     rows_update(tibble(
  #       item_id = row_child$item_id,
  #       children_ids = row_child$children_ids,
  #       title = str_c(row_to_collapse$title, " -> ", row_child$title),
  #       type = row_child$type,
  #     ), by = "item_id")
  #
  #   test2 %>% select(item_id, children_ids, title) %>%
  #     cli::tree()
  #     # left_join(
  #     #   test %>%
  #     #     select(item_id, child_type = type, child_title = title, child_children = children_ids),
  #     #   join_by(children_ids == item_id)
  #     # ) %>%
  #     # left_join(
  #     #   test %>%
  #     #     unnest(children_ids) %>%
  #     #     select(parent_item_id = item_id, children_ids, parent_type = type, parent_title = title),
  #     #   join_by(item_id == children_ids)
  #     # ) %>%
  #     # filter(child_type != "todo_fixme") %>%
  #     # mutate(
  #     #   new_item_id = children_ids,
  #     #   new_title = case_when(
  #     #     type == "filler" ~ child_title,
  #     #     .default = str_c(title, " -> ", child_title)
  #     #   )
  #     # ) %>%
  #     # reframe(
  #     #   old_item_id = c(item_id, parent_item_id),
  #     #   title = c(new_title, parent_title),
  #     #   item_id = c(new_item_id, parent_item_id),
  #     #   children_ids = c(child_children, as.list(new_item_id))
  #     # )
  #
  # }
  #
  # test %>%
  #   filter(map_int(children_ids, length) == 1) %>%
  #   unnest(children_ids) %>%
  #   left_join(
  #     test %>%
  #       select(item_id, child_type = type, child_title = title),
  #     join_by(children_ids == item_id)
  #   ) %>%
  #   reframe(
  #     title = str_c(title, " -> ", child_title)
  #   )
   invisible(x)
}


construct_outline_link <- function(.data) {
  dir_common <- get_dir_common_outline(path = .data$file)
  is_saved_doc <- !any(.data$file == "unsaved-doc.R")
  is_active_doc <- length(unique(.data$file)) == 1L
  rs_avail_file_link <- is_rstudio("2023.09.0.375") # better handling after
  .data <- define_important_element(.data)

  if (is.null(dir_common) || !nzchar(dir_common)) {
    dir_common <- "Don't remove anything if not null"
  }
  .data$rs_version <- ifelse(!is_rstudio("2023.12.0.274") && is_rstudio(), ".", "")
  .data$has_inline_markup <- dplyr::coalesce(stringr::str_detect(.data$outline_el, "\\{|\\}"), FALSE)
  .data$is_saved_doc <- is_saved_doc
  # Only show `complete_todo()` links for TODO.R files or active file in interactive sessions
  # Using rlang::is_interactive to be able to test it if I ever feel the need.
  .data$complete_todo_link <- rlang::is_interactive() & .data$is_todo_fixme & (is_active_doc | grepl("TODO.R", .data$file, fixed = TRUE))
  .data <- dplyr::mutate(
    .data,
    # to create `complete_todo()` links (only with active doc + is_todo_fixme) (and truncate if necessary)
    condition_to_truncate = !is.na(outline_el) & !has_title_el & (complete_todo_link) & is_saved_doc & !has_inline_markup,
    # Truncate todo items, subtitles
    condition_to_truncate2 = !is.na(outline_el) & !has_title_el & (is_todo_fixme & !complete_todo_link) & (is_second_level_heading_or_more | is_subtitle) & is_saved_doc & !has_inline_markup
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
      is.na(outline_el2) & !is.na(outline_el) & !has_title_el & complete_todo_link & is_saved_doc,
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

  dplyr::mutate(.data,
    # link_rs_api = paste0("{.run [", outline_el, "](reuseme::open_rs_doc('", file_path, "', line = ", line, "))}"),
    link_rs_api = dplyr::case_when(
      is.na(outline_el2) ~ NA_character_,
      !is_saved_doc ~ paste0("line ", line, " -", outline_el2),
      rs_avail_file_link ~ paste0(
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
    condition_to_truncate2 = NULL
  )
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
  dat <- dplyr::filter(
    .data,
    (is_news & (
      (!simplify_news & is_section_title & before_and_after_empty) |
        (simplify_news & is_section_title & !pkg_version %in% versions_to_drop & !is_second_level_heading_or_more & before_and_after_empty)
    )) |
      # still regular comments in .md files
      # what to keep in .md docs

      (is_md & (is_chunk_cap | (is_section_title & before_and_after_empty))) |
      # What to keep in .R files
      (!is_md & is_section_title_source) |
      # What to keep anywhere
      is_tab_or_plot_title | is_todo_fixme | is_test_name | is_cross_ref | is_function_def | is_doc_title # | is_cli_info # TODO reanable cli info
  )
  dat$simplify_news <- NULL
  dat

  # Remove duplicate outline elements
  if (anyDuplicated(dat$content) > 0L) {
    dat <- scrub_duplicate_outline(dat)
  }
  dat
}
# Remove duplicated entries from outline
# for example, snapshots will have priority and will not return both the snapshot and the original test
scrub_duplicate_outline <- function(x) {
  x$order <- seq_len(nrow(x))
  # outline = NA (title)
  #
  x <- dplyr::mutate(x, n_dup = dplyr::n(), .by = "content")
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
    by = "content"
  )
  # use the previous order
  x <- dplyr::arrange(x, .data$order)
  x$points <- NULL
  x$order <- NULL
  x$n_dup <- NULL
  x
}

#
# Includes removing headings comments
# Remove title =
# Removing quotes, etc.
display_outline_element <- function(.data) {
  x <- .data
  org_repo <- find_pkg_org_repo(unique(x$file))
  if (!is.null(org_repo)) {
    x$outline_el <- link_local_gh_issue(x$content, org_repo)
  } else {
    x$outline_el <- x$content
  }
  x$outline_el <- purrr::map_chr(x$outline_el, \(x) link_gh_issue(x, org_repo)) # to add link to GitHub.
  x$outline_el <- purrr::map_chr(x$outline_el, markup_href)
  x <- dplyr::mutate(
    x,
    outline_el = dplyr::case_when(
      is_todo_fixme ~ stringr::str_extract(outline_el, "(TODO.+)|(FIXME.+)|(WORK.+)|(BOOK.+)"),
      is_test_name ~ stringr::str_extract(outline_el, "(test_that|describe)\\(['\"](.+)['\"],\\s?\\{", group = 2),
      is_cli_info ~ stringr::str_extract(outline_el, "[\"'](.{5,})[\"']") |> stringr::str_remove_all("\""),
      is_tab_or_plot_title ~ stringr::str_extract(outline_el, "title =[^\"']*[\"']([^\"]{5,})[\"']", group = 1),
      is_chunk_cap_next & !is_chunk_cap ~ stringr::str_remove_all(outline_el, "\\s?\\#\\|\\s+"),
      is_chunk_cap ~ stringr::str_remove_all(stringr::str_extract(outline_el, "(cap|title)\\:\\s*(.+)", group = 2), "\"|'"),
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
    is_subtitle = (is_tab_or_plot_title | is_doc_title) & grepl("subt", content, fixed = TRUE),
  )

  y <- dplyr::mutate(
    x,
    has_title_el =
      ((line == 1 & !is_todo_fixme & !is_test_name & !is_snap_file & !is_cross_ref) |
        (is_doc_title & !is_subtitle & !is_snap_file & !is_second_level_heading_or_more)) & !is_news,
    .by = "file"
  )
  y <- withCallingHandlers(
    dplyr::mutate(y,
      title_el_line = ifelse(has_title_el, line[
        (line == 1 & !is_todo_fixme & !is_test_name & !is_snap_file & !is_cross_ref) |
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
    y <- dplyr::mutate(
      y,
      title_el = na_if0(title_el[!is.na(title_el)], "title"),
      title_el_line = na_if0(title_el_line[!is.na(title_el_line)], "line"),
      .by = "file"
    )
  }
  y
}

define_important_element <- function(.data) {
  dplyr::mutate(
    .data,
    importance = dplyr::case_when(
      is_second_level_heading_or_more | is_chunk_cap | is_cli_info | is_todo_fixme | is_subtitle | is_test_name ~ "not_important",
      .default = "important"
    )
  )
}

remove_outline_columns <- function(.data) {
  dplyr::mutate(.data,
    style_fun = NULL,
    is_saved_doc = NULL,
    is_roxygen_comment = NULL,
    is_notebook = NULL,
    complete_todo_link = NULL,
    is_news = NULL,
    # I may put it back ...
    importance = NULL,
    # may be useful for debugging
    before_and_after_empty = NULL,
    # may be useful for debugging
    has_inline_markup = NULL
  )
}

trim_outline <- function(x, width) {
  # problematic in case_when
  cli::ansi_strtrim(x, width = width)
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

get_dir_common_outline <- function(path) {
  path <- unique(path)
  if (rlang::has_length(path, 1)) {
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

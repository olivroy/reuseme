#' Rename an output or a data file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function can improve your workflow.
#' It is inspired by [usethis::rename_files()], but its scope
#' is more oriented towards analysis script.
#'
#' # Use case
#' Let's say you have an analysis and work on a certain subject.
#' You want to rename a figure for clarity.
#'
#' Here is what `rename_files2()` does for you, before it renames files.
#'
#' 1. Look for potential name conflict
#' 2. Look for data frame name conflicts
#' 3. Sends information to clipboard
#' @inheritParams usethis::rename_files
#' @param force Whether to force renaming if there are conflicts
#' @param action One of `"rename"` or `"test"`
#'
#' @export
rename_files2 <- function(old, new, force = FALSE, action = c("rename", "test")) {
  action <- rlang::arg_match(action)

  if (!rlang::is_interactive()) {
    return()
  }

  # Still a bit buggy.. Will have to look more closely eventually.
  fs::path_real(old) # Will fail immediately if it doesn't exist.
  if (fs::is_dir(old)) {
    cli::cli_abort("Can't rename directories with this function See {.fn fs::dir_copy} and {.fn fs::dir_delete}.")
  }

  # TODO don't fail if testing?
  if (fs::file_exists(new) && !force) {
    cli::cli_abort(c(
      "Can't rename file to {.val {new}}",
      "!" = "{.arg new} already exists {.path {new}}.",
      "i" = "Use {.code force = TRUE} to override."
    ))
  }

  is_git <- !isFALSE(tryCatch(rprojroot::find_root_file(criterion = rprojroot::criteria$is_vcs_root), error = function(e) FALSE))
  if (interactive() && !is_git) {
    cli::cli_warn(c(
      "It is better to use this function in a version-controlled repository.",
      i = "See {.help usethis::use_git} for help."
    ))
  }
  # looking for the object name as well if changing from a file name to another
  path_file_name <- fs::path_ext_remove(old)
  file_name_base <- fs::path_file(path_file_name)
  new_name_base <- fs::path_file(fs::path_ext_remove(new))

  # don't check for regexp if the original file name has less than min_n_char
  min_n_char <- 5
  cnd_check_for_object_names <-
    file_name_base != new_name_base &&
      !file_name_base %in% (c("index", "temp")) &&
      nchar(file_name_base) > min_n_char

  if (cnd_check_for_object_names) {
    object_snake_from_file_kebab <- stringr::str_replace_all(file_name_base, "-", "_")
    regex_file_name <- paste0(c(object_snake_from_file_kebab, old), collapse = "|")
  } else {
    regex_file_name <- paste0(path_file_name, "[^-]")
  }

  related_files <- fs::dir_ls(regexp = paste0(regex_file_name, "\\."), recurse = TRUE)
  related_files <- setdiff(related_files, old)
  if (length(related_files) > 0) {
    cli::cli_warn(c(
      "Other files have a similar pattern",
      "See {.file {related_files}}",
      "No support yet for that"
    ))
  }
  if (!force) {
    extra_msg_if_file_conflict <- c(
      x = "Did not rename files!",
      "!" = paste0("Found references to {.val ", old,"} in project"),
      i = paste0("Change file path to {.val ", new, "} or see {.run [Find in Files](rstudioapi::executeCommand('findInFiles'))} Replace All if confident. {.emph Copied new name to clipboard}"),
      i = "Also change object names to snake_case that follow the new file name."
    )
  } else {
    extra_msg_if_file_conflict <- c("Here are the conflicts. Review changes carefully", "renaming file anyway")
  }

  verbose <- cnd_check_for_object_names | length(related_files) > 0 | force

  # avoid searching in generated files and tests/testthat files
  file_names_conflicts <- fs::dir_ls(regexp = "ya?ml$|md$|R$", type = "file", recurse = TRUE) |>
    fs::path_filter(regexp = "_files|tests/testthat", invert = TRUE) |> # need to do elsewhere too
    solve_file_name_conflict(
      regex = regex_file_name,
      dir = ".",
      extra_msg = extra_msg_if_file_conflict,
      quiet = FALSE,
      what = paste0("to {.val ", file_name_base, "}")
    )

  if (!force && file_names_conflicts) {
    cli::cli_bullets("Rerun the code to make it work or use `force = TRUE`")
  }


  if (!file_names_conflicts || force) {
    rename_file_action(new, old, force, action, verbose)
    # Can't remember why I put this here?
    # Seems to query all-non existent files, only if renaming?
    # check_referenced_files(path = ".", quiet = !verbose)
    cli::cli_inform(c(
      i = "Call {.run reuseme::check_referenced_files()} to see if there are dead links in dir."
    ))
    return(invisible(new))
  }

  # readr::write_lines(new_name, file = readr::clipboard())
  if (.Platform$OS.type == "windows") {
    utils::write.table(new, file = "clipboard", eol = "", row.names = FALSE, col.names = FALSE)
  } else if (interactive()) {
    rlang::check_installed("clipr")
    if (clipr::clipr_available()) clipr::write_clip(new)
  }
  new
}


# Worth a simple write-up!
# Checks if a file is referenced somewhere in the docs before renaming
# Could be extended to crossrefs!
#' Check if files referenced in source files exist in a current dir
#'
#' 1. It goes through the source files (.R/.qmd etc.),
#' 2. It identifies data files (.csv, .xlsx) read or written
#' 3. Search on the system if these files exist.
#'
#' Still WIP, so you can add code for false positive as needed.
#'
#' @param path a directory to search for
#' @param quiet Whether it should print messages?
#' @returns A logical
#' @export
#' @keywords internal
#' @details
#' To find genuine referenced files, we exclude different paths
#'
#' 1. Those created with `fs::path()` or `file.path()` or `glue::glue()`
#' 2. Those that are checked for `fs::file_exists()`, `file.exists()`
#' 3. Deleted with `fs::file_delete()`, `unlink()`
check_referenced_files <- function(path = ".", quiet = FALSE) {
  # TODO insert in either proj_outline, or rename_file
  if (path == "." || fs::is_dir(path)) {
    path <- fs::dir_ls(path = path, recurse = TRUE, regexp = "\\.(R|md|ml)$")
    path <- fs::path_filter(path = path, regexp = "_files|tests/testthat", invert = TRUE) # need to do this in 2 places
  } else if (fs::path_ext(path) %in% c("R", "yml", "yaml", "Rmd", "md", "qmd", "Rmarkdown")) {
    path <- path
  } else {
    cli::cli_abort("Wrong specification.")
  }

  # Create a list of genuine referenced files
  # TODO Add false positive references
  # TODO fs::path and file.path should be handled differently
  # TODO probably needs a `detect_genuine_path()`
  referenced_files <- path |>
    purrr::map(\(x) readLines(x, encoding = "UTF-8")) |>
    purrr::list_c(ptype = "character") |>
    stringr::str_subset(pattern = "\\:\\:dav.+lt|\\:\\:nw_|g.docs_l.n|target-|\\.0pt", negate = TRUE) |> # remove false positive from .md files
    stringr::str_subset(pattern = "file.path|fs\\:\\:path\\(", negate = TRUE) |> # Exclude fs::path() and file.path from search since handled differently.
    stringr::str_subset(pattern = "file.[(exist)|(delete)]|glue\\:\\:glue|unlink", negate = TRUE) |> # don't detect where we test for existence of path or construct a path with glue
    stringr::str_subset(pattern = "[(regexp)|(pattern)]\\s\\=.*\".*[:alpha:]\"", negate = TRUE) |> # remove regexp = a.pdf format
    stringr::str_subset(pattern = "grepl?\\(|stringr", negate = TRUE) |> # avoid regexp
    stringr::str_subset(pattern = "nocheck", negate = TRUE) |> # remove nocheck and unlink statements (refers to deleted files anywa)
    stringr::str_subset("\"") |>
    stringr::str_trim() |>
    stringr::str_extract_all("\"[^\"]+\"") |>
    unlist() |>
    stringr::str_remove_all("\",?") |>
    stringr::str_subset(pattern = "\\.\\w{1,6}$") |> # file pattern
    stringr::str_subset(pattern = "\\.plot|\\.fr$|\\.frame|\\.obs$|\\.\\d{2,}$", negate = TRUE) |> # Manually add file exts that are not file exts.
    stringr::str_subset(pattern = "tmp|temp", negate = TRUE) |> # remove common file names that are not very nice
    stringr::str_subset(pattern = "https?", negate = TRUE) |> # doesn't check for files read online.
    stringr::str_subset(pattern = "\\@.+\\.", negate = TRUE) |> # email addresses or containing @
    stringr::str_subset(pattern = "_fichiers/", negate = TRUE) |> # manually remove false positive
    stringr::str_subset(pattern = "\n", negate = TRUE) |> # remove things with line breaks
    stringr::str_subset(pattern = "^\\.[:alpha:]{1,4}$", negate = TRUE) |> # remove reference to only file extensions
    stringr::str_subset(pattern = "\\.\\d+$", negate = TRUE) |> # remove 0.000 type
    purrr::set_names()

  files_detected <- unique(referenced_files)
  references_a_non_existent_file <- !(fs::file_exists(files_detected) | file.exists(files_detected)) # to avoid burden for now.
  if (!any(references_a_non_existent_file)) {
    return(invisible())
  }
  non_existent_files <- files_detected[references_a_non_existent_file]
  if (quiet) {
    cli::cli_warn(
      c(
        "Found {length(non_existent_files)} referenced file{?s} in folder.",
        i = "See {.help reuseme::check_referenced_files} for more info.",
        "There are locations in source files (qmd, Rmd, R) where a non-existent file (.csv, .xlsx etc.) is referenced.",
        "Run {.code check_referenced_files(quiet = FALSE)} to see where this file is referenced.",
        "{non_existent_files_show}"
      ),
      call = expr(check_referenced_files())
    )
  } else {
    solve_file_name_conflict(
      files = path,
      regex = paste0(non_existent_files, collapse = "|"),
      extra_msg = "Check in source files and rename the referenced (csv, xlsx etc.) files accordingly.",
      quiet = quiet,
      what = "to non-existent files"
    )
  }
  invisible(non_existent_files)
}

#' Check if outdated or non-existent file is.
#'
#' If `quiet = FALSE` (default) will give a hint of where the file is referenced.
#'
#' @param files which files to search in
#' @param regex a regex related to the file name to search for
#' @param dir A directory where to operate
#' @param extra_msg Extra message to pass
#' @param what Which file conflicts we talking about
#' @param quiet A logical, informs where the occurrences are found. (Default, `FALSE`)
#'
#' @return `FALSE` if no occurrences were found. `TRUE` if non-existent files
#'   are referenced
#' @export
#' @keywords internal
solve_file_name_conflict <- function(files, regex, dir = ".", extra_msg = NULL, quiet = FALSE, what = NULL) {
  regex <- stringr::str_replace_all(regex, "\\\\|\\)|\\(|\\}\\{\\?|\\$|~", ".")
  # regex <- as.character(regex)
  if (dir != ".") {
    cli::cli_abort("Don't know how to do this.")
  }
  bullets_df <- files |>
    purrr::set_names() |>
    purrr::map(\(x) readLines(x, encoding = "UTF-8")) |>
    purrr::map(\(x) tibble::enframe(x, name = "line_number", value = "content")) |>
    dplyr::bind_rows(.id = "file")

  bullets_df <- bullets_df[grepl(regex, bullets_df$content), ]

  if (nrow(bullets_df) == 0) {
    return(FALSE)
  }


  if (!quiet) {
    lines_match <- bullets_df$line_number
    # Derive column match
    start_end_pos <- stringr::str_locate(bullets_df$content, regex)
    cols_match <- dplyr::coalesce(
      start_end_pos[, "start"] - 1L,
      0L
    )
    # Create hyperlinks from lines and columns
    bullets <- stringr::str_glue(
      "{{.file {bullets_df$file}:{lines_match}:{cols_match}}}"
    )

    if (length(bullets) > 20) {
      display_msg <- cli::format_inline("[10 first references only]")
      # Showing First ten to avoid screen overflow.
      bullets_to_display <- cli::ansi_collapse(bullets[seq_len(10)])
    } else {
      display_msg <- NULL

    bullets_to_display <- cli::ansi_collapse(bullets)
    }
    # Remove duplicated Found x references
    which_bullet_to_replace <- stringr::str_subset(extra_msg, "Found references to", negate = T)
    # possibly just move up our
    #extra_msg[i] <-
    cli::cli_bullets(c(
      extra_msg,
      "i" = paste0("Found {length(bullets)} reference{?s} ", what, " in ", bullets_to_display, "."),
      display_msg
    ))
  } else {
    cli::cli_inform(
      c(
        extra_msg,
        "run {.run reuseme::check_referenced_files(quiet = TRUE)} to see where the conflicts are."
      ),
      .frequency = "always", .frequency_id = "nonexistantfiles"
    )
  }

  invisible(TRUE)
}

# put into own function
# arguments should all be checked
# Performs the action of renaming file
rename_file_action <- function(new, old, force, action, verbose) {
  if (tools::file_ext(old) == "R" && action == "test") {
    # usethis::rename_files(old_name, new_name)
    cli::cli_inform("See if need for tests change snapshots")
  }
  if (tools::file_ext(new) %in% c("png")) {
    cli::cli_inform(
      c(
        "Use in markdown/quarto docs (source mode) with",
        '![]({new}){{fig-alt="" width="70%"}}'
      )
    )
  }

  if (action == "rename") {
    fs::file_move(old, new)
    if (!force) {
      cli::cli_alert_success("Renamed file to {.file {new}} without issue.")
    } else {
      cli::cli_alert_danger("Renamed file to {.file {new}} by force. Be careful.")
    }
  } else if (action == "test") {
    # Not renaming, but going through the same path as I would have
    cli::cli_inform("Testing mode, did not rename file")
  }
}

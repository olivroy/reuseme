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
  is_git <- !isFALSE(tryCatch(rprojroot::find_root_file(criterion = rprojroot::criteria$is_vcs_root), error = function(e) FALSE))
  if (is_git) {
    cli::cli_warn("It is better to use this function in a version-controlled repository.")
  }
  # looking for the object name as well if changing from a file name to another
  path_file_name <- fs::path_ext_remove(old)
  file_name_base <- fs::path_file(path_file_name)
  new_name_base <- fs::path_file(fs::path_ext_remove(new))

  cnd_check_for_object_names <- file_name_base != new_name_base & !file_name_base %in% (c("index", "temp"))

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
      "Did not rename files!",
      i = "Make sure you change the file path to",
      new,
      "in these locations (`new_name` copied to clipboard) or see {.run [Find in Files](rstudioapi::executeCommand('findInFiles'))} Replace All if confident.",
      i = "Consider changing name snake_case objects that follow the file names",
      i = "Use {.kbd Ctrl + C}, then {.kbd Ctrl + Shift + Up} for replacing"
    )
  } else {
    extra_msg_if_file_conflict <- c("Here are the conflicts. Review changes carefully", "renaming file anyway")
  }

  verbose <- cnd_check_for_object_names | length(related_files) > 0 | force



  file_names_conflicts <- fs::dir_ls(regexp = "ya?ml$|md$|R$", type = "file", recurse = TRUE) |>
    solve_file_name_conflict(regex = regex_file_name, dir = ".", extra_msg = extra_msg_if_file_conflict, quiet = FALSE)

  if (!force && file_names_conflicts) {
    cli::cli_bullets("Rerun the code to make it work or use `force = TRUE`")
  }


  if (!file_names_conflicts || force) {
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

    if (action == "test") {
      cli::cli_inform("Testing mode, did not rename file")
      return(invisible(new))
    }

    fs::file_move(old, new)

    if (force) {
      cli::cli_alert_success("Renamed file to {.file {new}} without issue.")
    } else {
      cli::cli_alert_danger("Renamed file to {.file {new}} by force. Be careful.")
    }

    check_files_exist_in_dir(path = ".", quiet = !verbose)
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
#' @param path a directory to search for
#' @param quiet Whether it should print messages?
#' @returns A logical
#' @export
#' @keywords internal
check_files_exist_in_dir <- function(path = ".", quiet = FALSE) {
  # TODO insert in either proj_outline, or rename_file
  if (path == "." || fs::is_dir(path)) {
    path <- fs::dir_ls(path = path, recurse = TRUE, regexp = "\\.(R|md|ml)$")
  } else if (fs::path_ext(path) %in% c("R", "yml", "yaml", "Rmd", "md", "qmd")) {
    path <- path
  } else {
    cli::cli_abort("Wrong specification.")
  }
  referenced_files <- path |>
    purrr::map(\(x) readLines(x, encoding = "UTF-8")) |>
    purrr::list_c(ptype = "character") |>
    stringr::str_subset("\"") |>
    stringr::str_trim() |>
    stringr::str_extract_all("\"[^\"]+\"") |>
    unlist() |>
    stringr::str_remove_all("\",?") |>
    stringr::str_subset(pattern = "\\.\\w{1,6}$") |> # file pattern
    stringr::str_subset(pattern = "\\.plot|\\.fr$|\\.frame", negate = TRUE) |> # Manually add file exts that are not file exts.
    stringr::str_subset(pattern = "tmp|temp", negate = TRUE) |> # remove common file names that are not very nice
    stringr::str_subset(pattern = "https?", negate = TRUE) |> # doesn't check for files read online.
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
        "There are locations in source files (qmd, Rmd, R) where a non-existent file (.csv, .xlsx etc.) is referenced.",
        "Run {.code check_files_exist_in_dir(quiet = FALSE)} to see where this file is referenced.",
        "{non_existent_files}"
      ),
      call = expr(check_files_exist_in_dir())
    )
  } else {
    solve_file_name_conflict(
      files = path,
      regex = paste0(non_existent_files, collapse = "|"),
      extra_msg = "Check in source files and rename the referenced (csv, xlsx etc.) files accordingly.",
      quiet = quiet
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
#' @param quiet A logical, informs where the occurrences are found. (Default, `FALSE`)
#' @return `FALSE` if no occurrences were found. `TRUE` if non-existent files
#'   are referenced
#' @export
#' @keywords internal
solve_file_name_conflict <- function(files, regex, dir = ".", extra_msg = NULL, quiet = FALSE) {
  regex <- stringr::str_replace_all(regex, "\\\\|\\)|\\(|\\}\\{\\?|\\$|~", ".")
  # regex <- as.character(regex)
  if (dir != ".") {
    cli::cli_abort("Don't know how to do this.")
  }
  bullets_df <- files |>
    purrr::set_names() |>
    purrr::map(\(x) readLines(x, encoding = "UTF-8")) |>
    purrr::map(\(x) tibble::enframe(x, name = "line_number", value = "content")) |>
    dplyr::bind_rows(.id = "file") |>
    dplyr::filter(
      stringr::str_detect(content, regex)
    )
  if (nrow(bullets_df) == 0) {
    return(FALSE)
  }


  if (!quiet) {
    bullets <-
      dplyr::mutate(
        bullets_df,
        col_number = stringr::str_locate(.data$content, regex)[, "start"] - 1,
        col_number = dplyr::coalesce(.data$col_number, 0L),
        hyperlink = stringr::str_glue(
          "{{.file {file}:{line_number}:{col_number}}}"
        )
      )
    bullets <- dplyr::pull(bullets)

    cli::cli_bullets(c(
      extra_msg,
      bullets
    ))
  } else {
    cli::cli_inform(
      c(
        extra_msg,
        "run {.run reuseme::check_file_exist_in_dir(quiet = TRUE)} to see where the conflicts here."
      ),
      .frequency = "always", .frequency_id = "nonexistantfiles"
    )
  }

  invisible(TRUE)
}

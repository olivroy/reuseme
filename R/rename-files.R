#' Rename an output or a data file and watch for references
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function can improve your workflow.
#' It is inspired by [usethis::rename_files()], but its scope
#' is more oriented towards analysis script.
#'
#' # Use case
#'
#' Let's say you have an analysis and work on a certain subject.
#' You want to rename a figure for clarity.
#' For example, you had an input file named `data/my-streets.csv` and you now want to
#' rename it to
#'
#' Here is what `rename_files2()` does for you, before it renames files.
#'
#' 1. Look for potential name conflict
#' 2. Look for data frame name conflicts
#' 3. Sends information to clipboard
#'
#' Will work well for you if you tend to name your objects using snake case and
#' naming objects with snake case or kebab-case.
#'
#' The philosophy is to inform you of manual steps required before actually
#' performing file renaming.
#'
#' A way to be less strict is to us
#' @inheritParams usethis::rename_files
#' @param force Whether to force renaming if there are conflicts. Use `warn_conflicts = "none"`
#' @param action One of `"rename"` or `"test"`
#' @param warn_conflicts One of
#' * `"default"`: will be `"all"` if only moving directory, and `"all"` otherwise.
#' * `"all"` (stricter: if `old = "data/my-streets.csv"` will check for objects named `my_streets`, other files like `my-streets.R`, etc.),
#' * `"exact"` will only search for `"data/my-streets.csv"` in documents
#'   `"none"` will not search for references in documents and will rename.
#' @export
#' @returns `new` if renaming succeeded. Mostly called for its side-effects
rename_files2 <- function(old, new, warn_conflicts = c("default","all", "exact", "none"), force = FALSE, action = c("rename", "test")) {
  action <- rlang::arg_match(action)
  # TODO separate all this in helper functions
  if (action == "default") {
    action <- "all"
  }
  warn_conflicts <- rlang::arg_match(warn_conflicts)

  if (action == "none") {
    force <- TRUE
  }

  # Still a bit buggy.. Will have to look more closely eventually.
  # fs::path_real(old) # Will fail immediately if it doesn't exist.
  if (fs::is_dir(old) || fs::is_dir(new)) {
    cli::cli_abort("Can't rename directories with this function See {.fn fs::dir_copy} and {.fn fs::dir_delete}.")
  }
  if (!fs::is_file(old)) {
    cli::cli_abort("Can't rename {.file {old}} as it does not exist. Supply {.arg old} an existing file.")
  }
  if (fs::path_ext(old) != fs::path_ext(new)) {
    cli::cli_abort(c(
      "!" = "{.arg new} and {.arg old} must have the same file extension, not\\
          {.val {fs::path_ext(old)}} and {.val {fs::path_ext(new)}}."
    ))
  }
  # TODO don't fail if testing?
  if (fs::file_exists(new) && !force) {
    cli::cli_abort(c(
      "Can't rename file to {.val {new}}",
      "!" = "{.arg new} already exists {.path {new}}.",
      "i" = "Use {.code warn_conflicts = 'none'} to override."
    ))
  }

  # renaming should only happen in tests or interactive sessions
  if (action == "rename" && (!rlang::is_interactive() || !identical(Sys.getenv("TESTTHAT"), "true"))) {
    cli::cli_inform(c("Should only rename files in interactive sessions (or in tests)"))
  }

  is_git <- !isFALSE(tryCatch(rprojroot::find_root_file(criterion = rprojroot::criteria$is_vcs_root), error = function(e) FALSE))
  if (interactive() && !is_git && !identical(Sys.getenv("TESTTHAT"), "true")) {
    cli::cli_warn(c(
      "It is better to use this function in a version-controlled repository.",
      i = "See {.help usethis::use_git} for help."
    ))
  }
  # looking for the object name as well if changing from a file name to another
  path_file_name <- fs::path_ext_remove(old)
  file_name_base <- fs::path_file(path_file_name)
  new_name_base <- basename_remove_ext(new)

  # don't check for regexp if the original file name has less than min_n_char
  min_n_char <- 5
  cnd_check_for_object_names <-
    file_name_base != new_name_base &&
    !file_name_base %in% (c("index", "temp")) &&
    nchar(file_name_base) > min_n_char &&
    !tolower(fs::path_ext(old)) %in% c("png", "jpg", "jpeg", "pdf", "svg") && # don't check for figures
    warn_conflicts == "all" # check for all problems

  if (cnd_check_for_object_names) {
    object_snake_from_file_kebab <- stringr::str_replace_all(file_name_base, "-", "_")
    regex_file_name <- paste0(c(object_snake_from_file_kebab, old), collapse = "|")
  } else if (warn_conflicts == "exact") {
    # if generic name, search exclusively for file extension
    regex_file_name <- file_name_base
  } else {
    # if objects are called
    regex_file_name <- paste0(path_file_name, "[^-]?")
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
      "!" = paste0("Found references to {.val ", old, "} in project"),
      i = paste0("Change file path to {.val ", new, "} in files ahead of renaming file or see {.run [Find in Files](rstudioapi::executeCommand('findInFiles'))} Replace All if confident. {.emph Copied new name to clipboard}"),
      if (!is_moving(old, new)) i = "Also change object names to snake_case that follow the new file name."
    )
  } else {
    extra_msg_if_file_conflict <- c("Here are the conflicts. Review changes carefully", "renaming file anyway")
  }

  verbose <- cnd_check_for_object_names | length(related_files) > 0 | force
  # Either the file name base or the full file name
  what_are_we_looking_for <- ifelse(cnd_check_for_object_names, file_name_base, old)
  what_are_we_looking_for <- paste0("to {.val ", what_are_we_looking_for, "}")
  # avoid searching in generated files and tests/testthat files
  file_names_conflicts <- fs::dir_ls(regexp = "ya?ml$|md$|R$", type = "file", recurse = TRUE) |>
    fs::path_filter(regexp = "_files|tests/testthat", invert = TRUE) |> # need to do elsewhere too
    solve_file_name_conflict(
      regex = regex_file_name,
      dir = ".",
      extra_msg = extra_msg_if_file_conflict,
      quiet = FALSE,
      what = what_are_we_looking_for # either full path or basename.
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
# Helpers -------

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

compute_conflicts_regex <- function(old, new, action) {
  cli::cli_abort("todo.")

  if (cnd_check_for_object_names) {
    object_snake_from_file_kebab <- stringr::str_replace_all(file_name_base, "-", "_")
    regex_file_name <- paste0(c(object_snake_from_file_kebab, old), collapse = "|")
  } else if (match_conflicts == "exact") {
    # if generic name, search exclusively for file extension
    regex_file_name <- file_name_base
  } else {
    # if objects are called
    regex_file_name <- paste0(path_file_name, "[^-]?")
  }
}

compute_action <- function(old, new, action) {
  dplyr::case_when(
    action == "none" ~ "none",
    is_moving(old, new) ~ "exact",
    is_adding_a_suffix(old, new) ~ "default", # is it correct? like moving data to data-raw
    .default = "all"
   )
}

is_moving <- function(old, new) {
  fs::path_file(old) == fs::path_file(new)
}

is_adding_a_suffix <- function(old, new) {
  # TODO measure of string proximity
  base_name_old <- basename_remove_ext(old)
  base_name_new <- basename_remove_ext(new)

  matches <- c(
    stringr::str_match(base_name_new, base_name_old),
    stringr::str_match(base_name_old, base_name_new)
  )
  matches <- matches[!is.na(matches)]

  if (length(matches) == 0) {
    # no matches
    return(FALSE)
  }

  shortest_char <- min(nchar(c(base_name_new, base_name_old))) - 1L
  if (nchar(matches) >= max(4, shortest_char)) {
    TRUE
  } else {
    FALSE
  }
}

basename_remove_ext <- function(x) {
  fs::path_ext_remove(basename(x))
}

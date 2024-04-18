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
#' @param overwrite whether to overwrite `new` if it already exists. Be careful.
#' @param force `r lifecycle::badge('deprecated')` Whether to force renaming if there are conflicts. Use `warn_conflicts = "none"`
#' @param action One of `"rename"` or `"test"`
#' @param warn_conflicts One of
#' * `"default"`: will be check more thoroughly depending on the situation. If only moving directory, and `"all"` otherwise.
#' * `"all"` (larger scope: if `old = "data/my-streets.csv|my_streets"` will check for objects named `my_streets`, other files like `my-streets.R`, etc.),
#' * `"exact"` will only search for `"data/my-streets.csv"` in documents
#'   `"none"` will not search for references in documents and will rename.
#' @export
#' @returns `new` if renaming succeeded. Mostly called for its side-effects
rename_files2 <- function(old,
                          new,
                          warn_conflicts = c("default", "all", "exact", "none"),
                          overwrite = FALSE,
                          action = c("rename", "test"),
                          force = deprecated()) {
  action <- rlang::arg_match(action)

  warn_conflicts <- rlang::arg_match(warn_conflicts)
  check_logical(overwrite)

  if (lifecycle::is_present(force)) {
    lifecycle::deprecate_warn(
      when = "0.0.9006",
      what = "rename_files2(force)",
      with = "rename_files2(warn_conflicts)",
      details = cli::format_inline(
        "{.arg overwrite} must be used with caution to allow overwriing {.code new}"
      )
    )
   if (isTRUE(force)) {
      warn_conflicts <- "none"
      overwrite <- TRUE
    }
  }

  # `overwrite` should only be used to overwrite a file, probably should rename to overwrite.
  check_proper_renaming_condition(old, new, overwrite)

  # renaming should only happen in tests or interactive sessions
  if (action == "rename" && !(rlang::is_interactive() || identical(Sys.getenv("TESTTHAT"), "true"))) {
    cli::cli_inform(c("Should only rename files in interactive sessions (or in tests)"))
    return(invisible(FALSE))
  }

  is_git <- !isFALSE(tryCatch(rprojroot::find_root_file(criterion = rprojroot::criteria$is_vcs_root), error = function(e) FALSE))
  if (interactive() && !is_git && !identical(Sys.getenv("TESTTHAT"), "true")) {
    cli::cli_warn(c(
      "It is better to use this function in a version-controlled repository.",
      i = "See {.fn usethis::use_git} for help."
    ))
  }
  # After here, we start doing some renaming real situations---
  renaming_strategy <- scope_rename(old, new, warn_conflicts)

  regexp_to_search_for_in_files <- compute_conflicts_regex(old, renaming_strategy)

  # FIXME doesn't fit now.
  related_files <- fs::dir_ls(regexp = paste0(basename_remove_ext(old), "\\."), recurse = TRUE)
  related_files <- setdiff(related_files, old)
  if (length(related_files) > 0) {
    cli::cli_warn(c(
      "Other files have a similar pattern",
      "See {.file {related_files}}",
      "No support yet for that yet.",
      "Think about what triggers this and add new rules  in {.fn scope_rename}"
    ))
  }


  if (renaming_strategy != "free_for_all") {
    extra_msg_if_file_conflict <- c(
      x = "Did not rename files!",
      "!" = paste0("Found references to {.val ", old, "} in project"),
      i = paste0("Change file path to {.val ", new, "} in files ahead of renaming file or \\
                  see {.run [Find in Files](rstudioapi::executeCommand('findInFiles'))} Replace All if confident. {.emph Copied new name to clipboard}"),
      if (!is_moving(old, new)) i <- "Also change object names to snake_case that follow the new file name."
    )
  } else {
    extra_msg_if_file_conflict <- c("Here are the conflicts. Review changes carefully", "renaming file anyway")
  }

  verbose <- TRUE # length(related_files) > 0 || renaming_strategy == "free_for_all"
  # Either the file name base or the full file name

  if (renaming_strategy == "object_names") {
    regex_friendly <- paste0(basename_remove_ext(old), "/", stringr::str_replace_all(basename_remove_ext(old), "-", "_"))
  } else {
    regex_friendly <- ifelse(renaming_strategy %in% c("object_names"), basename_remove_ext(old), old)
  }

  regex_friendly <- paste0("to {.val ", regex_friendly, "}")
  # avoid searching in generated files and tests/testthat files
  n_file_names_conflicts <- fs::dir_ls(regexp = "ya?ml$|md$|R$", type = "file", recurse = TRUE) |>
    fs::path_filter(regexp = "_files|tests/testthat", invert = TRUE) |> # need to do elsewhere too
    solve_file_name_conflict(
      regex = regexp_to_search_for_in_files,
      dir = ".",
      extra_msg = extra_msg_if_file_conflict,
      quiet = FALSE,
      what = regex_friendly # either full path or basename.
    )

  if (renaming_strategy != "free_for_all" && n_file_names_conflicts > 10) {
    cli::cli_bullets("You can use {.code warn_conflicts = 'exact'} to see only exact references of {.val {old}}.")
  }


  if (n_file_names_conflicts == 0 || renaming_strategy == "free_for_all") {
    rename_file_action(new, old, strategy = renaming_strategy, action, verbose)
    # Can't remember why I put this here?
    # Seems to query all-non existent files, only if renaming?
    # check_referenced_files(path = ".", quiet = !verbose)
    if (interactive() && action != "test") {
      cli::cli_inform(c(
        i = "Call {.run reuseme::check_referenced_files()} to see if there are dead links in dir."
      ))
    }
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
rename_file_action <- function(new, old, strategy, action, verbose) {
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
    if (strategy == "free_for_all") {
      cli::cli_inform(c("x" = "Renamed file to {.file {new}} by force. Be careful."))
    } else {
      cli::cli_inform(c("v" = "Renamed file to {.file {new}} without issue."))
    }
  } else if (action == "test") {
    # Not renaming, but going through the same path as I would have
    cli::cli_inform(c(
      "Testing mode, did not rename file, but would have!",
      "strategy: {.val {strategy}}"
    ))
  }
}

compute_conflicts_regex <- function(file, renaming_strategy) {
  if (renaming_strategy == "free_for_all") {
    return("impossible to match nowhere")
  }

  if (renaming_strategy == "file_names") {
    return(file)
  }

  file_name_base <- basename_remove_ext(file)

  object_snake_from_file_kebab <- stringr::str_replace_all(file_name_base, "-", "_")

  if (renaming_strategy == "object_names") {
    # dat/file-name.csv|file_name
    # dat/file-name.csv|file_name|file-name

    regex_file_name <- paste(file, object_snake_from_file_kebab, sep = "|")
    return(regex_file_name)
  }

  if (renaming_strategy == "base_names") { # most strict and picky
    # file-name|file_name
    regex_file_name <- paste(file_name_base, object_snake_from_file_kebab, sep = "|")
    return(regex_file_name)
  }
  # other cases?
  # if objects are called
  # regex_file_name <- paste0(path_file_name, "[^-]?")
  cli::cli_abort(
    c(
      "Not implemented a return value for {.val {renaming_strategy}}",
      "Make sure tests are added."
    ),
    .internal = TRUE
  )
}

scope_rename <- function(old, new, warn_conflicts = "default") {
  res <- dplyr::case_when(
    # respect preferences before checking other conditions
    warn_conflicts == "none" ~ "free_for_all",
    warn_conflicts == "exact" ~ "file_names", # be careful with this
    warn_conflicts == "all" ~ "object_names",
    is_moving(old, new) ~ "file_names",
    is_image(old) ~ "file_names",
    is_generic_file_name(old) ~ "file_names",
    is_adding_a_suffix(old, new) ~ "file_names", # FIXME is it correct? like moving data to data-raw
    is_short_file_name(old, 5L) ~ "file_names",
    is_generic_file_name(old) ~ "file_names",
    # other option ~ "base_names", # would check for base names, but only file, instead of object.
    warn_conflicts == "default" ~ "object_names",
    .default = "base_names" # will probably change this as this gives too many matches...
  )

  # if exception, will check up

  if (res == "base_names") {
    # see if exceptions are needed.
    # don't check for regexp if the original file name has less than min_n_char
  }

  res
}

## helpers for computing scope of renaming ---
is_moving <- function(old, new) {
  fs::path_file(old) == fs::path_file(new)
}

is_short_file_name <- function(file, nchars) {
  nchar(basename_remove_ext(file)) <= nchars
}

# verifies short and contains certain keywords
is_generic_file_name <- function(file) {
  generic_words <- c("change", "temp", "dat", "data")
  regexp <- paste0(generic_words, collapse = "|")
  file_name <- basename_remove_ext(file)
  stringr::str_starts(file_name, regexp) &
    is_short_file_name(file_name, nchars = 9)
}

is_image <- function(file) {
  file_ext <- tolower(fs::path_ext(file))
  file_ext %in% c("png", "jpg", "jpeg", "pdf", "svg")
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
  if (max(nchar(matches)) >= max(4, shortest_char)) {
    TRUE
  } else {
    FALSE
  }
}

## Prevent renaming if something is going on -----


check_proper_renaming_condition <- function(old, new, overwrite, call = rlang::caller_env()) {
  if (fs::is_dir(old) || fs::is_dir(new)) {
    cli::cli_abort("Can't rename directories with this function See {.fn fs::dir_copy} and {.fn fs::dir_delete}.", call = call)
  }
  if (!fs::is_file(old)) {
    cli::cli_abort("Can't rename {.file {old}} as it does not exist. Supply {.arg old} an existing file.", call = call)
  }
  if (fs::path_ext(old) != fs::path_ext(new)) {
    cli::cli_abort(c(
      "!" = "{.arg new} and {.arg old} must have the same file extension, not\\
          {.val {fs::path_ext(old)}} and {.val {fs::path_ext(new)}}."
    ), call = call)
  }

  if (fs::file_exists(new) && !overwrite) {
    # FIXME maybe not fail while testing
    hint_acceptable_renaming(old, new, overwrite)
    cli::cli_abort(c(
      "Can't rename file to {.val {new}}",
      "!" = "{.arg new} already exists {.path {new}}.",
      "i" = "Use {.code overwrite = TRUE} to override."
    ), call = call)
  }
  invisible()
}

hint_acceptable_renaming <- function(old, new, overwrite) {
  # for now: only acceptable renaming is moving
  if (is_moving(old, new)) {
    info <- fs::file_info(c(old, new))
    if (anyNA(info$change_time)) {
      cli::cli_inform("One of the file doesn't exist. hint_acceptable_renaming() expects 2 existing files.", .internal = FALSE)
    }
    # TODO Check that old
    if (info$change_time[1] > info$change_time[2]) {
      cli::cli_inform(c(
        "!" = "{.val {new}} was modified later than {.val {old}}",
        x = "It is generally not a good idea to overwrite a file that doesn't have the latest modification"
      ))
      return(FALSE)
    }
    # r-lib/cli#683
    old_chr <- as.character(old)
    new_chr <- as.character(new)


    cli::cli_inform(c(
      "!" = "{.val {old}} was modified later than {.val {new}}",
      "It may be a good idea to view the diff it it makes sense.",
      i = "Use {.run diffviewer::visual_diff('{old_chr}',  '{new_chr}')}."
    ))
    return(TRUE)
  } else {
    FALSE
  }
}

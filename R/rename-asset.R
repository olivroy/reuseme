#' Rename an output or a data file
#'
#' @description
#' This function can improve your workflow.
#' It is inspired by [usethis::rename_files()], but its scope
#' is more oriented towards analysis script.
#'
#'
# Still a bit buggy.. Will have to look more closely eventually.
rename_asset <- function(old_name, new_name, force = FALSE, action = TRUE) {
  fs::path_real(old_name) # Will fail immediately if it doesn't exist.
  if (fs::is_dir(old_name)) {
    cli::cli_abort("Can't rename directories with this function See {.fn fs::dir_copy} and {.fn fs::dir_delete}.")
  }
  is_git <- !isFALSE(tryCatch(rprojroot::find_root_file(criterion = rprojroot::criteria$is_vcs_root), error = function(e) FALSE))
  if (is_git) {
    cli::cli_warn("It is better to use this function in a version-controlled repository.")
  }
  # looking for the object name as well if changing from a file name to another
  path_file_name <- fs::path_ext_remove(old_name)
  file_name_base <- fs::path_file(path_file_name)
  new_name_base <- fs::path_file(fs::path_ext_remove(new_name))

  cnd_check_for_object_names <- file_name_base != new_name_base & !file_name_base %in% (c("index", "temp"))

  if (cnd_check_for_object_names) {
    object_snake_from_file_kebab <- stringr::str_replace_all(file_name_base, "-", "_")
    regex_file_name <- paste0(c(object_snake_from_file_kebab, old_name), collapse = "|")
  } else {
    regex_file_name <- paste0(path_file_name, "[^-]")
  }

  related_files <- fs::dir_ls(regexp = paste0(regex_file_name, "\\."), recurse = TRUE)
  related_files <- setdiff(related_files, old_name)
  if (length(related_files) > 0) {
    cli::cli_warn(c(
      "Other files have a similar pattern",
      "See {.file {related_files}}",
      "No support yet for that"
    ))
  }
  if (!force) {
    extra_msg_if_file_conflict <- c(
      "did not rename files!",
      i = "Make sure you change the file path to",
      new_name,
      "in these locations (`new_name` copied to clipboard) or see Find in Files Replace All if confident.",
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
    if (tools::file_ext(old_name) == "R" && action) {
      # usethis::rename_files(old_name, new_name)
      cli::cli_inform("See if need for tests change snapshots")
    }
    if (tools::file_ext(new_name) %in% c("png")) {
      cli::cli_inform(
        c(
          "Use in markdown/quarto docs (source mode) with",
          '![]({new_name}){{fig-alt="" width="70%"}}'
        )
      )
    }

    if (!action) {
      cli::cli_inform("Testing mode, did not rename file")
      return(invisible(new_name))
    }

    fs::file_move(old_name, new_name)

    if (force) {
      cli::cli_alert_success("Renamed file to {.file {new_name}} without issue.")
    } else {
      cli::cli_alert_danger("Renamed file to {.file {new_name}} by force. Be careful.")
    }

    check_files_exist_in_dir(path = ".", quiet = !verbose)
    return(invisible(new_name))
  }

  # readr::write_lines(new_name, file = readr::clipboard())
  if (.Platform$OS.type == "windows") {
    write.table(new_name, file = "clipboard", eol = "", row.names = FALSE, col.names = FALSE)
  } else {
    clipr::write_clip(new_name)
  }
  new_name
}

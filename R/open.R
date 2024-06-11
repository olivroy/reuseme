#' Open a Document in RStudio
#'
#' Wrapper around [rstudioapi::documentOpen()], but with `fs paths`, for consistency.
#' If the file could not be opened, a clickable hyperlink is displayed.
#'
#' * `active_rs_doc()` is a wrapper around [rstudioapi::documentPath()] that handles
#'   unsaved files gracefully
#' @inheritParams rstudioapi::documentOpen
#' @param move_cursor Boolean; move the cursor to the requested location after
#'   opening the document?
#' @return Invisibly returns the document id
#' @export
#' @examples
#' if (FALSE) {
#'   # open the fictious file.R at line 5
#'   open_rs_doc("file.R", line = 5)
#' }
#'
open_rs_doc <- function(path, line = -1L, col = -1L, move_cursor = TRUE) {
  path <- fs::path_real(path)
  check_number_whole(line)
  check_number_whole(col)

  if (col != -1L && line == -1L) {
    cli::cli_abort("Internal error, you can't specify col only.")
  }

  doc_id <- rstudioapi::documentOpen(path = path, line = line, col = col, moveCursor = move_cursor)
  if (is.null(doc_id)) {
    # FIXME why is this code like this?
    file_pos_string <- path
    if (line != -1L) pos_string <- paste0(pos_string, ":", line)
    if (col != -1L) pos_string <- paste0(pos_string, ":", col)
    cli::cli_bullets()
  }
  invisible(doc_id)
}

active_rs_proj <- function() {
  NULL
}

#' @name open_rs_doc
#' @export
active_rs_doc <- function() {
  if (!interactive() && !is_rstudio()) {
    return("Non-existing doc")
  }
  if (!is_rstudio()) {
    cli::cli_abort("Not in RStudio.")
  }
  unsaved_doc <- tryCatch(rstudioapi::documentPath(), error = function(e) TRUE)
  if (isTRUE(unsaved_doc)) {
    return(NULL)
  }
  path <- tryCatch(rstudioapi::documentPath(), error = function(e) {
    cli::cli_abort("Either RStudio is not available or you are trying to map an unsaved file")
  })
  path <- fs::path_expand_r(path)
  # likely not hapenning on RStudio >= 2023.06.2
}

#' Copy the active document to the same location
#'
#' The goal is to provide things that RStudio or usethis doesn't provide natively.
#'
#' For example, `active_rs_doc_rename()` will not happen, because it is already easy
#' to do so via the RStudio IDE.
#'
#' @param new The new file name, that will be copied in the same
#'   directory as the [active document][active_rs_doc()]
#' @param old The old name, defaults to the active document.
#' @inheritParams rlang::args_dots_empty
#' @returns The new file name
#' @family document manipulation helpers
#' @seealso [rename_files2()]
#' @export
active_rs_doc_copy <- function(new = NULL, ..., old = NULL) {
  rlang::check_dots_empty()
  old <- old %||% active_rs_doc()

  if (is.null(old)) {
    cli::cli_abort("Unsaved document, focus on the saved doc you want to save.")
  }

  if (!fs::path_ext(old) %in% c("md", "R", "qmd", "Rmd")) {
    cli::cli_abort("Only R and md docs for now")
  }
  old_path_file <- fs::path_ext_remove(fs::path_file(old))

  if (grepl("r-profile|Rprofile", old)) {
    cli::cli_abort("Attempting to copy Rprofile (focus on the document you want)")
  }
  if (is.null(new)) {
    new_name <- paste0(old_path_file, "-new")
  } else {
    new_name <- sub("\\.R|\\.[Rq]?md$", "", new)
  }
  # Hack to ensure file/file.R will be correctly renamed.
  new_path <- sub(paste0(old_path_file, "\\."), paste0(new_name, "."), old)

  copied <- file.copy(old, new_path, overwrite = FALSE)
  if (copied) {
    cli::cli_inform(c(
      v = "Copied {.file {old}}",
      i = "Edit {.file {new_path}}"
    ))
  } else {
    cli::cli_abort(c(
      "Did not overwrite the file {.file {new_path}}.",
      i = "Set {.arg new} explicitly or use {.fn fs::file_copy}."
    ))
  }
  invisible(new_path)
}

#' Delete the active RStudio document safely
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Gathers informative summary about the document you are about to delete.
#'
#'
#' Will delete more easily if file name starts with `temp-`, if file is untracked and recent.
#' @return Called for side-effects. The document content invisibly if deleting and reason.
#' @export
#' @family document manipulation helpers
#' @examplesIf FALSE
#' active_rs_doc_delete()
active_rs_doc_delete <- function() {
  if (!rlang::is_interactive() || !is_rstudio()) {
    cli::cli_abort(c("Can't delete files in non-interactive sessions."))
  }
  doc <- active_rs_doc()
  reasons_deleting <- NULL
  reasons_not_deleting <- NULL
  will_delete <- NULL
  if (is.null(doc)) {
    cli::cli_abort(c("Can't delete an unsaved file.", i = "Save the file first."))
  }

  elems <- normalize_proj_and_path(doc)

  if (fs::is_dir(elems$full_path)) {
    cli::cli_abort("Must be a file", .internal = TRUE)
  }
  if (interactive() && is_rstudio()) {
    rstudioapi::documentSave()
  }
  cli::cli_inform(c(
    "i" = "Checking if active file can be deleted safely."
  ))
  if (!is.na(elems$project)) {
    is_git <- is_git(elems$project)
    if (!is_git) {
      cli::cli_abort("Can't delete a file in non-git directory.")
    }
  } else {
    is_git <- FALSE
  }

  if (is_git) {
    rlang::check_installed("gert")
    stat_files <- gert::git_stat_files(elems$rel_path, repo = elems$project)
    is_untracked <- is.na(stat_files$modified)
  } else {
    stat_files <- data.frame(modified = NA)
    is_untracked <- NA
  }

  if (!is.na(stat_files$modified)) {
    print(stat_files)
    file_status <- gert::git_status(pathspec = elems$rel_path, repo = elems$project)
    if (nrow(file_status) > 0) {
      print(file_status)

      if (all(file_status$status == "conflicted")) {
        will_delete <- append(will_delete, TRUE)
        reasons_deleting <- c(reasons_deleting, "the file is a renamed git conflict")
      } else {
        will_delete <- append(will_delete, FALSE)
        reasons_not_deleting <- c(reasons_not_deleting, "the file is tracked with git")
      }
    }
    file_info <- fs::file_info(elems$rel_path)
  } else {
    if (is_git) {
      will_delete <- append(will_delete, TRUE)
      reasons_deleting <- c(reasons_deleting, "file is untracked")
    } else {
      # ?
    }
    file_status <- NULL
    outline <- withCallingHandlers(
      file_outline(path = elems$full_path),
      error = function(e) {
        cli::cli_warn("File outline failed somehow. Please report.", parent = e)
        NA
      }
    )
    if (!is.null(outline) && !identical(outline, NA)) {
      will_delete <- append(will_delete, FALSE)
      reasons_not_deleting <- c(
        reasons_not_deleting, "couldn't explore the outline, worth taking a look."
      )
      outline <- NULL
    } else if (!is.null(outline) && nrow(outline) > 0) {
      print(utils::head(outline))
      will_delete <- append(will_delete, FALSE) # perhaps worth taking a look
      reasons_not_deleting <- c(reasons_not_deleting, "it has contents")
    } else {
      reasons_deleting <- append(reasons_deleting, "empty outline")
    }
    if (!is.na(elems$project)) {
      file_info <- fs::file_info(elems$rel_path)
    } else {
      file_info <- fs::file_info(elems$full_path)
    }
  }

  parent_dir <- fs::path_file(fs::path_dir(elems$full_path))

  if (grepl("^temp", fs::path_file(elems$rel_path)) ||
      (!parent_dir %in% c("tests", "testthat") && grepl("^test-", fs::path_file(elems$rel_path)))) {
    reasons_deleting <- c(reasons_deleting, "it has the temp- prefix.")
    will_delete <- append(will_delete, TRUE)
  }
  if (parent_dir %in% c("Downloads", "Desktop")) {
    # Consider that files in the Downloads or Desktop are temp files.
    will_delete <- append(will_delete, TRUE)
    reasons_deleting <- c(reasons_deleting, "in the ~/Downloads or ~/Desktop folder.")
  }

  if (isTRUE(is_untracked)) {
    # file created in the last hour
    creation_recent <-
      difftime(Sys.time(), file_info$birth_time, units = "hours") < 1

    if (creation_recent) {
      reasons_deleting <- c(reasons_deleting, "very recent")

      will_delete <- append(will_delete, TRUE)
    } else {
      reasons_not_deleting <- c(reasons_not_deleting, "older untracked file, better to look at outline to see if not important.")
      will_delete <- append(will_delete, FALSE)
    }
  }

  # TODO structure and summarise information.
  file_info <- dplyr::select(file_info, path, size, dplyr::ends_with("time"))
  file_info <- dplyr::select(file_info, !dplyr::where(\(x) all(is.na(x))))
  file_info <- dplyr::select(file_info, !dplyr::any_of(rm_duplicate_columns(file_info)))
  if (!is.null(file_info$size) && all(file_info$size == 0)) {
    will_delete <- append(will_delete, c(TRUE, TRUE))
    reasons_deleting <- c(reasons_deleting, "file is empty")
    file_info$size <- NULL
  }
  pillar::glimpse(file_info)

  # defaults to FALSE if equality :)
  # print(table(will_delete))
  will_delete_decision <- as.logical(names(which.max(table(will_delete))))
  # only true or false acceptable!
  check_bool(will_delete_decision)
  if (isTRUE(will_delete_decision)) {
    cli::cli_inform(c(
      "v" = "Deleted the active document {.val {elems$rel_path}} because {reasons_deleting}.",
      # FIXME (upstream) the color div doesn't go all the way r-lib/cli#694
      "i" = paste(cli::col_grey("The deleted file"), "{.path {elems$full_path}}", cli::col_grey("contents are returned invisibly in case you need them."))
    ))
    contents <- readLines(elems$full_path, encoding = "UTF-8")
    fs::file_delete(elems$full_path)
    return(invisible(contents))
  }

  cli::cli_abort(c(
    "Can't delete the active document {.path {elems$rel_path}}, because {reasons_not_deleting}.",
    "It outweighs the reasons for deleting: {reasons_deleting}."
  ))
}

active_rs_doc_sitrep <- function() {
  # The goal is to prepare it for action.
  # print file outline
  # git status (untracked, modified, staged, etc.)
  # git history.
  # git compare with previous state
  # mod time
  # git mod time
  # print ssh short commit id..
  list(
    staged = NA,
    # etc.
  )
}

active_rs_doc_undo_local_changes <- function() {
  # When active_rs_doc_delete is mature, create this one!
}


is.POSIXct <- function(x) inherits(x, "POSIXct")
rm_duplicate_columns <- function(x) {
  x_date <- dplyr::select(
    x,
    dplyr::where(is.POSIXct)
  )

  if (ncol(x_date) <= 1) {
    return(integer(0))
  }

  which <- integer(0)
  for (i in 2:ncol(x_date)) {
    # numeric rounds to seconds
    diff <- difftime(
      x_date[[1]],
      x_date[[i]],
      units = "secs"
    )
    diff <- abs(diff)
    # consider the same if time within 1 second
    if (all(diff < 1)) {
      which <- c(which, i)
    }
  }
  if (length(which) > 0) {
    names(x_date)[which]
  } else {
    integer(0)
  }
}

path_metadata <- function() {
  characteristics <- NULL
}

normalize_proj_and_path <- function(path, call = caller_env()) {
  full_path <- fs::path_real(fs::path_expand_r(path))
  if (!fs::is_file(full_path)) {
    cli::cli_abort("{.path {path}} does not exist.", call = call)
  }
  project <-
    tryCatch(
      rprojroot::find_root_file(criterion = rprojroot::is_rstudio_project, path = full_path),
      warning = function(e) NULL,
      error = function(e) NULL,
      message = function(e) NULL
    )
  if (is.null(project)) {
    return(
      list(
        project = NA,
        rel_path = fs::path_file(full_path),
        full_path = full_path
      )
    )
  }

  rel_path <- fs::path_rel(full_path, start = project)

  if (grepl("../", rel_path, fixed = TRUE)) {
    cli::cli_abort(c(
      "Something went wrong in path normalization.",
      "With path = {path}, detected project = {project}, derived full_path = {full_path} and rel_path = {rel_path}"
    ), call = call)
  }
  list(
    project = project,
    rel_path = rel_path,
    full_path = full_path
  )
}

#' Open Files Pane at current document location
#'
#' Easily navigate to active file document.
#'
#' Wrapper around [executeCommand("activateFiles")][rstudioapi::executeCommand()] +
#' [rstudioapi::filesPaneNavigate()] + [rstudioapi::getActiveDocumentContext()]
#'
#' @param path A path to file to navigate to (default active document).
#'
#' @returns NULL, called for its side effects.
#' @export
active_rs_doc_nav <- function(path = active_rs_doc()) {
  if (!is_rstudio() || !interactive()) {
    cli::cli_abort("Must use in RStudio interactive sessions.")
  }
  if (is.null(path)) {
    cli::cli_abort("Can't navigate to an unsaved file!")
  }
  if (fs::is_file(path)) {
    dir <- fs::path_dir(path)
  } else if (fs::is_dir(path)) {
    dir <- path
  } else {
    cli::cli_abort("{.arg path} must be an existing file or directory.")
  }
  rstudioapi::executeCommand("activateFiles")
  rstudioapi::filesPaneNavigate(dir)
  cli::cli_inform(c(
    "v" = "Navigated to {.path {dir}} in RStudio Files Pane."
  ))
  invisible()
}

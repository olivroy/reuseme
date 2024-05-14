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

#' @name open_rs_doc
#' @export
active_rs_doc <- function() {
  if (!interactive() && !rstudioapi::isAvailable()) {
    return("Non-existing doc")
  }
  if (!rstudioapi::isAvailable()) {
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

  if (!fs::path_ext(old) %in% c("R", "qmd", "Rmd")) {
    cli::cli_abort("Only R docs for now")
  }
  old_path_file <- fs::path_file(old) |> fs::path_ext_remove()
  if (stringr::str_detect(old, "r-profile|Rprofile")) {
    cli::cli_abort("Attempting to copy Rprofile (focus on the document you want)")
  }
  if (is.null(new)) {
    new_name <- paste0(old_path_file, "-new")
  } else {
    new_name <- stringr::str_remove(new, "\\.R|\\.qmd|\\.Rmd$")
  }
  # Hack to ensure file/file.R will be correctly renamed.
  new_path <- stringr::str_replace(old, paste0(old_path_file, "\\."), paste0(new_name, "."))

  file.copy(old, new_path, overwrite = FALSE)
  cli::cli_inform(c(
    v = "Copied {.file {old}}",
    i = "Edit {.file {new_path}}"
  ))
}

#' Delete the active RStudio document safely
#'
#' `r lifecycle::badge('experimental')`
#'
#' Gathers informative summary about the document you are about to delete.
#'
#' @return Called for side-effects. The document content invisibly if deleting and reason.
#' @export
#' @family document manipulation helpers
#' @examplesIf FALSE
#' active_rs_doc_delete()
active_rs_doc_delete <- function() {
  if (!rlang::is_interactive() || !rstudioapi::isAvailable()) {
    cli::cli_abort(c("Can't delete files in non-interactive sessions."))
  }
  doc <- active_rs_doc()
  if (is.null(doc)) {
    cli::cli_abort(c("Can't delete an unsaved file.", i = "Save the file first."))
  }

  elems <- normalize_proj_and_path(doc)

  rstudioapi::documentSave()
  if (!is_git(elems$project)) {
    cli::cli_abort("Can't delete a file in non-git directory.")
  }

  rlang::check_installed("gert")
  stat_files <- gert::git_stat_files(elems$rel_path, repo = elems$project)
  if (!is.na(stat_files$modified)) {
    will_delete <- FALSE
    print(stat_files)
    file_status <- gert::git_status(pathspec = elems$rel_path, repo = elems$project) |> print()
    file_info <- fs::file_info(elems$rel_path)
  } else {
    will_delete <- TRUE
    file_status <- NULL
    outline <- file_outline(path = elems$full_path)
    if (!is.null(outline) && nrow(outline) > 0) {
      print(outline)
      will_delete <- NA # perhaps worth taking a look
    }
    file_info <- fs::file_info(elems$rel_path)
  }

  # TODO structure and summarise information.
  file_info <- dplyr::select(file_info, path, size, dplyr::ends_with("time"))
  file_info <- dplyr::select(file_info, !dplyr::where(\(x) all(is.na(x))))
  file_info <- dplyr::select(file_info, !any_of(rm_duplicate_columns(file_info)))
  if (all(file_info$size == 0)) {
    file_info$size <- NULL
  }
  pillar::glimpse(file_info) |> print()

  if (isTRUE(will_delete)) {
    cli::cli_inform(c(
      "v" = "Deleted the active document {.val {elems$rel_path}} because xyz",
      "i" = cli::col_grey("The deleted file contents are returned invisibly in case you need them.")
    ))
    contents <- readLines(elems$full_path, encoding = "UTF-8")
    fs::file_delete(elems$full_path)
    return(invisible(contents))
  }

  cli::cli_abort(c(
    "Can't delete the active document {.path {elems$rel_path}}.",
    "It seems important"
  ))
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
  full_path <- fs::path_expand_r(path) |> fs::path_real()
  if (!fs::is_file(full_path)) {
    cli::cli_abort("{.path {path}} does not exist.", call = call)
  }
  project <- rprojroot::find_root_file(criterion = rprojroot::is_rstudio_project, path = full_path)

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

#' Add a TODO list by project to a TODO.R file in the base directory
#'
#' Creates or edits a `TODO.R` file to store your TODOs.
#' By default it will write in the current RStudio project.
#'
#' If you use `use_todo()` with a version-control repository, you may want to
#' use `usethis::use_git_ignore("TODO.R")` if you don't want your `TODO.R` file
#'
#' to be included in git. If using in a package directory, use
#' `usethis::use_build_ignore("TODO.R")` to prevent a note in `R CMD CHECK`
#'
#' @param todo A character vector of lines to add to the TODO file
#' @param proj By default, the active project, an arbitrary directory, or a
#'   RStudio project name in the following directories
#'   `options(reuseme.destdir)`, uses [proj_list()] in this case.
#' @param code If `TRUE`, will render code output (default is text).
#' @seealso [usethis::write_union()]
#'
#' @return A `TODO.R` file appended with the `todo` string.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   use_todo("I need to do that")
#'   use_todo(c("I need to do that again", "youppi"))
#'   use_todo("c(x, y)", code = TRUE)
#'   use_todo("Here", proj = "my-analysis")
#'   use_todo(c("my-analysis::Here", "I am"))
#'   # add to a global todo.
#'   use_todo(c("all::Here", "I am"))
#' }
use_todo <- function(todo, proj = proj_get(), code = FALSE) {
  check_character(todo)
  # TODO think about maybe using todo = clipr::read_clip()
  # Check how reprex do it.

  # if clipr::read_clip(), should put code = TRUE.
  proj_name_in_todo <- stringr::str_extract(todo[1], "^([^\\s\\:]{2,20})\\:{2}", group = 1)

  if (!is.na(proj_name_in_todo)) {
    proj <- proj_name_in_todo
    regex_proj_in_todo <- paste0(proj_name_in_todo, "\\:\\:", "\\s?")
    todo[1] <- stringr::str_remove(todo[1], regex_proj_in_todo)
  }

  # Handle special global and all syntax for todo items.
  if (proj %in% c("global", "all")) {
    proj <- Sys.getenv("R_USER") # ?base::path.expand
  }

  is_active_proj <- identical(proj, proj_get2())

  if (!code) {
    todo_lines <- paste("# TODO", todo)
  } else {
    todo_lines <- todo
  }

  path_todo <- "TODO.R"

  if (!fs::dir_exists(proj)) { # when referring to a project by name.
    all_projects <- proj_list()
    rlang::arg_match0(proj, values = names(all_projects))
    proj_path <- unname(all_projects[proj])
  } else {
    proj_path <- proj
  }

  full_path_todo <- if (is_active_proj) {
    path_todo
  } else {
    fs::path(proj_path, path_todo)
  }
  # TODO nice to have, but would need to extract duplicates
  # (ideally changes in usethis)
  # Change the default write_union message.
  write_union2(full_path_todo, lines = todo_lines, quiet = FALSE)
}

#' Remove a TODO/WORK/FIXME item from a file
#'
#' Function meant to be wrapped as `{.run}` hyperlinks with `file_outline()`.
#' It basically removes a line from a file.
#'
#' @param line_id The line number (a single integer)
#' @param file Path to a file
#' @param regexp A regexp to assess that the file content has not changed.
#' @param rm_line A logical If `NULL` will remove the full line in the file
#'   (for TODO, or FIXME items), else for WORK, will only remove the WORK tag
#'   will remove only the tag (i.e. TODO, WORK, FIXME)
#'
#' @return Writes a file with corrections, and returns the new line
#'   content invisibly.
#' @export
#' @keywords internal
mark_todo_as_complete <- function(line_id, file, regexp, rm_line = NULL) {
  check_string(regexp)
  check_number_whole(line_id)

  # to defer warning.
  warn_change_of_line <- FALSE

  file_content <- readLines(file, encoding = "UTF-8")
  line_content <- file_content[line_id]
  detect_regexp_in_line <- grepl(pattern = regexp, x = line_content)

  if (!detect_regexp_in_line) {
    regexp_detection <- grep(pattern = regexp, x = file_content)
    warn_change_of_line <- TRUE

    if (length(regexp_detection) == 1) {
      line_id <- regexp_detection
      line_content <- line_content <- file_content[line_id]
      detect_regexp_in_line <- grepl(pattern = regexp, x = line_content)
    } else {
      cli::cli_abort(c(
        "{.arg rexpexp} was detected in more than 1 line.",
        "Not marking TODO as complete."
      ))
    }
  }

  tag_type <- extract_tag_in_text(text = line_content)

  if (warn_change_of_line) {
    cli::cli_warn(c(
      x = "Could not detect {.arg regexp} as expected",
      "Could not find {.val {regexp}} at line {line_id}.",
      i = "Has the file content changed since you ran this code?",
      "`regexp` was detected in lines {regexp_detection}."
    ))
  }

  if (is.null(rm_line)) {
    rm_line <- tag_type %in% c("TODO", "FIXME")
  }

  if (rm_line) {
    cli::cli_alert_success("Marking `{line_content}` as done! ")
    file_content_new <- file_content[-line_id]
    line_content_new <- ""
  } else {
    cli::cli_alert_success(
      "Marking `{line_content}` as done! (Removing the {tag_type})"
    )
    line_content_new <- sub(
      pattern = paste0(tag_type, "\\s+"),
      replacement = "",
      line_content
    )

    file_content[line_id] <- line_content_new
    file_content_new <- file_content
  }

  usethis::write_over(path = file, lines = file_content_new, overwrite = TRUE)
  invisible(line_content_new)
}

extract_tag_in_text <- function(text, error_call = caller_env()) {
  check_string(text, call = call, arg = "line_content")
  match_tag <- regexpr(pattern = "WORK|FIXME|TODO", text = text)
  tag_type <- regmatches(x = text, m = match_tag)
  if (length(tag_type) == 0) {
    choices <- c("WORK", "FIXME", "TODO")
    cli::cli_abort(c(
      x = "Cannot mark a TODO item as complete if it doesn't contain the tags.",
      i = "Did not detect any {.val {choices}} tags in the specified line."
    ), call = error_call)
  }
  # Fails if no WORK, FIXME or TODO tags are found.
  arg_match0(tag_type, c("WORK", "FIXME", "TODO"), error_call = error_call, arg_nm = "line_content")
}

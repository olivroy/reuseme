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
#' @param todo A character vector of lines to add to the TODO file. See details
#'   for special handling.
#' @param proj By default, the active project, an arbitrary directory, or a
#'   RStudio project name in the following directories
#'   `options(reuseme.destdir)`, uses [proj_list()] in this case.
#'   If a file, will write there.
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
use_todo <- function(todo, proj = proj_get2(), code = FALSE) {
  check_character(todo)
  # TODO think about maybe using todo = clipr::read_clip()
  # Check how reprex do it.
  proj_is_a_file <- fs::is_file(proj)

  if (proj_is_a_file) {
    path_todo <- proj
  } else {
    # compute full path of todo
    todo_pkg <- compute_path_todo(todo, proj)
    path_todo <- todo_pkg$path_todo
    todo <- todo_pkg$todo
  }

  if (code) {
    todo_lines <- todo
  } else {
    todo_lines <- paste("# TODO", todo)
  }



  # TODO nice to have, but would need to extract duplicates
  # (ideally changes in usethis)
  # Change the default write_union message.
  write_union2(path_todo, lines = todo_lines, quiet = FALSE)
  invisible(path_todo)
}

#' Remove a TODO/WORK/FIXME item from a file
#'
#' Function meant to be wrapped as `{.run }` hyperlinks with [file_outline()].
#' It basically removes a line from a file.
#'
#' @param line The line number (a single integer)
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
complete_todo <- function(line, file, regexp, rm_line = NULL) {
  check_string(regexp)
  check_number_whole(line)
  line_original <- line
  # to defer warning.
  if (interactive() && is_rstudio()) {
    rstudioapi::documentSaveAll()
  }
  warn_change_of_line <- FALSE

  file_content <- readLines(file, encoding = "UTF-8", warn = FALSE)
  line_content <- file_content[line]

  # Special case for issues (probably need to opt out at some point)
  # patch that will likely not work for many cases.
  if (grepl(pattern = "issues", x = regexp, fixed = TRUE)) {
    regexp <- sub("issues", "#", regexp, fixed = TRUE)
    # regexp <- stringr::str_replace(regexp, "([^\\d]+)(\\d+)", "\\1#\\2")
  }


  detect_regexp_in_line <- grepl(pattern = regexp, x = line_content)

  if (!detect_regexp_in_line) {
    regexp_detection <- grep(pattern = regexp, x = file_content)
    warn_change_of_line <- TRUE

    if (length(regexp_detection) == 1) {
      line <- regexp_detection
      line_content <- line_content <- file_content[line]
      detect_regexp_in_line <- grepl(pattern = regexp, x = line_content)
    } else if (length(regexp_detection) > 1) {
      cli::cli_abort(c(
        "{.arg rexpexp} was detected in more than 1 line.",
        "Not marking TODO as complete."
      ))
    } else {
      cli::cli_abort(c(
        "{.arg rexpexp} was not detected.",
        i = "Did you delete the line already?"
      ))
    }
  }

  tag_type <- extract_tag_in_text(text = line_content, line)

  if (warn_change_of_line) {
    cli::cli_warn(c(
      x = "Could not find {.arg regexp} as expected",
      "Could not find {.val {regexp}} at line {line_original}.",
      i = "Has the file content changed since you ran this code?",
      # needs qty for cli pluralization, but no printing
      "`regexp` was detected in {cli::qty(length(regexp_detection))} line{?s} {regexp_detection}."
    ))
  }

  line_content_before_comment <- stringr::str_extract(line_content, "([^#]+)#", group = 1)
  if (is.null(rm_line)) {
    rm_line <- tag_type %in% c("TODO", "FIXME") # && is.na(line_content_before_comment)
  }
  line_content_todo <- stringr::str_extract(line_content, "#[^#]+")
  line_content_show <- stringr::str_squish(line_content)

  if (rm_line) {
    if (is.na(line_content_before_comment)) {
      line_content_show <- cli::style_strikethrough(line_content_show)
    } else {
      line_content_show <- paste(line_content_before_comment, cli::style_strikethrough(line_content_todo))
    }
  } else {
    # Only strikethrough the tag
    regex <- paste0(" ", tag_type)
    regex_new <- cli::style_strikethrough(regex)
    line_content_show <- stringr::str_replace(line_content_show, regex, regex_new)
  }
  file_line <- paste0(file, ":", line)
  cli::cli_alert_success(
    "Removed {.code {line_content_show}} from {.file {file_line}}!"
  )
  # Rem
  line_content_new <- strip_todo_line(line_content, only_rm_tag = !rm_line)

  if (nzchar(line_content_new)) {
    file_content[line] <- line_content_new
    file_content_new <- file_content
  } else {
    file_content_new <- file_content[-line]
    if (!rm_line) {
      # WIll remove this line eventually
      # remove line if it ends up empty. not supposed to happen
      cli::cli_abort("This should not happen. We were supposed not to remove lines", .internal = TRUE)
    }
  }

  if (length(file_content_new) > 0) {
    usethis::write_over(path = file, lines = file_content_new, overwrite = TRUE, quiet = TRUE)
  } else {
    cli::cli_inform(c(
      "!" = "No more TODOs.",
      "v" = "Deleting {.file {file}}."
    ))
    fs::file_delete(file)
  }
  invisible(line_content_new)
}

extract_tag_in_text <- function(text, line, error_call = caller_env()) {
  check_string(text, call = error_call, arg = "line_content")
  match_tag <- regexpr(pattern = "WORK|FIXME|TODO", text = text)
  tag_type <- regmatches(x = text, m = match_tag)
  if (length(tag_type) == 0) {
    choices <- c("WORK", "FIXME", "TODO")
    cli::cli_abort(c(
      x = "Could not delete the TODO item.",
      i = "Line {line} does not contain any {.or {.val {choices}}} tags."
    ), call = error_call)
  }
  arg_match0(tag_type, c("WORK", "FIXME", "TODO"), error_call = error_call, arg_nm = "line_content")
}


# Helpers --------

#' @returns A list of path_todo and transformed todo
#' @noRd
compute_path_todo <- function(todo, proj) {
  # if clipr::read_clip(), should put code = TRUE.
  proj_name_in_todo <- stringr::str_extract(todo[1], "^([^\\s\\:]{2,20})\\:{2}", group = 1)

  if (!is.na(proj_name_in_todo)) {
    proj <- proj_name_in_todo
    regex_proj_in_todo <- paste0(proj_name_in_todo, "\\:\\:", "\\s?")
    todo[1] <- sub(regex_proj_in_todo, "", todo[1])
  }

  is_active_proj <- tryCatch(
    identical(proj, proj_get2()),
    error = function(e) FALSE
  )

  # Handle special global and all syntax for todo items.
  if (proj %in% c("global", "all")) {
    proj_path <- Sys.getenv("R_USER", Sys.getenv("HOME")) # ?base::path.expand
  } else if (!is_active_proj) {
    # in interactive session with options set
    proj_path <- proj_list(proj)
  }

  if (!is_active_proj && !fs::dir_exists(proj_path)) {
    cli::cli_abort(
      c(
        "proj should be a valid path now.",
        i = "Transformed proj to {.path {proj_path}}"
      ),
      .internal = TRUE,
      call = error_call
    )
  }
  if (is_active_proj) {
    path_todo <- "TODO.R"
  } else {
    path_todo <- fs::path(proj_path, "TODO.R")
  }
  list(path_todo = path_todo, todo = todo)
}
# this doesn't work

# accepts a single line
strip_todo_line <- function(x, only_rm_tag = FALSE) {
  check_string(x)
  if (!grepl("TODO|WORK|FIXME", x)) {
    cli::cli_abort("Could not detect a todo tag in x")
  }
  if (only_rm_tag) {
    x_new <- sub("\\s(TODO|WORK|FIXME)", "", x)
  } else {
    x_new <- stringr::str_extract(x, "([^#]+)\\#+", group = 1)
    if (is.na(x_new)) {
      x_new <- ""
      # cli::cli_abort("Could not extract content before tag")
    }
  }
  if (x_new == x) {
    cli::cli_abort("Could not make any change to x = {.val {x}}")
  }
  x_new <- stringr::str_trim(x_new, "right")
  x_new
}

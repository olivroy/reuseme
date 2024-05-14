#' Opens a RStudio project in a new session
#'
#' If not specified, will generate hyperlinks that call [usethis::proj_activate()].
#' `proj_switch()` looks at `options(reuseme.reposdir)`.
#'
#' @param proj the name of a project located in the default locations or `NULL`
#' @param new_session Should open in a new session?
#'
#' @returns Single logical value indicating if current session is modified.
#' @export
#' @seealso [usethis::proj_activate()]
#' @family project management helpers
proj_switch <- function(proj = NULL, new_session = TRUE) {
  # This is my default places (reuseme.reposdir possibly)
  # See fs::path_home
  project <- proj_list(proj)

  if (rlang::has_length(project, 1)) {
    # Doing the switch
    if (new_session) {
      usethis::proj_activate(unname(project))
    } else {
      # remove if https://github.com/r-lib/usethis/pull/1954 is implemented.
      rstudioapi::openProject(path = unname(project), newSession = FALSE)
    }
    return(invisible(project))
  }

  # Displaying possibilities
  # TODO maybe add a max?
  bullets <- paste0("Open {.run [", names(project), "](reuseme::proj_switch('", names(project), "', new_session = ", new_session, "))}")
  cli::cli_bullets(bullets)
}


#' Active a file at location
#'
#' @param file A filename or regexp to a file inside `proj`
#' @param proj a project path or file [proj_list()]
#' @param pattern A regular expression to look for
#' @return The file outline if multiple matches are found
#' @export
#'
#' @examples
#' try(proj_file("A non-existent file"))
#' @family project management helpers
proj_file <- function(file = NULL, proj = NULL, pattern = NULL) {
  rlang::check_required(file)
  # search will only be conducted with pattern
  if (is.null(pattern) && is.null(file)) {
    cli::cli_abort(
      "One of {.arg pattern} or {.arg file} must exist."
    )
  }
  file <- file %||% "A non-existent rubbish file placeholder"
  if (fs::file_exists(file)) {
    file_outline(path = file)
    open_rs_doc(file)
  }
  proj <- proj %||% proj_get2()
  proj_path <- proj_list(proj)

  file_exts <- c("R", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")
  possible_files <- fs::dir_ls(proj_path, regexp = file_exts_regex, recurse = TRUE)
  possible_files <- fs::path_filter(possible_files, regexp = "_snaps", invert = TRUE)
  possible_files <- fs::path_filter(possible_files, regexp = file)

  if (length(possible_files) == 0) {
    if (is.null(pattern)) {
      cli::cli_abort("No match found for {.val {file}} in {.file {proj_path}}")
    } else {
      return(proj_outline(
        pattern = pattern,
        proj = proj
      ))
    }
  }

  if (length(possible_files) == 1) {
    if (is.null(pattern)) {
      open_rs_doc(possible_files)
    } else {
      file_outline(pattern = pattern, path = possible_files)
    }
  }
  cli::cli_inform(c( # TODO improve on this message
    "A couple files found. Access the desired place."
  ))
  file_outline(pattern = pattern, path = possible_files)
}

#' Returns a named project list options
#'
#' It peeks `options(reuseme.reposdir)` to find projects.
#'
#' @param proj A project path or name to match
#' @param dirs The directories in which we want to list projects.
#'
#' @return A named character vector with the project name as name, and path as value.
#'   If `proj` is supplied
#' @export
#' @family project management helpers
proj_list <- function(proj = NULL, dirs = getOption("reuseme.reposdir")) {
  check_string(proj, allow_null = TRUE)

  if (!is.null(proj) && length(proj) == 1 && fs::dir_exists(proj)) {
    return(
      rlang::set_names(
        proj,
        fs::path_file(proj)
      )
    )
  }

  proj_location <- dirs %||% default_dirs() %||% getOption("usethis.destdir")
  directories <- fs::dir_ls(
    proj_location,
    type = "directory",
    recurse = FALSE,
    regexp = ".Rcheck|_files",
    invert = TRUE
  )

  projects <- rlang::set_names(x = as.character(directories), nm = fs::path_file)

  if (!is.null(proj)) {
    if (!grepl("~/", proj, fixed = TRUE)) {
      # try to catch an invalid path
      rlang::arg_match0(
        proj,
        names(projects)
      )
      projects <- projects[proj]
    } else {
      cli::cli_abort(c("Can't find the project location for {.val {proj}} using {.topic fs::path}."))
    }
  }
  projects
}

default_dirs <- function() {
  fs::path_home("Documents", c("rrr", "rrr-forks"))
}

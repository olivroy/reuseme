#' Opens a RStudio project in a new session
#'
#' If not specified, will generate hyperlinks that call [usethis::proj_activate()].
#' `proj_switch()` looks at `options(reuseme.reposdir)`.
#'
#' @param proj the name of a project located in the default locations or `NA`
#' @param new_session Should open in a new session?
#'
#' @returns Single logical value indicating if current session is modified.
#' @export
#' @seealso [usethis::proj_activate()]
#' @family project management helpers
proj_switch <- function(proj = NA, new_session = TRUE) {
  # This is my default places (reuseme.reposdir possibly)
  # See fs::path_home
  project <- proj_path(proj)

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


#' Access the file outline within other project
#'
#' It can be used as [file_outline()] + `proj`.
#'
#' @param file A filename or regexp to a file inside `proj`
#' @param path a project path  [proj_path()]. If `NULL`,
#'   will return active project.
#' @param pattern A regular expression to look for
#' @return The file outline if multiple matches are found
#' @export
#'
#' @examples
#' try(proj_file("A non-existent file"))
#' @family project management helpers
proj_file <- function(file = NULL, path = active_rs_proj(), pattern = NULL) {
  rlang::check_required(file)
  check_proj(path, allow_null = TRUE)

  # search will only be conducted with pattern
  if (is.null(pattern) && is.null(file)) {
    cli::cli_abort(
      "One of {.arg pattern} or {.arg file} must exist."
    )
  }

  file <- file %||% "A non-existent rubbish file placeholder"

  if (fs::is_file(file)) {
    file_outline(file)
    open_rs_doc(file)
    return(invisible(file))
  }

  file_path <- proj_path(path, file)

  if (fs::is_file(file_path)) {
    file_outline(path = file_path)
    open_rs_doc(file_path)
    return(invisible(file_path))
  }

  file_exts <- c("R", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")
  possible_files <- fs::dir_ls(proj_path, regexp = file_exts_regex, recurse = TRUE, type = "file")
  possible_files <- fs::path_filter(possible_files, regexp = file)

  if (length(possible_files) > 1L) {
    # exclude these files if multiple matches
    possible_files <- fs::path_filter(possible_files, regexp = "_snaps|testthat/test-", invert = TRUE)
  }

  if (length(possible_files) == 0L) {
    if (is.null(pattern)) {
      cli::cli_abort("No match found for {.val {file}} in {.file {proj_path}}")
    } else if (fs::is_dir(file_path)) {
      return(dir_outline(path = file_path, pattern = pattern))
    } else {
      return(file_outline(path = fs::path_dir(file_path), pattern = pattern))
    }
  }

  if (length(possible_files) == 1L) {
    return(file_outline(possible_files, pattern = pattern))
  }

  cli::cli_inform(c( # TODO improve on this message
    "A couple files found. Access the desired place."
  ))
  file_outline(path = possible_files, pattern = pattern)
}

#' Specify `proj` in functions
#'
#' @description
#' * `proj_`
#' Two main ways to specify proj:
#' * Set `options(reuseme.reposdir)` in `.Rprofile` that contains a character
#'   vector of paths to your projects. (i.e. `~/rrr` that contains all your projects)
#' * Specify the full path to `proj`. (like you would for usethis function)
#'
#' @param proj A project path or name to match. If `NA`, returns all projects.
#'   If `NULL`, returns the active project.
#' @param dirs The directories in which we want to list projects.
#'
#' @returns A named character vector with the project name as name, and path as value.
#'   If `proj` is supplied
#' @export
#' @family project management helpers
#' @export
#' @inheritParams fs::path
proj_path <- function(proj = NA, ..., dirs = getOption("reuseme.reposdir")) {
  proj <- proj_list_internal(proj = proj, dirs = dirs)
  fs::path(proj, ...)
}

#' @export
#' @rdname proj_path
proj_list <- function(proj = NA, dirs = getOption("reuseme.reposdir")) {
  # lifecycle::deprecate_warn(
  #   when = "0.1.1",
  #   what = "proj_list()",
  #   with = "proj_path()",
  #   details = "More convenient. Didn't find it too useful to list all projects most of the time."
  # )
  proj_list_internal(proj = proj, dirs = dirs)
}
proj_list_internal <- function(proj, dirs, call = rlang::caller_env()) {
  check_proj(proj, allow_null = TRUE, allow_na = TRUE)
  proj <- proj %||% proj_get2()

  if (!is.na(proj) && length(proj) == 1) {
    # avoid creating a nested project.
    # known direct
    if (!proj %in% c("pkgdown", "testthat", "R", "man", "tests", "inst", "src", "vignettes", "images")) {
      if (fs::dir_exists(proj)) {
        return(
          rlang::set_names(
            proj,
            fs::path_file(proj)
          )
        )
      }
    }
  }

  proj_location <- dirs %||% default_dirs() %||% getOption("usethis.destdir")
  directories <- fs::dir_ls(
    proj_location,
    type = "directory",
    recurse = FALSE,
    regexp = ".Rcheck|_files",
    invert = TRUE
  )

  projects <- rlang::set_names(as.character(directories), fs::path_file)

  if (!is.na(proj) && anyDuplicated(names(projects)) > 0) {
    which_duplicate <- names(projects)[duplicated(names(projects))]
    if (proj %in% which_duplicate) {
      # R only returns
      duplicates_to_resolve <- projects[which(names(projects) == proj)]
      cli::cli_abort(c(
        "{which_duplicate} can be found in more than one location: {.file {duplicates_to_resolve}}.",
        "i" = "Specify the full path to disambiguate." # ! (This may be improved in the future)
      ))
    }
  }

  if (!is.na(proj)) {
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

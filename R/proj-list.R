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


#' Access the file outline within other project
#'
#' It can be used as [file_outline()] + `proj`.
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
  # avoid indexing roxy comments.
  withr::local_options(
    "reuseme.roxy_parse" = FALSE
  )
  # search will only be conducted with pattern
  if (is.null(pattern) && is.null(file)) {
    cli::cli_abort(
      "One of {.arg pattern} or {.arg file} must exist."
    )
  }
  file <- file %||% "A non-existent rubbish file placeholder"
  if (fs::is_file(file)) {
    file_outline(path = file)
    open_rs_doc(file)
    return(invisible(file))
  }
  proj <- proj %||% proj_get2()
  proj_path <- proj_list(proj)
  file_path <- fs::path(proj_path, file)
  if (fs::is_file(file_path)) {
    file_outline(path = file_path)
    open_rs_doc(file_path)
    return(invisible(file_path))
  }
  file_exts <- c("R", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")
  possible_files <- fs::dir_ls(proj_path, regexp = file_exts_regex, recurse = TRUE)
  possible_files <- fs::path_filter(possible_files, regexp = file)
  if (length(possible_files) > 1L) {
    # exclude these files if multiple matches
    possible_files <- fs::path_filter(possible_files, regexp = "_snaps|testthat/test-", invert = TRUE)
  }
  if (length(possible_files) == 0L) {
    if (is.null(pattern)) {
      cli::cli_abort("No match found for {.val {file}} in {.file {proj_path}}")
    } else {
      return(proj_outline(
        pattern = pattern,
        proj = proj
      ))
    }
  }

  if (length(possible_files) == 1L) {
    return(file_outline(pattern = pattern, path = possible_files))
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

  if (!is.null(proj) && length(proj) == 1) {
    # avoid creating a nested project.
    # known direct
    if (!proj %in% c("pkgdown", "testthat", "R", "man", "tests", "inst", "src")) {
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

  projects <- rlang::set_names(x = as.character(directories), nm = fs::path_file)
  if (!is.null(proj) && anyDuplicated(names(projects)) > 0) {
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

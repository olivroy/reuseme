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
  all_projects <- proj_list()

  if (!is.null(proj)) {
    rlang::arg_match0(proj, values = names(all_projects))
    if (new_session) {
      usethis::proj_activate(unname(all_projects[proj]))
    } else {
      # remove if https://github.com/r-lib/usethis/pull/1954 is implemented.
      rstudioapi::openProject(path = unname(all_projects[proj]), newSession = FALSE)
    }
    return(invisible(all_projects[proj]))
  }

  bullets <- paste0("Open {.run [", names(all_projects), "](usethis::proj_activate('", unname(all_projects), "'))}")
  cli::cli_bullets(bullets)
}


#' Active a file at location
#'
#' @param file A filename or regexp to a file inside `proj`
#' @param proj a project path or file [proj_list()]
#' @param regex_outline A regular expression to look for
#' @return The file outline if multiple matches are found
#' @export
#'
#' @examples
#' try(proj_file("A non-existent file"))
#' @family project management helpers
proj_file <- function(file = NULL, proj = NULL, regex_outline = NULL) {
  rlang::check_required(file)
  # search will only be conducted with regex_outline
  if (is.null(regex_outline) && is.null(file)) {
    cli::cli_abort(c(
      "One of {.arg regex_outline} or {.arg file} must exist."
    ))
  }
  file <- file %||% "A non-existent rubbish file placeholder"
  if (fs::file_exists(file)) {
    file_outline(path = file)
    open_rs_doc(file)
  }
  proj <- proj %||% proj_get2()
  if (fs::dir_exists(proj)) {
    proj_path <- proj
  } else {
    proj_path <- proj_list()[proj]
  }

  file_exts <- c("R", "qmd", "Rmd", "md", "Rmarkdown")
  file_exts_regex <- paste0("*.", file_exts, "$", collapse = "|")
  possible_files <- fs::dir_ls(proj_path, regexp = file_exts_regex, recurse = TRUE)
  possible_files <- fs::path_filter(possible_files, regexp =  "_snaps", invert = TRUE)
  possible_files <- fs::path_filter(possible_files, regexp =  file)

  if (length(possible_files) == 0) {
    if (is.null(regex_outline)) {
      cli::cli_abort("No match found for {.val {file}} in {.file {proj_path}}")
    } else {
      return(proj_outline(
        regex_outline = regex_outline,
        proj = proj
      ))
    }
  }

  if (length(possible_files) == 1) {
    if (is.null(regex_outline)) {
      open_rs_doc(possible_files)
    } else {
      file_outline(regex_outline = regex_outline, path = possible_files)
    }
  }
  cli::cli_inform(c(
    "A couple files found. Access the desired place."
  ))
  file_outline(regex_outline = regex_outline, path = possible_files)
}

#' Returns a named project list options
#'
#' It peeks `options(reuseme.reposdir)` to find projects.
#'
#' @param dirs The directories in which we want to list projects.
#'
#' @return A named character vector with the project name as
#' @export
#' @family project management helpers
proj_list <- function(dirs = getOption("reuseme.reposdir")) {
  proj_location <- dirs %||% default_dirs() %||% getOption("usethis.destdir")
  directories <- fs::dir_ls(
    proj_location,
    type = "directory",
    recurse = FALSE,
    regexp = ".Rcheck|_files",
    invert = TRUE
  )

  purrr::set_names(x = as.character(directories), nm = fs::path_file)
}

default_dirs <- function() {
  fs::path_home("Documents", c("rrr", "rrr-forks"))
}

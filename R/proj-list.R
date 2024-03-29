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

#' Returns a named project list options
#'
#' It peeks `options(reuseme.reposdir)` to find projects.
#'
#' @param dirs The directories in which we want to list projects.
#'
#' @return A named character vector with the project name as
#' @export
proj_list <- function(dirs = getOption("reuseme.reposdir")) {
  proj_location <- dirs %||% default_dirs() %||% getOption("usethis.destdir")
  directories <- fs::dir_ls(
    proj_location,
    type = "directory",
    recurse = FALSE,
    regexp = ".Rcheck",
    invert = TRUE
  )

  purrr::set_names(x = as.character(directories), nm = fs::path_file)
}

default_dirs <- function() {
  fs::path_home("Documents", c("rrr", "rrr-forks"))
}

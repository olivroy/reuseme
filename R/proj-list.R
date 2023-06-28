#' Opens a RStudio project in a new session
#'
#' If not specified, will generate hyperlinks that call [usethis::proj_activate()].
#' `proj_switch()` looks at `options(reuseme.reposdir)`.
#' @param proj the name of a project located in the default locations or `NULL`
#'
#' @returns Single logical value indicating if current session is modified.
#' @export
#' @seealso [usethis::proj_activate()]
proj_switch <- function(proj = NULL) {
  # This is my default places (reuseme.reposdir possibly)
  # See fs::path_home
  all_projects <- proj_list()
  if (!is.null(proj)) {
    rlang::arg_match0(proj, values = names(all_projects))
    usethis::proj_activate(all_projects[proj])
    return(invisible(proj))
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
  fs::dir_ls(proj_location, type = "directory", recurse = FALSE) |>
    as.character() |>
    purrr::set_names(fs::path_file)
}

default_dirs <- function() {
  fs::path_home_r(c("rrr", "rrr-forks"))
}

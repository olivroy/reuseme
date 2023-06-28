#' Returns a named project list options
#'
#' It peeks `options(reuseme.reposdir)`
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

# Project utils --------------------------
#' copy of usethis::is_package
#'
#' @param base_path the path used
#'
#' @returns `TRUE` if is a package directory.
#' @noRd
#' @keywords internal
is_pkg <- function(base_path = proj_get()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}
proj_get <- function() {
  rprojroot::find_root(criterion = rprojroot::is_rstudio_project)
}

# OS utils ---------------------------------------------------------
# Copy of xfun::is_windows
is_windows <- function() {
  .Platform$OS.type == "windows"
}

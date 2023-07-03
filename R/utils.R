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
is_quarto_blog <- function(base_path = proj_get()) {
  is_quarto <- fs::file_exists("_quarto.yml")
  if (!is_quarto) {
    return(FALSE)
  }
  fs::dir_exists("posts")
}
check_active_qmd_post <- function(base_path = proj_get(), call = caller_env()) {
  # as long as rstudioapi is not consistent with fs on file paths, this may cause issues.
  full_doc_path <- normalizePath(
    path = rstudioapi::documentPath(),
    mustWork = TRUE,
    winslash = "/"
  )
  if (!identical(fs::path_common(c(full_doc_path, base_path)), base_path)) {
    cli::cli_abort(c(
      "Mess-up in the Quarto blog file path",
      x = "Use a file in the current project to make it work."
      ),
    .internal = TRUE,
    call = call
    )
  }
  relative_path <- fs::path_rel(full_doc_path)
  if (!fs::path_ext(relative_path) %in%  c("qmd", "md", "Rmd", "Rmarkdown")) {
    cli::cli_abort(c(
      x = "You must be calling the screenshot function when editing a Quarto document.",
      i = "Open a document then retry. or set `dir` to manually override the location of the screenshot."
    ),
    call = call)
  }
  fs::path_dir(relative_path)
}
proj_get <- function() {
  rprojroot::find_root(criterion = rprojroot::is_rstudio_project)
}

# OS utils ---------------------------------------------------------
# Copy of xfun::is_windows
is_windows <- function() {
  .Platform$OS.type == "windows"
}

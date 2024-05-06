# usethis adaptions utils --------------------------
#' copy of usethis::is_package
#'
#' @param base_path the path used
#'
#' @returns `TRUE` if is a package directory.
#' @noRd
is_pkg <- function(base_path = proj_get()) {
  res <- tryCatch(
    rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )

  !is.null(res)
}

is_quarto_blog <- function(base_path = proj_get()) {
  is_quarto <- fs::file_exists(fs::path(base_path, "_quarto.yml"))

  if (!is_quarto) {
    return(FALSE)
  }

  unname(fs::dir_exists(fs::path(base_path, "posts")))
}

# Active project / document ----------------------------------------------------


is_proj <- function(path) {
  withCallingHandlers(
    !is.null(rprojroot::find_root(criterion = rprojroot::criteria$is_rstudio_project, path = path)),
    error = function(e) FALSE
  )
}

proj_get <- function() {
  path <- rprojroot::find_root(criterion = rprojroot::is_rstudio_project)
  fs::path(path)
}

# If not in a project, warn and return working directory
proj_get2 <- function() {
  path <- tryCatch(
    rprojroot::find_root(criterion = rprojroot::is_rstudio_project),
    error = function(e) {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        cli::cli_warn("Not in a Project, returning the current working directory")
      }
      getwd() # wd is a criterion.. to see...
    }
  )
  fs::path(path)
}

get_active_qmd_post <- function(base_path = proj_get(), error_call = caller_env()) {
  # as long as rstudioapi is not consistent with fs on file paths, this may cause issues.
  base_path <- fs::path(unname(base_path))

  # For finding active project
  full_doc_path <-
    normalizePath(
      path = rstudioapi::getSourceEditorContext()$path,
      mustWork = TRUE,
      winslash = "/"
    )

  if (full_doc_path == "") {
    cli::cli_abort("problematic, did not get the document path.", .internal = TRUE, call = error_call)
  }

  # very similar to usethis:::in_active_proj (possibly could have a helper.)
  if (!identical(fs::path_common(c(full_doc_path, base_path)), base_path)) {
    cli::cli_abort(
      c(
        "Mess-up in the Quarto blog file path",
        "full doc path is {full_doc_path}, base_path is {base_path}",
        x = "Use a file in the current project to make it work."
      ),
      .internal = TRUE,
      call = error_call
    )
  }

  relative_path <- fs::path_rel(full_doc_path, start = base_path)
  if (!has_length(relative_path, 1)) {
    cli::cli_abort(
      "Debug: base_path = {base_path},
              dir = {dir}, relative_path = {relative_path},
              full_doc_path = {full_doc_path}",
      .internal = TRUE
    )
  }
  if (!fs::path_ext(relative_path) %in% c("qmd", "md", "Rmd", "Rmarkdown")) {
    cli::cli_abort(
      c(
        x = "{.fn screenshot} must be used when editing a qmd file.",
        i = "Open a qmd file or set `dir` to override the location of the screenshot."
      ),
      call = error_call
    )
  }

  fs::path_dir(relative_path)
}

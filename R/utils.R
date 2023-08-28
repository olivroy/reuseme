# usethis adaptions utils --------------------------
#' copy of usethis::is_package
#'
#' @param base_path the path used
#'
#' @returns `TRUE` if is a package directory.
#' @noRd
#' @keywords internal
is_pkg <- function(base_path = proj_get()) {
  res <- tryCatch(
    rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )

  !is.null(res)
}

# copied from usethis:::read_utf8
read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

# Wrapper around [usethis::write_union()] to add TODO items in a specific R
# Redone with cli. (Could open an issue to add creating )
write_union2 <- function(path, lines, quiet = FALSE) {
  check_name(path)
  check_character(lines)
  check_bool(quiet)
  path <- fs::path_expand(path)

  if (fs::file_exists(path)) {
    existing_lines <- read_utf8(path)
    new_flag <- FALSE
  } else {
    existing_lines <- character()
    new_flag <- TRUE

    if (!quiet) {
      cli::cli_alert_success("Creating {.file {path}}")
    }
  }

  new <- setdiff(lines, existing_lines)

  if (length(new) == 0) {
    return(invisible(FALSE))
  }

  if (!quiet) {
    msg <- paste0("Adding {.val {new}}", if (!new_flag) " to {.file {path}}")
    cli::cli_alert_success(msg)
  }

  all <- c(existing_lines, new)
  usethis::write_over(path = path, lines = all, quiet = TRUE, overwrite = TRUE)
}


# Quarto blog utils -------------------------------------------------
is_quarto_blog <- function(base_path = proj_get()) {
  is_quarto <- fs::file_exists(fs::path(base_path, "_quarto.yml"))

  if (!is_quarto) {
    return(FALSE)
  }

  unname(fs::dir_exists(fs::path(base_path, "posts")))
}

get_active_qmd_post <- function(base_path = proj_get(), dir = NULL, call = caller_env()) {
  # as long as rstudioapi is not consistent with fs on file paths, this may cause issues.
  is_active_proj <- identical(proj, proj_get2())
  base_path <- fs::path(unname(base_path))


  # For finding active project
  full_doc_path <-
    if (!is_active_proj) {
      fs::path_real(fs::path(dir, "index.qmd"))
    } else {
      normalizePath(
        path = rstudioapi::documentPath(),
        mustWork = TRUE,
        winslash = "/"
      )
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
      call = call
    )
  }

  relative_path <- fs::path_rel(full_doc_path, start = base_path)

  if (!fs::path_ext(relative_path) %in% c("qmd", "md", "Rmd", "Rmarkdown")) {
    cli::cli_abort(
      c(
        x = "You must be calling the screenshot function when editing a Quarto document.",
        i = "Open a document then retry. or set `dir` to manually override the location of the screenshot."
      ),
      call = call
    )
  }

  fs::path_dir(relative_path)
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
      cli::cli_warn("Not in a Project, returning the current working directory")
      getwd()
    }
  )
  fs::path(path)
}

# OS utils ---------------------------------------------------------
# Copy of xfun::is_windows
is_windows <- function() {
  .Platform$OS.type == "windows"
}

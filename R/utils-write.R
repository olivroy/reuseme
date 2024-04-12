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

  if (!fs::is_dir(path) && fs::file_exists(path)) {
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
  invisible(path)
}

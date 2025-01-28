#' Move temporary file automatically from the R console
#'
#' It works well when you have no API to download a file, but still want a fast R implementation.
#'
#' @param destdir The desired directory to send this to
#' @export
#' @seealso [file_rename_auto()]
file_move_temp_auto <- function(destdir) {
  rlang::check_required(destdir)
  if (dir.exists(destdir)) {
    destdir <- fs::path_expand_r(destdir)
  }
  if (!fs::is_dir(destdir)) {
    cli::cli_abort(c(
      "Can't copy file to destdir",
      i = "{.path {destdir}} doesn't exist."
    ))
  }
  most_recent_file_in_downloads_folder <- fs::dir_info(c("~/Downloads", "~/Desktop"))
  source_file <- most_recent_file_in_downloads_folder[[
    which.max(most_recent_file_in_downloads_folder$modification_time),
    "path"
  ]]
  diff_time <- difftime(Sys.time(), file.mtime(source_file), units = "mins")
  if (is.na(diff_time) || diff_time > 60) {
    rlang::check_installed("prettyunits")
    cli::cli_abort("{.file {source_file}} was created {prettyunits::pretty_dt(diff_time)} ago.")
  }
  destfile <- fs::path(destdir, fs::path_file(source_file))
  fs::file_move(source_file, destfile)
  cli::cli_inform(c(
    v = "Successfully moved {.file {source_file}} to {.file {destfile}}.",
    "Open with {.run fs::file_show('{as.character(destfile)}')}",
    "For new name, call `reuseme::file_rename_auto('better-name-sans-ext')` immediately."
  ))
  invisible(destfile)
}

#' Move file automatically between folders
#'
#' @description
#' * `file_rename_auto()` automatically renames your file to a better name while keeping the same folder structure
#' * `file_move_dir_auto()` automatically moves your file while keeping the same file name
#' * `file_copy_auto()` automatically copies your file while keeping the same file name (Useful to copy read-only files).
#'
#' # Advantages
#'
#' Instead of calling `fs::file_move("path/to/dir/file.R", "path/to/dir/new-file.R")`, you can just call
#' `file_rename_auto("new-file", "path/to/dir/file.R")`
#'
#' Instead of calling `fs::file_move("path/to/dir/file.R", "path/to/new-dir/file.R")`, you can just call
#' `file_move_auto("new-dir", "path/to/dir/file.R")`
#'
#' If the functions are used in conjunction with [file_move_temp_auto()],
#'
#' @export
#' @param new_name,new_dir New directory or file name (without extension)
#' @param old_file The old file name
#'
#' @returns The new full path name, invisibly, allowing you to call the functions another time.
file_rename_auto <- function(new_name, old_file = .Last.value) {
  if (!fs::is_file(old_file)) {
    cli::cli_abort("incorrect context for {.fn file_rename_auto}.")
  }
  # path_dir() behaves weirdly if not an fs path
  # fs::path_dir("~/")
  # fs::path_dir(fs::path("~))
  # Workaround r-lib/fs#459
  old_path <- fs::path_real(old_file)
  ext <- fs::path_ext(old_file)
  # TRICK {{wf}} path_ext_set replaces or appends extension
  new_name <- fs::path_ext_set(new_name, ext)
  new_path <- fs::path(
    fs::path_dir(old_path),
    fs::path_file(new_name)
  )
  fs::file_move(old_path, new_path)
  cli::cli_inform(c(
    v = "Successfully moved {.file {old_path}} to {.val {new_name}}.",
    "Open with {.run fs::file_show('{as.character(new_path)}')}",
    "For new name, call `reuseme::file_rename_auto('better-name-sans-ext')` immediately.",
    "For new dir, call `reuseme::file_move_auto('better-name-sans-ext')` immediately."

  ))
  invisible(new_path)
}

#' @export
#' @rdname file_rename_auto
file_move_dir_auto <- function(new_dir, old_file = .Last.value) {
  if (!fs::file_exists(old_file) || fs::is_dir(old_file)) {
    cli::cli_abort("Incorrect usage. {.arg old_file} = {.file {old_file}} doesn't exist.")
  }

  old_file_name <- fs::path_file(old_file)
  new_file_name <- fs::path(new_dir, old_file_name)
  fs::file_move(
    old_file,
    new_file_name
  )
  cli::cli_inform(c(
    v = "Successfully moved {.val {old_file_name}} to {.file {new_file_name}}.",
    "i" = "Call `reuseme::file_rename_auto('new-file-with-no-ext')` to rename"
  ))
  invisible(new_file_name)
}

#' @export
#' @rdname file_rename_auto
file_copy_auto <- function(new_dir, old_file = .Last.value) {
  if (!fs::file_exists(old_file) || fs::is_dir(old_file)) {
    cli::cli_abort("Incorrect usage. {.arg old_file} = {.file {old_file}} doesn't exist.")
  }

  old_file_name <- fs::path_file(old_file)
  new_file_name <- fs::path(new_dir, old_file_name)
  fs::file_copy(
    old_file,
    new_file_name
  )
  cli::cli_inform(c(
    v = "Successfully copied {.val {old_file_name}} to {.file {new_file_name}}.",
    "i" = "Call `reuseme::file_rename_auto('new-file-with-no-ext')` to rename"
  ))
  invisible(new_file_name)
}

# 1. file_move shortcuts
# file_move_auto() is a wrapper of file_move_dir()
# file_rename()
# file_move_dir()
# move dir -> same name
# rename (same dir)




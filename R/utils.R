# OS utils ---------------------------------------------------------
# Copy of xfun::is_windows
is_windows <- function() {
  .Platform$OS.type == "windows"
}

basename_remove_ext <- function(x) {
  fs::path_ext_remove(basename(x))
}

# OS utils ---------------------------------------------------------
# Copy of xfun::is_windows
is_windows <- function() {
  .Platform$OS.type == "windows"
}

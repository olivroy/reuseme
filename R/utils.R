# OS utils ---------------------------------------------------------
# Copy of xfun::is_windows
is_windows <- function() {
  .Platform$OS.type == "windows"
}

basename_remove_ext <- function(x) {
  fs::path_ext_remove(basename(x))
}

# Since we are only expecting .R .Rmd files in outline
s_file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

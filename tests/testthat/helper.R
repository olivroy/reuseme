expect_complete_todo <- function(object, warn = FALSE) {
  if (warn) {
    expect_warning(object, "Could not find") |>
      expect_message(regexp = "Removed")
  } else {
    expect_message(object, regexp = "Removed")
  }
}

# to snapshot the exact match match of regex before running snapshots.
matches_regex <- function(x, regex, group = NULL) {
  regex <- common_regex(regex)
  stringr::str_extract(x, regex, group = group)
}

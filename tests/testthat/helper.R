expect_complete_todo <- function(object, warn = FALSE) {
  if (warn) {
    expect_warning(object, "Could not find") |>
      expect_message(regexp = "Removed")
  } else {
    expect_message(object, regexp = "Removed")
  }
}

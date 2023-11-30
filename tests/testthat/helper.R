expect_mark_as_complete <- function(object, warn = FALSE) {
  if (warn) {
    expect_warning(object, "Could not find") %>%
      expect_message(regexp = "Marking") %>%
      expect_message(regexp = "Writing")
  } else {
    expect_message(object, regexp = "Marking") %>%
      expect_message(regexp = "Writing")
  }
}

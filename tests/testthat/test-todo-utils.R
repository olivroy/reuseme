test_that("Marking TODO as done detects tags", {
  expect_equal(
    extract_tag_in_text(c("TODO I am crazy")),
    "TODO"
  )
  expect_error(
    extract_tag_in_text(c("TODO ", "only length 1 allowed"))
  )
  expect_equal(
    extract_tag_in_text("FIXMES"),
    "FIXME"
  )
  # Only extracts the first tag.
  expect_equal(
    extract_tag_in_text("WORK I need TODO this"),
    "WORK"
  )
})

test_that("Marking a TODO item as done works", {
  tmp <- tempfile(fileext = "R")
  content <- c(
    "# I Want this done",
    "# TODO item to delete",
    "# WORK Explain what the next code does.",
    "# TODO with {.href [cli hyperlinks](https://cli.r-lib.org/reference/links.html)}",
    "# FIXME Repair this function",
    "print('R code')"
  )
  writeLines(text = content, con = tmp)
  expect_mark_as_done <- function(object) {
    expect_message(object, regexp = "Marking") %>%
      expect_message(regexp = "Writing")
  }

  # Can't delete the first line as it doesn't contain a TODO item (tmp still has 5 lines)
  expect_error(
    mark_todo_as_complete(line_id = 1, file = tmp, regexp = "I want this done")
  )
  # Trying to delete TODO item at line 2 without providing the regexp for safeguard
  expect_error(
    mark_todo_as_complete(line_id = 2, file = tmp),
    regexp = "`regexp` is absent"
  )
  # Deleting the TODO item line completely (tmp now has 4 lines)
  expect_mark_as_done(out <- mark_todo_as_complete(line_id = 2, file = tmp, regexp = "item to delete"))
  expect_equal(
    out,
    ""
  )
  # trying to delete the original 3rd line (that is now the second line after deletion)
  expect_error(
    mark_todo_as_complete(line_id = 3, file = tmp, regexp = "Explain what the next code does")
  )
  # Deleting the WORK tag (on new line 2), but keeping the comment.
  expect_mark_as_done(out <- mark_todo_as_complete(line_id = 2, file = tmp, regexp = "Explain what the next code does"))
  expect_equal(
    out,
    "# Explain what the next code does."
  )
  expect_equal(
    readLines(tmp),
    c(
      "# I Want this done",
      "# Explain what the next code does.",
      "# TODO with {.href [cli hyperlinks](https://cli.r-lib.org/reference/links.html)}",
      "# FIXME Repair this function",
      "print('R code')"
    )
  )
  unlink(tmp)
})

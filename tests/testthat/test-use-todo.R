test_that("Marking TODO as done detects tags", {
  expect_equal(
    extract_tag_in_text("TODO I am crazy"),
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
    "# TODO Check r-lib/usethis#1890",
    "# TODO Check https://github.com/r-lib/usethis/issues/1890",
    "print('R code')"
  )
  writeLines(text = content, con = tmp)
  # test use_todo
  expect_message(use_todo("Another TODO", tmp))
  # tmp still has 7 lines
  expect_snapshot(error = TRUE, {
    # Can't delete the first line as it doesn't contain a TODO item
    mark_todo_as_complete(line_id = 1, file = tmp, regexp = "I Want this done")
    # Try to delete TODO item at line 2 without providing the regexp for safeguard
    mark_todo_as_complete(line_id = 2, file = tmp)
  })
  # Deleting the TODO item line completely (tmp now has 4 lines)
  expect_mark_as_complete(
    mark_todo_as_complete(line_id = 2, file = tmp, regexp = "item to delete")
  )
  # Deleting the WORK tag (on new line 2), but keeping the comment.
  # Will throw a warning for now.

  expect_mark_as_complete(
    out <- mark_todo_as_complete(
      line_id = 3,
      file = tmp,
      regexp = "Explain what the next code does"
    ),
    warn = TRUE
  )
  expect_equal(
    out,
    "# Explain what the next code does."
  )
  expect_mark_as_complete(
    out <- mark_todo_as_complete(
      line_id = 4,
      file = tmp,
      regexp = "ethisissues1890"
    ),
    warn = TRUE
  )
  expect_equal(
    read_utf8(tmp),
    c(
      "# I Want this done",
      "# Explain what the next code does.",
      "# TODO with {.href [cli hyperlinks](https://cli.r-lib.org/reference/links.html)}",
      "# FIXME Repair this function",
      "# TODO Check https://github.com/r-lib/usethis/issues/1890",
      "print('R code')",
      "# TODO Another TODO"
    )
  )
  unlink(tmp)
  skip("mark_todo_as_complete fails if changing lines + regexp match in many places. Many add a condition like closest")
})

test_that("use_todo global works", {
  skip_on_cran()
  skip_on_ci()
  expect_no_error(
    path <- use_todo("global::it it time")
  )
  expect_no_error(
    path <- use_todo("global::print()", code = TRUE)
  )
})

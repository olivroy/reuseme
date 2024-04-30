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
    "2^2 # TODO item to delete",
    "2^2 # WORK Explain what the next code does.",
    "# TODO with {.href [cli hyperlinks](https://cli.r-lib.org/reference/links.html)}",
    "# FIXME Repair this function",
    "   # TODO Check r-lib/usethis#1890",
    "# TODO Check https://github.com/r-lib/usethis/issues/1890"
  )
  writeLines(text = content, con = tmp)
  suppressMessages(use_todo("print('R code')", tmp, code = TRUE))
  # test use_todo
  expect_message(use_todo("Another TODO", tmp))
  # tmp still has 7 lines
  expect_snapshot(error = TRUE, {
    # Can't delete the first line as it doesn't contain a TODO item
    complete_todo(line_id = 1, file = tmp, regexp = "I Want this done")
    # Try to delete TODO item at line 2 without providing the regexp for safeguard
    complete_todo(line_id = 2, file = tmp)
  })
  # Deleting the TODO item line completely (tmp now has 4 lines)
  expect_complete_todo(
    complete_todo(line_id = 2, file = tmp, regexp = "item to delete")
  )
  # Deleting the work tag (on new line 2), but keeping the comment.
  # Will throw a warning for now.

  expect_complete_todo(
    out <- complete_todo(
      line_id = 3,
      file = tmp,
      regexp = "Explain what the next code does"
    ),
    warn = TRUE
  )
  expect_equal(
    out,
    "2^2 # Explain what the next code does."
  )
  expect_complete_todo(
    out <- complete_todo(
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
      "2^2 ",
      "2^2 # Explain what the next code does.",
      "# TODO with {.href [cli hyperlinks](https://cli.r-lib.org/reference/links.html)}",
      "# FIXME Repair this function",
      "   ",
      "# TODO Check https://github.com/r-lib/usethis/issues/1890",
      "print('R code')",
      "# TODO Another TODO"
    )
  )
  unlink(tmp)
  skip("complete_todo fails if changing lines + regexp match in many places. Many add a condition like closest")
})

test_that("use_todo global works", {
  skip_on_cran()
  skip_on_ci()
  expect_no_error(
    suppressMessages(path <- use_todo("global::it is time"))
  )
  line_to_delete <- length(readLines(path, encoding = "UTF-8"))
  suppressMessages(complete_todo(file = path, regexp = "it is time", line_id = line_to_delete, rm_line = TRUE))
})

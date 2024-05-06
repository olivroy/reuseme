# Test individual components of the outline ------
test_that("o_is_roxygen_comment() works", {
  expect_true(o_is_roxygen_comment("#' @param"))
  expect_equal(
    o_is_roxygen_comment(
      c("#' @param", "# a comment", "#' a qmd things?"),
      c("R", "R", "qmd")
    ),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("o_is_todo_fixme() works", {
  expect_true(o_is_todo_fixme("# TODO: go for it"))
  # avoid finding comments.
  expect_false(o_is_todo_fixme("# another TODO item"))
  expect_true(o_is_todo_fixme("  # WORK this out"))
  expect_true(o_is_todo_fixme("  # TODO this is important"))
  expect_true(o_is_todo_fixme("  # FIXME this is important"))
  expect_false(o_is_todo_fixme("  expect_true(o_is_todo_fixme(\"  # TODO this is important\"))"))
})

test_that("o_is_work_item() works", {
  expect_true(o_is_work_item("# WORK this needs to be done."))
})

test_that("o_is_test_that() works", {
  expect_true(o_is_test_that('test_that("Serious things are happening"'))
})

test_that("o_is_generic_test() works", {
  expect_true(o_is_generic_test('test_that("Serious things work properly"'))
})

test_that("o_is_object_title() works", {
  expect_true(o_is_object_title("title = 'A great'"))
})

test_that("o_is_section_title() works", {
  expect_true(o_is_section_title("# Analysis of this"))
  expect_false(o_is_section_title("# TidyTuesday"))
  expect_true(o_is_section_title("  # section 1 ----"))
})

test_that("o_is_commented_code() works", {
  expect_true(o_is_commented_code("# DiagrammeR(x = 1,"))
  expect_true(o_is_commented_code("# DiagrammeR(x = 1)"))
  expect_true(o_is_commented_code("#' # DiagrammeR(x = 1)"))
  expect_true(o_is_commented_code("# DiagrammeR(x = 1\""))

  expect_false(o_is_commented_code("# A new section {.unnumbered}"))
})

test_that("No outline criteria are untested", {
  skip_on_ci()
  skip_on_cran()
  # using technique in tidyverse/ggplot2#5754
  outline_crit <-
    stringr::str_subset(ls("package:reuseme"), "^o_is")
  # Add tests if it fails.
  expect_snapshot(
    cat(outline_crit, sep = "\n")
  )
})

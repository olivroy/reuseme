# Test individual outline elements ------
test_that("o_is_notebook() works", {
  expect_true(o_is_notebook("#' ---", "file.R", file_ext = "R", line = 1))
  expect_false(o_is_notebook("#' ---", "file.R", file_ext = "qmd", line = 1))
  expect_false(o_is_notebook("#' Fn title", "file.R", file_ext = "qmd", line = 1))
})

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
  expect_true(o_is_todo_fixme("  # WORK this out"))
  expect_true(o_is_todo_fixme("  # TODO this is important"))
  expect_true(o_is_todo_fixme("  # FIXME this is important"))
  expect_false(o_is_todo_fixme("  expect_true(o_is_todo_fixme(\"  # TODO this is important\"))"))
  # avoid finding comments.
  expect_false(o_is_todo_fixme("# another TODO item"))
  expect_false(o_is_todo_fixme("#' TODO, WORK, FIXME)", is_roxygen_comment = T))
})

test_that("o_is_test_name() works", {
  expect_true(o_is_test_name('test_that("Serious things are happening", {'))
  expect_true(o_is_test_name('describe("This is happening", {'))
  expect_false(o_is_test_name('test_that("", {'))
})

test_that("o_is_generic_test() works", {
  expect_true(o_is_generic_test('test_that("Serious things work properly"'))
})

test_that("o_is_tab_plot_title() works", {
  expect_true(o_is_tab_plot_title("title = 'A great'"))
  expect_true(o_is_tab_plot_title("title = md('A great')"))
  
  expect_false(o_is_tab_plot_title("tab_header()"))
  expect_false(o_is_tab_plot_title("```{r tab_header}"))
  expect_false(o_is_tab_plot_title("fwd_title = 'Family'"))
  expect_false(o_is_tab_plot_title("guide_legend(title = 'Family'"))
  expect_false(o_is_tab_plot_title("title = ''"))
  expect_false(o_is_tab_plot_title("title = '', symbol = 'x'"))

  expect_false(o_is_tab_plot_title('title = ".+", " +\\(",")'))
  expect_false(o_is_tab_plot_title("dc:title = 'aaaaaaa'"))
})

test_that("o_is_section_title() works", {
  expect_true(o_is_section_title("# Analysis of this"))
  expect_true(o_is_section_title("  # section 1 ----"))
  expect_false(o_is_section_title("# TidyTuesday"))
  # not considering roxygen sections anymore.
  expect_false(o_is_section_title("#' # A very interesting section"))
})

test_that("o_is_cli_info() works", {
  expect_true(o_is_cli_info("cli::cli_ul('this')"))
  expect_false(o_is_cli_info("\"cli::cli_ul('aa')"))
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

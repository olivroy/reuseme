test_that("escape_markup() works", {
  expect_equal(escape_markup(c("gt", "y {")), c("gt", "y {{"))
  expect_equal(escape_markup(c("gt", "y {", "{gt}")), c("gt", "y {{", "{{gt}}"))
  expect_equal(escape_markup("{gt}"), "{{gt}}")
  expect_equal(escape_markup("{.file {here}}"), "{.file {.url here}}")

  input <- "multi problems {{gt}} to {gt} to {.file gt} to {.file {gt}}"
  exp_str <- "multi problems {{gt}} to {{gt}} to {.file gt} to {.file {.url gt}}"
  expect_equal(escape_markup(input), exp_str)
  input <- "{.href [cran]({cran_home})}"
  exp_str <- "<cran>"
  expect_equal(escape_markup(input), exp_str)
  input <- "{fn}({arg})"
  exp_str <- "fn({{arg}})"
  expect_equal(escape_markup(input), exp_str)
  escape_markup("Marking {.code {line_content}} as done! ")
  input <- "date is {.path okay} to {release_date}."
  exp_str <- "date is {.path okay} to {{release_date}}."
  expect_equal(escape_markup(input), exp_str)
  expect_snapshot({
    escape_markup("i{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
    escape_markup("{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
  })
  # No error when formatting inline! (end goal)
  expect_no_error(
    cli::format_inline(escape_markup(c(
      "i{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}."
    ))
  ))
})

# test helpers

test_that("replace_r_var() works", {
  expect_snapshot({
    replace_r_var("i{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
  })
})

test_that("is_markup_incorrect() works", {
  expect_true(is_markup_incorrect("{gt}"))
  expect_false(is_markup_incorrect("{{gt}}"))
  expect_true(is_markup_incorrect("{.file {gt}}"))
  expect_false(is_markup_incorrect("{.file x}"))
})

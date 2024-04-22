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
  skip("Not ready")
  input <- "{fn}({arg})"
  exp_str <- "fn({{arg}})"
  expect_equal(escape_markup(input), exp_str)
  escape_markup("Marking {.code {line_content}} as done! ")
})

test_that("is_markup_okay() works", {
  expect_false(is_markup_okay("{gt}"))
  expect_true(is_markup_okay("{{gt}}"))
  expect_false(is_markup_okay("{.file {gt}}"))
  expect_true(is_markup_okay("{.file x}"))
})


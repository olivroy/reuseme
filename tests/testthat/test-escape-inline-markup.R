test_that("escape_markup() works", {
  expect_equal(escape_markup(c("gt", "y {")), c("gt", "y {{"))
  expect_equal(escape_markup(c("gt", "y {", "{gt}")), c("gt", "y {{", "{{gt}}"))
  expect_equal(escape_markup("{gt}"), "{{gt}}")
  expect_equal(escape_markup("{.file {here}}"), "{.file {.url here}}")

  input <- "multi problems {{gt}} to {gt} to {.file gt} to {.file {gt}}"
  exp_str <- "multi problems {{gt}} to {{gt}} to {.file gt} to {.file {.url gt}}"
  expect_equal(escape_markup(input), exp_str)
})

test_that("is_markup_okay() works", {
  expect_false(is_markup_okay("{gt}"))
})


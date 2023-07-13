test_that("Returns identity", {
  withr::local_options(rlang_interactive = FALSE)
  expect_equal(
    names_identity(mtcars),
    mtcars
  )
})

test_that("Side effects are what's intended in interactive sessions", {
  withr::local_options(rlang_interactive = TRUE)
  expect_snapshot({
    dplyr::starwars %>%
      filter_identity(name == "C3P-O", extra_msg = c("Looking at C3P-0 characteristics")) %>%
      distinct_identity(hair_color, sex, extra_msg = c("Looking if there is association between `hair_color` and `sex`. ", "Printing 15 rows."), nrows = 15)
  }
  )
})

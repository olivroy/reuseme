test_that("Returns identity", {
  withr::local_options(rlang_interactive = FALSE)
  expect_equal(
    names_identity(mtcars),
    mtcars
  )
})

test_that("Side effects are what's intended in interactive sessions", {
  withr::local_options(rlang_interactive = TRUE)
  withr::local_seed(100)
  withr::local_options(pillar.advice = FALSE, pillar.max_footer_lines = 1)
  f_identity <- function(data) {
    data %>%
      filter_identity(name == "C3P-O", extra_msg = c("Looking at C3P-0 characteristics")) %>%
      slice_sample_identity(
        n = 5,
        extra_msg = "Check random rows here"
      ) %>%
      slice_group_sample_identity(
        n_groups = 2,
        group_var = sex,
        extra_msg = c("Looking at all individuals of a sex.")
      ) %>%
      slice_min_max_identity(
        height,
        each = TRUE,
        n = 4,
        extra_msg = c("Looking at people with min and max height (4 each) total = 8 rows.")
      ) %>%
      distinct_identity(
        hair_color,
        sex,
        extra_msg = c("Looking if there is association between `hair_color` and `sex`. ", "Printing 15 rows."),
        nrows = 15
      )
  }
  expect_snapshot(f_identity(dplyr::starwars))
})

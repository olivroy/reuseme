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
  skip_if_not_installed("dplyr", "1.1.4")
  f_identity <- function(data) {
    data |>
      filter_identity(name == "C-3PO", extra_msg = c("Looking at C3P-0 characteristics")) |>
      filter_if_any_identity(
        name == "Non-existent person",
        extra_msg = "No one is named Non-existent."
      ) |>
      slice_sample_identity(
        n = 5,
        extra_msg = "Check random rows here",
        nrows = 6
      ) |>
      slice_group_sample_identity(
        n_groups = 2,
        group_var = sex,
        extra_msg = c("Looking at all individuals of a sex.")
      ) |>
      filter_if_any_identity(
        name == "C-3PO",
        hair_color == "brown",
        nrows = 5,
        extra_msg = "Looking at everyone that is C3P-0 or has brown hair."
      ) |>
      slice_min_max_identity(
        height,
        each = TRUE,
        n = 4,
        extra_msg = c("Looking at people with min and max height (4 each) total = 8 rows.")
      ) |>
      distinct_identity(
        hair_color,
        sex,
        .arrange = TRUE,
        extra_msg = c("Looking if there is association between `hair_color` and `sex`. ", "Printing 15 rows."),
        nrows = 4
      )
  }
  expect_snapshot(f_identity(dplyr::starwars))
})

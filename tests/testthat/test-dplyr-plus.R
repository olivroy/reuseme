test_that("slice_group_sample() works as expected", {
  withr::local_seed(100)
  mtcars_grouped_vs <- dplyr::group_by(mtcars, vs)
  expect_message(
    group_vs_sampled <- slice_group_sample(mtcars_grouped_vs, group_var = am),
    regexp = "Ignoring"
  )
  # trying to do two tests at once here.
  expect_equal(
    withr::with_seed(100, slice_group_sample(mtcars, group_var = vs)),
    dplyr::ungroup(group_vs_sampled)
  )
  # ungrouped
  expect_error(
    slice_group_sample(mtcars),
    regexp = "grouped"
  )
})

test_that("`filter_if_any()` works as expected", {
  expect_equal(
    dplyr::starwars %>%
      dplyr::mutate(v1 = birth_year > 10, .by = gender) %>%
      dplyr::filter(
        dplyr::if_any(matches("v\\d")),
        .by = gender
      ) %>%
      dplyr::select(-matches("v\\d")),
    dplyr::starwars %>%
      filter_if_any(birth_year > 10, .by = gender)
  )
})


test_that("filter_if_any() errors correctly when using `by` instead of `.by`", {
  skip("Not ready")
  expect_error(
    dplyr::starwars %>%
      filter_if_any(birth_year > 10, .by = gender),
    regexp = "by"
  )
})

test_that("`filter_if_any()` doesn't work with `across()`", {
  # TODO improve this error
  expect_snapshot(
    error = TRUE,
    filter_if_any(
      dplyr::starwars,
      dplyr::across(
        ends_with("color"),
        function(x) stringr::str_detect(x, "brown")
      )
    )
  )
})


test_that("adds rows in front, but warns the user", {
  sw <- dplyr::starwars
  expect_snapshot(filter_if_any(sw, is.na(hair_color), hair_color == "brown"))
})

test_that("summarise_with_total() works", {
  expect_snapshot({
    # group_by
    mtcars %>%
      dplyr::group_by(vs = as.character(vs)) %>%
      summarise_with_total(
        x = sum(mpg),
        .label = "All vs",
        .first = TRUE
      )
    # .by
    mtcars %>%
      tibble::as_tibble() %>%
      summarise_with_total(
        x = sum(mpg),
        .by = vs,
        .label = "All vs",
        .first = TRUE
      )
    mtcars %>%
      tibble::as_tibble() %>%
      dplyr::mutate(vs = as.character(vs)) %>%
      summarise_with_total(
        x = sum(mpg),
        y = mean(mpg),
        .by = vs,
        .label = "All vs",
        .first = F
      )
  })
})

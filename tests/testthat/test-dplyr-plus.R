test_that("count_pct works as expected.", {
  res <- count_pct(dplyr::group_by(mtcars, vs), cyl)
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("vs", "cyl", "n", "percent"))
  expect_type(res$percent, "double")

  skip_if_not_installed("scales")
  res2 <- count_pct(dplyr::group_by(mtcars, vs), cyl, label = TRUE)
  expect_type(res2$percent, "character")
})

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
        .first = FALSE
      )
  })
})

test_that("summarise_with_total() keeps factors", {
  fac <- mtcars %>%
    dplyr::mutate(vs = factor(vs), mpg, .keep = "none")
  res <- summarise_with_total(fac, m = mean(mpg), .by = vs) %>% tibble::as_tibble()
  expect_s3_class(res$vs, "factor")
  expect_equal(levels(res$vs), c("Total", "0", "1"))
})

test_that("slice_min_max() works", {
  expect_snapshot({
    slice_min_max(mtcars, mpg, n = 3)
    slice_min_max(mtcars, mpg, n = 3, ascending = FALSE)
  })
  expect_equal(
    nrow(slice_min_max(mtcars, mpg, with_ties = FALSE, n = 2, each = FALSE)),
    2
  )
})

test_that("na_if2() works with expr and values", {
  vec <- c(0, 1, 1, 2)
  vec2 <- c("Here", "not", NA, "Here")
  # NA all 2s
  # You can actually use dplyr::na_if() in this case
  expect_snapshot(error = TRUE, {
    # No entry
    na_if2(vec)
    # Both entries
    #
    na_if2(vec, expr = c(0, 2))
  })


  expect_equal(na_if2(vec, expr = TRUE), rep(NA_real_, 4))
  expect_equal(
    na_if2(vec, 2),
    dplyr::na_if(vec, 2)
  )
  # NA all 1 and 2
  expect_equal(na_if2(vec, c(1, 2)), c(0, rep(NA_real_, 3)))
  expect_equal(na_if2(vec, expr = vec2 == "Here"), c(NA_real_, 1, 1, NA))
})

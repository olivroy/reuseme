test_that("case_if_any basic work", {
  expect_snapshot({
    case_if_any(
      mtcars$vs == 1 ~ "Woww",
      mtcars$mpg > 15 ~ "QW",
      mtcars$qsec > 18 ~ "ooh lalal",
      .default = " "
    )
  })
})

test_that("wrong cases error", {
  expect_snapshot(error = TRUE, {
    case_if_any(mtcars$vs == 1 ~ "Woww", mtcars$mpg > 15 ~ "QW", mtcars$qsec > 18 ~ "ooh lalal", .sep = " ")
    case_if_any(mtcars$vs == 1 ~ "Woww", mtcars$mpg > 15 ~ "QW", .sep = "")
  })
})

test_that("You can select a variable that was just created (#8)", {
  skip("Not ready")
  expect_snapshot({
    mtcars %>%
      group_by(vs) %>%
      summarise(avg_mpg = mean(mpg), error = case_if_any(avg_mpg > 20 ~ "Youppi"))
  })
})

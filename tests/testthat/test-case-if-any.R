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

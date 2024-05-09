test_that("href_name_url() works", {
  expect_snapshot(
    href_name_url(c(
      "x" = "https://google.com",
      "y" = "https://github.com"
    ))
  )
})

test_that("quarto_help() works", {
  expect_snapshot(quarto_help())
})

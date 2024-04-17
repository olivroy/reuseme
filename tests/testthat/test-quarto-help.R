test_that("link_href works", {
  expect_snapshot(
    href_name_url(c("x" = "https://google.com", "y" = "https://github.com"))
  )
  expect_snapshot(
    quarto_help()
  )
})

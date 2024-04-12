test_that("browse_pkg() works", {
  skip_on_cran()
  skip_if_offline()
  expect_snapshot({
    browse_pkg("cli", open = FALSE)
    browse_pkg("reuseme", news_only = TRUE)
    browse_pkg("reuseme", ref_only = TRUE)
  })
  skip_if_not_installed("Matrix")
  expect_snapshot(browse_pkg("Matrix"))
})


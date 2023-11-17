test_that("browse_pkg() works", {
  expect_snapshot({
    browse_pkg("cli", open = FALSE)
    browse_pkg("reuseme", news_only = TRUE)
    browse_pkg("reuseme", ref_only = TRUE)
  })
})

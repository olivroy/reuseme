test_that("link_issue() works", {
  expect_snapshot({
    lines <- c("Go to rstudio/rstudio#120.")
    cat(link_issue(lines))
  })
})

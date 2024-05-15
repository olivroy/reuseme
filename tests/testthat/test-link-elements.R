test_that("link_gh_issue() + markup_href() work", {
  expect_snapshot({
    matches_regex("[franchise](https://en.wikipedia.org/wiki/Fallout_(franchise))", "md_url")
    matches_regex("([franchise](https://google.com))", "md_url")
}
  )

  expect_snapshot({
    lines <- c(
      "Go to rstudio/rstudio#120 and [gh](https://github.com) and {.href [gh](https://github.com)}.",
      "For faster printing of message (r-lib/cli#607), use withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))",
      "[franchise](https://en.wikipedia.org/wiki/Fallout_(franchise))"
    )
    lines_with_md_link <- link_gh_issue(lines)
    cat(markup_href(lines_with_md_link), sep = "\n")
  })
})

# link_gh_issue() + markup_href() work

    Code
      matches_regex("[franchise](https://en.wikipedia.org/wiki/Fallout_(franchise))",
        "md_url")
    Output
      [1] "[franchise](https://en.wikipedia.org/wiki/Fallout_(franchise))"
    Code
      matches_regex("([franchise](https://google.com))", "md_url")
    Output
      [1] "[franchise](https://google.com))"

---

    Code
      lines <- c(
        "Go to rstudio/rstudio#120 and [gh](https://github.com) and {.href [gh](https://github.com)}.",
        "For faster printing of message (r-lib/cli#607), use withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))",
        "[franchise](https://en.wikipedia.org/wiki/Fallout_(franchise))")
      lines_with_md_link <- link_gh_issue(lines)
      cat(markup_href(lines_with_md_link), sep = "\n")
    Output
      Go to {.href [rstudio/rstudio#120](https://github.com/rstudio/rstudio/issues/120)} and {.href [gh](https://github.com)} and {.href [gh](https://github.com)}.
      For faster printing of message ({.href [r-lib/cli#607](https://github.com/r-lib/cli/issues/607))}, use withr::local_options(list(cli.num_colors = cli::num_ansi_colors()))
      {.href [franchise](https://en.wikipedia.org/wiki/Fallout_(franchise))}


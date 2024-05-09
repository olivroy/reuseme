# link_gh_issue() + markup_href() work

    Code
      lines <- c(
        "Go to rstudio/rstudio#120 and [gh](https://github.com) and {.href [gh](https://github.com)}.")
      lines_with_md_link <- link_gh_issue(lines)
      cat(markup_href(lines_with_md_link))
    Output
      Go to {.href [rstudio/rstudio#120](https://github.com/rstudio/rstudio/issues/120) and [gh](https://github.com)} and {.href [gh](https://github.com)}.


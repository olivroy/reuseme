#' Create a markdown link to a GitHub issue
#'
#' In RStudio, links to issues are automatically recognized.
#' This function creates intermediate markdown links to entries of the form rstudio/rstudio#1100
#'
#' Note: doesn't (yet) support without <OWNER>/<REPO>
#'
#' Basically trransform repo/org#xx -> [repo/org#xx](https://github.com/repo/org/issues/xx).
#'
#' Afterwards, we use [markup_href()] to create a cli link
#' @param x A string, usually lines of files that contains issue numbers.
#'
#' @return A markdown link linked issue to GitHub issue
#' @export
#' @keywords internal
#' @family inline markup internal helpers
#' @examples
#' link_gh_issue(c("We really need rstudio/gt#1469 to be fixed."))
link_gh_issue <- function(x) {
  # Return early if no issue pattern is detected.
  regex_gh_issue <- "([:graph:]+/[^#\\s]+)#(\\d+)"

  has_gh_issue <- stringr::str_detect(
    x,
    regex_gh_issue
  )
  if (!any(has_gh_issue)) {
    return(x)
  }
  # what we need to do is to t

  x_to_change <- x[has_gh_issue]
  # x_changed <-
  x_changed <- x_to_change |> stringr::str_replace_all(
    regex_gh_issue,
    paste0("[\\1#\\2](https://github.com/\\1/issues/\\2)")
  )

  x[has_gh_issue] <- x_changed
  x
}

#' Create a cli href with a markdown link
#'
#' Transforms [text](url) -> {.href [text](url)}
#' @family inline markup internal helpers
#' @param x A character vector
#' @returns A character vector with substrings changed
#' @keywords internal
#' @export
#' @examples
#' markup_href(c("[link](https://google.com)", "{.href [link](https://google.com)}"))
#'
markup_href <- function(x) {
  # already excluding markuped strings
  # only safe links for now
  regex_md_url <- "(?<!\\{\\.href\\s)(\\[.+\\])(\\(https.+\\))(?!\\})"

  has_md_url <- stringr::str_detect(
    x,
    regex_md_url
  )
  if (!any(has_md_url)) {
    return(x)
  }
  x_to_change <- x[has_md_url]
  x_changed <- x_to_change |> stringr::str_replace_all(
    regex_md_url,
    paste0("{.href \\1\\2}")
  )

  x[has_md_url] <- x_changed
  x
}

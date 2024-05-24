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
#' @param home_repo Optional, but if supplied, will be stripped.
#' @return A markdown link linked issue to GitHub issue
#' @export
#' @keywords internal
#' @family inline markup internal helpers
#' @examples
#' link_gh_issue(c("We really need rstudio/gt#1469 to be fixed."))
link_gh_issue <- function(x, home_repo = NULL) {
  # Return early if no issue pattern is detected.
  regex_gh_issue <- common_regex("gh_issue")

  has_gh_issue <- grepl(
    regex_gh_issue,
    x,
    perl = TRUE
  )
  if (!any(has_gh_issue)) {
    return(x)
  }
  # what we need to do is to t

  x_to_change <- x[has_gh_issue]
  # x_changed <-
  x_changed <- stringr::str_replace_all(
    x_to_change,
    regex_gh_issue,
    paste0("[\\1#\\2](https://github.com/\\1/issues/\\2)")
  )
  if (!is.null(home_repo))
  x_changed <- gsub(
    paste0(home_repo,"#"),
    "#",
    x_changed
  )

  x[has_gh_issue] <- x_changed
  x
}
# transforms (#xx) to (org/repo#xx)
link_local_gh_issue <- function(x, repo_home) {
  gsub(
    # max 99999 issues.
    pattern =  "\\((#\\d{1,5})\\)",
    paste0("(", repo_home, "\\1)"),
    x
  )
}
find_pkg_org_repo <- function(dir_common = NULL, file = NULL) {
  rlang::local_interactive(FALSE)
  withr::local_options("usethis.quiet" = TRUE)
  if (!is.null(dir_common)) {
    pkg_path <- tryCatch(
      rprojroot::find_package_root_file(path = dir_common),
      error = function(e) {
        # cli::cli_inform("Could not detect path.")
        NULL
      })
    if (is.null(pkg_path)) {
      return(NULL)
    }
    gh_url <- withCallingHandlers(
      usethis::browse_github(basename(pkg_path)),
      error = function(e) {
        cli::cli_abort("didn't find a way to do what is required.")
      }
    )
    org_repo_found <- stringr::str_remove(gh_url, ".+github.com/|.+gitlab.com/")
    return(org_repo_found)
  }

  if (!is.null(file)) {
    pkg_path <- withCallingHandlers(
      rprojroot::find_package_root_file(path = file),
      error = function(e) {
        # cli::cli_inform("Could not detect path.")
        NULL
      }
    )

    gh_url <- usethis::browse_github(basename(pkg_path))
    org_repo_found <- stringr::str_remove(gh_url, ".+github.com/|.+gitlab.com/")
  } else {
    org_repo_found <- NULL
  }
  if (is.null(org_repo) && is.null(org_repo_found)) {
    cli::cli_abort("No way to discover URL.")
  }

}
#' Create a cli href with a markdown link
#'
#' Transforms `[text](url)` -> `{.href [text](url)}`
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
  regex_md_url <- common_regex("md_url")

  has_md_url <- grepl(
    regex_md_url,
    x,
    perl = TRUE
  )
  if (!any(has_md_url)) {
    return(x)
  }
  x_to_change <- x[has_md_url]

  # for debugging (delete when regex_md_url seems okay)
  # print(stringr::str_extract(
  #   x_to_change,
  #   regex_md_url,
  #   group = 1
  # ))
  x_changed <- stringr::str_replace_all(
    x_to_change,
    regex_md_url,
    paste0("{.href \\1\\2}")
  )
  resolve_parens <- grepl("({.href [", x_changed, fixed = TRUE) &
    grepl("))}", x_changed, fixed = TRUE)

  if (any(resolve_parens)) {
    # resolve markdown links parsing in this case ([md](url)) to make sure parens
    # are correct
    x_changed[resolve_parens] <- stringr::str_replace_all(
      x_changed[resolve_parens],
      c(
        "(\\.href[^\\}\\{]+)\\)\\)\\}" = "\\1)})"
      )
    )
  }

  x[has_md_url] <- x_changed
  x
}
common_regex <- function(which) {
  x <- c(
    # usage of double negation will make it work
    md_url = "(?<!\\{\\.href\\s)(\\[[^\\[\\]]+\\])(\\(https[^,\\s]+\\)(?![^\\s,\\:;\\.$\\)]))",
    gh_issue = "([[:alpha:]][[:graph:]]+/[^#\\s]+)#(\\d+)"
  )
  unname(x[which])
}

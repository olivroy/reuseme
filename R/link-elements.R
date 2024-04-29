#' Create a cli link to a GitHub issue
#'
#' In RStudio, links to issues are automatically recognized.
#' This function creates cli ansi links to entries of the form rstudio/rstudio#1100
#'
#' Note: doesn't support without <OWNER>/<REPO>
#' @param x A string, usually lines of files that contains issue numbers.
#'
#' @return A formatted linked issue to GitHub issue
#' @export
#' @keywords internal
#' @family inline markup internal helpers
#' @examples
#' link_issue(c("We really need rstudio/gt#1469 to be fixed.")) |> cli::cli_bullets()
link_issue <- function(x) {
  # Return early if no issue pattern is detected.
  if (!any(stringr::str_detect(x, "[:graph:]+/[^#\\s]+#\\d"))) {
    return(x)
  }

  li <- stringr::str_split(x, pattern = "\\s")

  issue_regex <- "([:graph:]+/[^#\\s]+)#(\\d+)"

  res <- purrr::map(li, function(x) {
    purrr::map_chr(x, function(y) {
      if (stringr::str_detect(y, issue_regex)) {
        rest <- stringr::str_extract(y, "([:graph:]+/[^#\\s]+)#(\\d+)(.*)", 3)
        if (!is.na(rest)) {
          y <- stringr::str_remove(y, paste0(rest, "$"))
        }
        rep <- stringr::str_replace_all(y, "([:graph:]+/[^#\\s]+)#(\\d+)", "https://github.com/\\1/issues/\\2")

        y <- paste0("{.href [", y, "](", rep, ")}", rest)
      }
      y
    })
  })
  purrr::map_chr(res, \(x) paste(x, collapse = " "))
}

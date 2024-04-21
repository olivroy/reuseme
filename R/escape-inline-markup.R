# If error around here, check if maybe these functions can help
#' escape inline markup in case of problems
#'
#' @param x A character vector
#'
#' @return A character vector with inline markup scrubbed.
#' @export
#' @keywords internal
#' @family inline markup internal helpers
#' @examples
#' escape_markup("{gt}")
#' escape_markup("{.file {here}}")
#' escape_markup("multi problems {{gt}} to {gt} to {.file gt} to {.file {gt}}")
escape_markup <- function(x) {
  is_left_bracket <- stringr::str_detect(x, "\\{")
  is_right_bracket <- stringr::str_detect(x, "\\}")
  is_bracket <- is_left_bracket & is_right_bracket

  if (!any(is_bracket)) {
    x[is_left_bracket & !is_bracket] <- stringr::str_replace_all(x[is_left_bracket & !is_bracket], "\\{", "{{")
    x[is_right_bracket & !is_bracket] <- stringr::str_replace_all(x[is_right_bracket & !is_bracket], "\\}", "}}")
    return(x)
  }

  is_markup_referring_to_local_variable <-
    is_bracket & stringr::str_detect(x, "\\{\\.[:alpha:]+\\s\\{")
  is_markup_okay <- is_bracket & stringr::str_detect(x, "\\{\\.[:alpha:]+[^\\{]") & !is_markup_referring_to_local_variable

  if (all(is_markup_okay) && !any(is_markup_referring_to_local_variable)) {
    x[is_left_bracket & !is_bracket] <- stringr::str_replace_all(x[is_left_bracket & !is_bracket], "\\{", "{{")
    x[is_right_bracket & !is_bracket] <- stringr::str_replace_all(x[is_right_bracket & !is_bracket], "\\}", "}}")
    return(x)
  }

  x <- stringr::str_replace_all(
    x,
    #       =   {.file {}}
    pattern = "(\\{\\.[:alpha:]+\\s\\{)([:alpha:]+)\\}\\}",
    replacement = "\\1.url \\2}}"
  )
  # possibly this could be word characters?
  x <- stringr::str_replace_all(
    x,
    # don't use anything [^a], instead use not preceding
    pattern = "((?<!\\{)\\{)([:alpha:]+)\\}",
    replacement = "\\1{\\2}}"
  )
  x

  x[is_left_bracket & !is_bracket] <- stringr::str_replace_all(x[is_left_bracket & !is_bracket], "\\{", "{{")
  x[is_right_bracket & !is_bracket] <- stringr::str_replace_all(x[is_right_bracket & !is_bracket], "\\}", "}}")

  if (any(stringr::str_detect(x, "\\{{3,}"))) {
    # more than 3 {
    cli::cli_abort("internal errror. Did not transform string correctly.")
  }
  x

}

#' Is inline markup valid?
#'
#'
#' @return A logical vector
#' @export
#' @keywords internal
#' @examples
#' is_markup_okay("{{gt}}")
#' is_markup_okay("{gt}")
#' is_markup_okay("{.file {gt}}")
is_markup_okay <- function(x) {
  # no match of single { or }
  stringr::str_detect(x, pattern = "(?<!\\{)\\{[:alpha:]+\\}(?!\\{)", negate = TRUE)
}

# from cli
cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

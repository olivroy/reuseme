# If error around here, check if maybe these functions can help
#' escape inline markup in case of problems
#'
#' @param x A character vector
#'
#' @return A character vector with inline markup scrubbed.
#' @noRd
#' @keywords internal
#' @family inline markup internal helpers
#' @examples
#' escape_markup("{gt}")
#' escape_markup("{.file {here}}")
#' escape_markup("multi problems {{gt}} to {gt} to {.file gt} to {.file {gt}}")
#' escape_markup("{x^2}")
escape_markup <- function(x) {
  is_left_bracket <- grepl("{", x, fixed = TRUE)
  is_right_bracket <- grepl("}", x, fixed = TRUE)
  is_bracket <- is_left_bracket & is_right_bracket

  if (!any(is_bracket)) {
    x[is_left_bracket & !is_bracket] <- stringr::str_replace_all(x[is_left_bracket & !is_bracket], "\\{", "{{")
    x[is_right_bracket & !is_bracket] <- stringr::str_replace_all(x[is_right_bracket & !is_bracket], "\\}", "}}")
    return(x)
  }

  local_var_ref_in_markup <-
    is_bracket & stringr::str_detect(x, "\\{\\.[:alpha:]+\\s\\{|\\(\\{.+\\}\\)")
  is_markup_okay <- is_bracket & stringr::str_detect(x, "\\{\\.[:alpha:]+[^\\{]") & !local_var_ref_in_markup & !is_markup_incorrect(x)

  if (all(is_markup_okay) && !any(local_var_ref_in_markup)) {
    x[is_left_bracket & !is_bracket] <- stringr::str_replace_all(x[is_left_bracket & !is_bracket], "\\{", "{{")
    x[is_right_bracket & !is_bracket] <- stringr::str_replace_all(x[is_right_bracket & !is_bracket], "\\}", "}}")
    return(x)
  }
  # replace fn arg {fn}(arg) -> fn({arg})
  valid_r_variable_regex <- "[:alpha:][[:alpha:]\\_\\d]+"
  x <- stringr::str_replace_all(
    x,
    paste0("\\{(", valid_r_variable_regex, ")\\}\\("),
    "\\1\\("
  )
  x <- stringr::str_replace_all(
    x,
    #       =   {.file {}}
    pattern = paste0("(\\{\\.[:alpha:]+\\s\\{)(", valid_r_variable_regex, ")\\}\\}"),
    replacement = "\\1.url \\2}}"
  )
  x <- stringr::str_replace_all(
    x,
    pattern = "\\{\\.href\\s\\[(.+)\\]\\(\\{.+\\}\\)\\}",
    "<\\1>"
  )
  # possibly this could be word characters?
  # x <- stringr::str_replace_all(
  #   x,
  #   # don't use anything [^a], instead use not preceding
  #   pattern = "((?<!\\{)\\{)([:alpha:]+)\\}",
  #   replacement = "\\1{\\2}}"
  # )
  # replace variables



  x <- replace_r_var(x)

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
#' @noRd
#' @return A logical vector
#' @keywords internal
#' @examples
#' is_markup_incorrect("{{gt}}")
#' is_markup_incorrect("{gt}")
#' is_markup_incorrect("{.file {gt}}")
is_markup_incorrect <- function(x) {
  # no match of single { or }
  valid_r_variable_regex <- "[:alpha:][[:alpha:]\\_\\d]+"

  stringr::str_detect(x, pattern = paste0("(?<!\\{)\\{", valid_r_variable_regex, "\\}(?!\\})")) |
    stringr::str_detect(x, pattern = "\\]\\(\\{.+\\}\\)\\}") |
    stringr::str_detect(x, paste0("\\{\\.[:alpha:]+\\s\\{", valid_r_variable_regex, "\\}"))
}

is_r_variable <- function(x) {
  identical(make.names(x, unique = FALSE), x)
}

# from cli
cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

#' @noRd
#' @examples
#' # example code
#' replace_r_var("{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}} and {my1e}.")
replace_r_var <- function(x) {
  valid_r_variable_regex <- "[:alpha:][[:alpha:]\\_\\d]+"
  regexp <- paste0("(?<!\\{)\\{(", valid_r_variable_regex, ")\\}(?!\\})")
  stringr::str_replace_all(
    x, regexp, "\\{\\{\\1\\}\\}"
  )
}

#' @noRd
#'
#' @examples
#' replace_r_var("i{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
#' # last instance taken care of with escape_markup with a different strategy
#' #> "{{gt_var}} in {{gt_var}} in gt_var in {.file {gt_var}}."
#' escape_markup("{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
#' #> "{{gt_var}} in {{gt_var}} in gt_var in {.file gt_var}."

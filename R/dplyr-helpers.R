# Internal dplyr functions copied over
check_by_typo <- function(..., by = NULL, error_call = caller_env()) {
  check_by_typo_impl(wrong = "by", right = ".by", by = {{ by }}, error_call = error_call)
}
check_by_typo_impl <- function(wrong, right, by = NULL, error_call = caller_env()) {
  by <- enquo(by)
  if (quo_is_null(by)) {
    return(invisible())
  }
  message <- c("Can't specify an argument named {.code {wrong}} in this verb.",
    i = "Did you mean to use {.code {right}} instead?"
  )
  cli::cli_abort(message, call = error_call)
}

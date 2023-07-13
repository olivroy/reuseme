#' Helpers that can return a named vector
#'
#' Base R keeps names in various places, but drops them elsewhere
#' These functions are some that I use frequently, like `max`, or `unique`
#' @param x A vector
#' @param na.rm Should remove `NA`
#' @returns A vector (preserves names)
#' @examples
#' max_named(c("this guy" = 2, "that guy" = 3))
#'
#' unique_named(c("this guy" = 2, "that guy" = 3, "this guy" = 2))
#' # returns the same as base R for unnamed input
#' unique_named(c(1, 2, 3, 3))
#' @name named-base
NULL
#' @rdname named-base
#' @export
max_named <- function(x, na.rm = FALSE) {
  max_res <- max(x, na.rm = na.rm)
  if (rlang::is_named(x)) {
    names(max_res) <- names(x[which.max(x)])
  }
  max_res
}
#' @rdname named-base
#' @export
min_named <- function(x, na.rm = FALSE) {
  min_res <- min(x, na.rm = na.rm)
  if (rlang::is_named(x)) {
    names(min_res) <- names(x[which.min(x)])
  }
  min_res
}
#' @rdname named-base
#' @export
unique_named <- function(x) {
  # Duplicate names
  # purrr::map vs list_c, SO question (this is in canadr, when querying possible geographies.)
  if (!rlang::is_named(x)) {
    return(unique(x))
  }

  unique_x <- unique(x)
  unique_names <- unique(names(x))

  if (length(unique_x) != length(unique_names)) {
    cli::cli_abort(c("length of unique names is not the same as length x"))
  }
  names(unique_x) <- unique_names
  unique_x
}

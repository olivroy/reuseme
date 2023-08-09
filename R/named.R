#' Helpers that can return a named vector
#'
#' Base R keeps names in various places, but drops them elsewhere
#' These functions are some that I use frequently, like `max`, or `unique`
#' @param x A vector
#' @param na.rm Should remove `NA`?
#' @param all_matches If `FALSE` (default), will only return the first match,
#'   similar to [which.min()] / [which.max()]
#'   If `TRUE`, will return a named vector of all values
#'   corresponding to the max value.
#' @returns
#'   * `min/max_named(all_matches = FALSE)`, a named vector of length 1.
#'   * Otherwise, a named vector.
#' @examples
#' max_named(c("this guy" = 2, "that guy" = 3))
#'
#' unique_named(c("this guy" = 2, "that guy" = 3, "this guy" = 2))
#' # returns the same as base R for unnamed input
#' unique_named(c(1, 2, 3, 3))
#' # returns all values
#' min_named(c("x" = 1, "y" = 1, "ww" = 2), all_matches = FALSE)
#'
#' # TODO is usable with `extract_cell_value()`
#'
#' @name named-base
NULL
#' @rdname named-base
#' @export
min_named <- function(x, na.rm = FALSE, all_matches = FALSE) {
  min_res <- min(x, na.rm = na.rm)

  if (rlang::is_named(x)) {
    names(min_res) <- names(x[which.min(x)])
  }

  if (all_matches) {
    # wouldn't make sense if there were duplicated names that return a different value
    # min_named(c("x" = 1, "y" = 1, "ww" = 2, "y" = 2))
    cli::cli_abort("Still not done.")
  }

  min_res
}
#' @rdname named-base
#' @export
max_named <- function(x, na.rm = FALSE, all_matches = FALSE) {
  max_res <- max(x, na.rm = na.rm)

  if (all_matches) {
    cli::cli_abort("Still not done.")
  }

  if (rlang::is_named(x)) {
    names(max_res) <- names(x[which.max(x)])
  }

  max_res
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

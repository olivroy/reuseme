#' test
#'
#' @param x  a value
#' @param order another
#'
#' @return a return
#' @export
#' @keywords internal
#' @examples
#' f_test(x = "1", y = "c", z = "z")
f_test <- function(x, y = c("a", "b", "c"), ...) {
  z <- rlang::arg_match(y)
  h <- 2
  UseMethod("f_test")
}

#' @export
f_test.character <- function(x, y = c("a", "b", "c"), ..., z) {
  paste0(x, y, z, h)
}

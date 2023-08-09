#' Open a Document in RStudio
#'
#' Wrapper around [rstudioapi::documentOpen()], but with `fs paths`, for consistency.
#'
#' @inheritParams rstudioapi::documentOpen
#' @param move_cursor Boolean; move the cursor to the requested location after
#'   opening the document?
#' @return Invisibly returns the document id
#' @export
#' @examples
#' if (FALSE) {
#'   # open the fictious file.R at line 5
#'   open_rs_doc("file.R", line = 5)
#' }
#'
open_rs_doc <- function(path, line = -1L, col = -1L, move_cursor = TRUE) {
  path <- fs::path_real(path)
  invisible(rstudioapi::documentOpen(path = path, line = line, col = col, moveCursor = move_cursor))
}

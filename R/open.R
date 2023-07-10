#' Open a Document in RStudio
#'
#' Wrapper around [rstudioapi::documentOpen]
#' @inheritParams rstudioapi::documentOpen
#' @param move_cursor Boolean; move the cursor to the requested location after
#'   opening the document?
#' @return Invisibly returns the document id
#' @export
open_rs_doc <- function(path, line = -1L, col = -1L, move_cursor = TRUE) {
  invisible(rstudioapi::documentOpen(path = path, line = line, col = col, moveCursor = move_cursor))
}

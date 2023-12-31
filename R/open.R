#' Open a Document in RStudio
#'
#' Wrapper around [rstudioapi::documentOpen()], but with `fs paths`, for consistency.
#' If the file could not be opened, a clickable hyperlink is displayed.
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
  check_number_whole(line)
  check_number_whole(col)

  if (col != -1L && line == -1L) {
    cli::cli_abort("Internal error, you can't specify col only.")
  }

  doc_id <- rstudioapi::documentOpen(path = path, line = line, col = col, moveCursor = move_cursor)
  if (is.null(doc_id)) {
    file_pos_string <- path
    if (line != -1L) pos_string <- paste0(pos_string, ":", line)
    if (col != -1L) pos_string <- paste0(pos_string, ":", col)
    cli::cli_bullets()
  }
  invisible(doc_id)
}

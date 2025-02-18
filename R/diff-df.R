#' Get a widget that displays diff between data frame
#' 
#' Thin wrapper around [diffviewer::visual_diff()].
#' 
#' Also writes to tempfiles to make sure it works
#' 
#' @param old,new Pair of data frames to compare
#' 
#' @inheritParams diffviewer::visual_diff
#' @returns A HTML widget
#' @export 
#' @examples
#' 
#' mtcars2 <- mtcars
#' 
#' mtcars2[1, 1] <- NA
#' visual_diff_df(old = mtcars, new = mtcars2)
visual_diff_df <- function(old, new, width = NULL, height = NULL) {
  check_data_frame(old)
  check_data_frame(new)
  rlang::check_installed("diffviewer")

  tmp_old <- withr::local_tempfile(pattern = "old", fileext = ".csv")

  tmp_new <- withr::local_tempfile(pattern = "new", fileext = ".csv")

  utils::write.csv(old, tmp_old, row.names = FALSE)
  utils::write.csv(new, tmp_new, row.names = FALSE)

  diffviewer::visual_diff(
    tmp_old,
    tmp_new,
    width = width,
    height = height
  )

}

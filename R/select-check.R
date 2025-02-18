#' Give a more informative error in case of tidyselect errors
#' 
#' 
#' @inheritParams dplyr::select
#'
#' 
#' 
#' @export
#' @examples
#' try(mtcars |> select_check(vs3))
select_check <- function(.data, ...) {
  check_data_frame(.data)
  withCallingHandlers(
    dplyr::select(.data, ...),
    error = function(e) {

      cli::cli_abort(c(
        "Available columns are {.code {names(.data)}}"
      ))
    }
  )
}

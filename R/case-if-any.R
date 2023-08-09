#' case-when, but checks for all matches, returns a character
#'
#' Each case is evaluated for **all** cases and a character vector match
#' for each element determines the corresponding value in the output vector.
#' If no cases match, the `.default` is used.
#' The function allows you to assign multiple values to a character value, which
#' can be very handy for EDA.
#' @inheritParams dplyr::case_when
#' @param .sep, the separator between answers. (default is `;`), can't be a substring of any of the text
#' @inherit dplyr::case_when return
#' @param .drop_empty drop if no match is returned.
#'   (Defaults to `TRUE` for legibility), but if `FALSE`,
#'    can be used more easily with `tidyr::separate_wider/longer_delim()`
#' @export
#' @examples
#'
#' case_if_any(
#'   mtcars$vs == 1 ~ "vs = 1",
#'   mtcars$mpg > 150 ~ "I have mpg > 150"
#' )
#' case_if_any(
#'   mtcars$vs == 1 ~ "Woww",
#'   mtcars$mpg > 15 ~ "QW",
#'   mtcars$qsec > 18 ~ "ooh lalal",
#'   .sep = ";",
#'   .default = NA
#' )
case_if_any <- function(..., .default = "", .sep = ";", .drop_empty = TRUE) {
  check_string(.sep, allow_empty = FALSE)
  check_string(.default, allow_na = TRUE)
  expr <- rlang::list2(...)
  n_expr <- rlang::dots_n(...)

  expr_list <- purrr::map(.x = expr, .f = as.character)
  expr_list <- purrr::map(expr_list, function(x) x[-1])
  names(expr_list) <- paste0("cond", seq_len(n_expr))
  condition <- purrr::map(expr_list, 1)
  text_condition <- purrr::map(expr_list, 2)

  result_out1 <- eval(parse(text = condition[1]))
  length_out <- length(result_out1)
  res <- list()

  for (i in seq_len(n_expr)) {
    res[[i]] <- eval(parse(text = condition[i]))
  }

  names(res) <- names(expr_list)

  result_raw1 <-
    dplyr::mutate(
      .data = dplyr::bind_cols(res),
      dplyr::across(dplyr::everything(), function(x) ifelse(x, text_condition[dplyr::cur_column()], .default))
    )

  result_raw2 <-
    dplyr::mutate(
      .data = dplyr::rowwise(result_raw1),
      text = paste0(dplyr::c_across(dplyr::everything()), collapse = .sep)
    )

  result <- result_raw2$text

  if (any(grepl(.sep, text_condition))) {
    cli::cli_abort("`.sep` cannot be contained in the `condition`. Change the replacement text, or `sep`")
  }

  if (.drop_empty) {
    if (nzchar(.default)) {
      result <- gsub(pattern = paste0("(", .default, ")", .sep, "(", .default, ")?"), replacement = "", x = result)
      result <- gsub(pattern = paste0(.sep, .default, "$"), replacement = "", x = result)
      result <- gsub(pattern = paste0("^(", .default, ")?", .sep, "(", .default, ")?$"), replacement = .default, x = result)
    }

    result <- gsub(pattern = paste0("^", .sep, "|", .sep, "$"), replacement = "", x = result)
    result <- dplyr::na_if(result, "")
  } else {
    cli::cli_abort("Not supported yet.")
  }

  dplyr::coalesce(result, .default)
}

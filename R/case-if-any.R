#' case-when, but checks for all matches, returns a character
#'
#' Each case is evaluated for **all** cases and a character vector match
#' for each element determines the corresponding value in the output vector.
#' If no cases match, the `.default` is used.
#' The function allows you to assign multiple values to a character value, which
#' can be very handy for EDA.
#' @inheritParams dplyr::case_when
#' @param .sep, the separator between answers. (default is `;`), can't be a
#'   substring of any of the text
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
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = function(x) {
          ifelse(x, text_condition[dplyr::cur_column()], .default)
        }
      )
    )

  result_raw2 <-
    dplyr::mutate(
      .data = dplyr::rowwise(result_raw1),
      text = paste0(dplyr::c_across(dplyr::everything()), collapse = .sep)
    )

  result <- result_raw2$text

  if (any(grepl(.sep, text_condition))) {
    cli::cli_abort(c(
      x = "{.arg .sep} cannot be contained in the condition.",
      i = "Change either the replacement text, or {.arg .sep}"
    ))
  }

  # I ended up importing stringr...
  if (.drop_empty) {
    if (nzchar(.default)) {
      result <- gsub(pattern = paste0("(", .default, ")", .sep, "(", .default, ")?"), replacement = "", x = result)
      result <- gsub(pattern = paste0(.sep, .default, "$"), replacement = "", x = result)
      result <- gsub(pattern = paste0("^(", .default, ")?", .sep, "(", .default, ")?$"), replacement = .default, x = result)
    }

    result <- gsub(pattern = paste0("^", .sep, "+|", .sep, "+$"), replacement = "", x = result)
    result <- dplyr::na_if(result, "")
  } else {
    cli::cli_abort("Not supported yet.")
  }

  dplyr::coalesce(result, .default)
}


#' Categorize a vector of free text
#'
#' Related to [case_if_any()] but simpler!
#'
#' It works with the same syntax as [forcats::fct_collapse()]
#' @param variable a character vector
#' @param ... dynamic dots, list of recoding
#' @param sep Separator between the results
#' @param ignore_case should match be case-sensitive or not.
#'
#' @returns A named character vector
#' @export
#'
#' @examples
#' categorize(
#' c("banana apple", "apple orange pear", "apple", "cucumber"),
#'  "yellow fruits" = c("banana", "pear"),
#'  "bad fruits" = c("orange")
#' )
categorize <- function(variable, ..., sep = ";", ignore_case = TRUE) {
  check_character(variable)
  categories <- rlang::list2(...)

  category_names <- names(categories)
  # will be a list
  match <- list()
  for (i in seq_along(categories)) {
    category <- category_names[i]
    regex <- paste0(categories[[i]], collapse = "|")

    match[[i]] <- grepl(regex, x = variable, ignore.case = ignore_case)
  }
  names(match) <- category_names
  create_label_column(match, sep = sep)
}

#' @noRd
#' @examples
#' create_label_column(list(
#' "yellow fruits" = c(T, T, F, F),
#' "bad fruits" = c(F, T, T, F)
#' )
#' )
#'
create_label_column <- function(list, sep = ";") {
  cats <- names(list)
  res <- list
  for (i in seq_along(list)) {
    res[[i]] <- ifelse(list[[i]], cats[i], "")
  }
  res

  res2 <- purrr::map_chr(
    purrr::list_transpose(res),
    \(x) paste0(x, collapse = sep)
  )
  # remove extra seps
  res2 <- gsub(
    paste0("^",sep, "+|", sep, "+$"),
    "",
    res2
  )
  # Remove extra seps
  gsub(paste0(sep, "{2,}"), sep, res2)
}

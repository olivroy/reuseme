#' Write temporary Excel to Downloads
#'
#' @param dat data frame
#' @param name name of file
#' @param font font name
#'
#' @returns A new Excel file
#' @export
#'
write_temp_excel <- function(dat, name, font = "Arial") {
  rlang::check_installed("openxlsx2")
  rlang::check_required(name)
  if (is.null(getOption("reuseme.temp_dir"))) {
    cli::cli_abort(c("Set reuseme.temp_dir option in Rprofile to continue."))
  }
  # janmarvin/openxlsx2#1365
  withr::local_options(openxlsx2.percentageFormat = "#,#%", openxlsx2.minWidth = 5, openxlsx2.maxWidth = 12)
  wb <- openxlsx2::wb_workbook()$add_worksheet()

  pct_cols <- stringr::str_which(names(dat), "caf")
  for (i in seq_along(pct_cols)) {
    class(dat[[pct_cols[i]]]) <- c(
      "percentage", class(dat[[pct_cols[i]]])
    )
  }
  renamed_var <- label_column(dat)
  renamed_var <- renamed_var[!is.na(names(renamed_var))]
  dat <- dplyr::rename(dat, dplyr::any_of(renamed_var))
  wb$add_data(x = dat, na.strings = "")
  wb$set_base_font(font_name = "Arial")
  wb$add_cell_style(dims = openxlsx2::wb_dims(x = dat, select = "col_names"), wrap_text = TRUE)
  wb$set_col_widths(widths = "auto", cols = seq_along(names(dat)))

  # wb$add_cell_style(
  #   dims = openxlsx2::wb_dims(x = dat),
  #   vertical = "center"
  # )
  filename <- fs::path_expand(fs::path(getOption("reuseme.temp_dir"), sub("\\.xlsx$", "", name), ext = "xlsx"))
  wb$save(filename)
  openxlsx2::xl_open(filename)
}

# Use the label attribute as label if available
#' attr(mtcars$mpg, "label") <- "Miles per gallon"
#'   mtcars |> rename(any_of(label_column(mtcars)))
#' @returns A named character vector
#' @noRd
label_column <- function(x) {
  # Probably already provided by {.pkg labelled}
  check_data_frame(x)
  res0 <- purrr::imap_chr(
    x,
    \(x, y) attr(x, "label", exact = TRUE) %||% y
  )
  res <- names(res0)
  names(res) <- unname(res0)
  res
}

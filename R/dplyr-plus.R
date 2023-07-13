# dplyr extra ------------------------------------------
#' Count observations by group and compute percentage
#'
#' `count_pct()` lets you quickly count the unique values of one or more
#' variables: `df %>% count_pct(a, b)` It calculates the percentage by group
#' afterwards
#'
#' Wrapper function around [dplyr::count()]
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @inheritParams dplyr::count
#' @inheritParams scales::label_number
#' @param label A logical, If `TRUE`, will return the vector as character
#' @inherit dplyr::count return
#' @family dplyr extensions
#' @export
#' @examples
#' count_pct(mtcars, cyl)
#' library(magrittr)
#' mtcars %>%
#'   dplyr::group_by(vs) %>%
#'   count_pct(cyl)
#'
#' mtcars %>%
#'   dplyr::group_by(vs) %>%
#'   count_pct(cyl, label = TRUE)
#'
#' mtcars %>%
#'   dplyr::group_by(vs) %>%
#'   count_pct(cyl, label = TRUE, accuracy = 0.1)
count_pct <- function(.data, ..., label = FALSE, accuracy = NULL, name = NULL, sort = FALSE) {
  check_installed(c("dplyr", "scales"))
  counted_data <- dplyr::count(.data, ..., name = name, sort = sort)
  res_counted <- dplyr::mutate(counted_data, percent = prop.table(.data[[name %||% "n"]]))

  if (label) {
    res_counted$percent <- scales::percent(res_counted$percent, accuracy = accuracy)
  }
  res_counted
}

#' Subset rows using their positions
#'
#' A wrapper around `dplyr::bind_rows()`, `dplyr::slice_min()`, `dplyr::slice_max()`
#' @param each If `FALSE` (default), `n` and `prop` passed to `dplyr::slice_min()` and
#' `dplyr::slice_max()` will be divided by 2. (will use `ceiling()` if n is)
#' @inheritParams dplyr::slice_min
#' @family dplyr extensions
#' @param ... Arguments are passed on to methods.
#' @return An object of the same type as `.data.`
#' The output has the following properties:
#' * Each row may appear 0, 1, or many times in the output.
#' * A `minmax` column is added to show which is min, which is max.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' @export
#'
#' @examples
#' # in the presence of ties.
#' library(magrittr)
#' mtcars %>% dplyr::slice_min(cyl, n = 1)
#' # Use with_ties = FALSE to return exactly n matches
#' mtcars %>% dplyr::slice_min(cyl, n = 1, with_ties = FALSE)
#' # Use each = FALSE to have n divided in each place
#' mtcars %>% slice_min_max(cyl, n = 2)
#' # Using each = TRUE (to retun n = 2, for min, n = 2 for max)
#' mtcars %>% slice_min_max(cyl, each =  TRUE,  n = 2)
slice_min_max <- function(.data,
                          order_by,
                          each = FALSE,
                          ...,
                          n,
                          prop,
                          by = NULL,
                          with_ties = TRUE,
                          na_rm = FALSE) {
  rlang::check_installed("dplyr")
  if (!each) {
    if (!missing(n)) {
      n <- divide_ceiling_null(n, 2)
    }
    if (!missing(prop)) {
      prop <- divide_ceiling_null(prop, 2)
    }
  }
  min <- dplyr::slice_min(
    .data = .data,
    order_by = {{ order_by }},
    ...,
    n = n,
    prop = prop,
    by = {{ by }},
    with_ties = with_ties,
    na_rm = na_rm
  )
  max <- dplyr::slice_max(
    .data = .data,
    order_by = {{ order_by }},
    ...,
    n = n,
    prop = prop,
    by = {{ by }},
    with_ties = with_ties,
    na_rm = na_rm
  )
  dplyr::bind_rows(list(min = min, max = max), .id = "minmax")
}

#' Explore all rows in a random group
#'
#' Compared to `slice_sample()` `slice_group_sample` will return all rows corresponding to a group.
#'
#' @param data A `data.frame`
#' @param n_groups Number of groups to sample. (passed to `sample(size = n_groups)`)
#' @param group_var if the data is grouped, will be ignored.
#' @family family dplyr extensions
#' @return A data frame with a sample group.
#' @export
#'
#' @examples
#' set.seed(10)
#' library(magrittr)
#' slice_group_sample(mtcars, group_var = vs)
#' mtcars %>% dplyr::group_by(vs) %>% slice_group_sample()
slice_group_sample <- function(data, n_groups = 1, group_var = NULL) {

  is_grouped <- dplyr::is_grouped_df(data)

  single_group_var <- dplyr::n_groups(data) == 1
  if (is_grouped) {
    group_var_name <- dplyr::group_vars(data)
    if (length(group_var_name) != 1) {
      cli::cli_abort("Currently only works with a single group.")
    }
    # FIXME Doesn't work, problem with symbols here
    # if (!rlang::is_null({{group_var}})) {
    #   cli::cli_inform("Ignoring `group_var`, using current groups")
    # }
  }

  if (!is_grouped) {
    check_required(group_var)
    data <- dplyr::group_by(data, {{ group_var }})
  }
  if (!dplyr::is_grouped_df(data)) {
    cli::cli_abort(c(
      "Not supposed to happen"
    ),
    .internal = TRUE
    )
  }
  # Assuming the data is grouped now.
  group_var_name <- dplyr::group_vars(data)
  data$id <- vctrs::vec_group_id(data[[group_var_name]])

  which_ids <- sample(x = unique(data$id), size = n_groups)

  sample <- data[data$id %in% which_ids, ]

  sample$id <- NULL
  sample
}
#' Keep rows that match one of the conditions
#'
#' The `filter_or()` function is used to subset a data frame, retaining all rows
#' that satisfy **at least one of** your conditions.
#' To be retained, the row must produce a value of `TRUE` for **one of the conditions**
#' Note that when a condition evaluates to `NA` the row will be dropped, (hence this function) unlike base subsetting with `[`.
#'
#' The reason to be of this function is to simplify a call like
#' ```r
#' # with dplyr::filter
#' dat %>% dplyr::filter(vs == 1 | is.na(vs))
#' dat %>% filter_or(vs == 1, is.na(vs))
#' ```
#' Basically, this is just a shortcut to `mutate(.data, new_lgl_vars)` + `filter(if_any(new_lgl_vars))` + `select(-new_lgl_vars)`
#'
#' @param .data A data frame
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expressions that
#'   return a logical value, and are defined in terms of the variables in
#'   `.data`. If multiple expressions are included, they are combined with the
#'   `|` operator. Only rows for which **one of the conditions** evaluate to `TRUE` are
#'   kept.
#' @param .keep If `NULL`, will keep variables if they are named, drop otherwise.
#'   if `TRUE`, will drop, if `FALSE`, will keep (still don't know how to implement.)
#'
#' @returns
#'   An object of the same type as `.data`. The output has the following properties:
#'
#'  * Rows are a subset of the input, but appear in the same order.
#'  * Columns are not modified (if `.keep = FALSE`.
#'  * The number of groups may be reduced (if `.preserve` is not `TRUE`).
#'  * Data frame attributes are preserved.
#' @keywords internal
#' @export
#' @examples
#' library(magrittr)
#' mtcars %>% dplyr::filter(cyl > 5 | mpg == 2)
#' mtcars %>% dplyr::filter(!(!cyl > 5 & !mpg == 2))
#' mtcars %>% filter_or(cyl > 5, vs == 0)
filter_or <- function(.data, ..., .keep = NULL) {
  if (!is.null(.keep) || isTRUE(.keep)) {
    cli::cli_abort("Still don't know how to keep named variables only. Defaults to `FALSE`/ `NULL` for now.")
  }
  n_var <- rlang::dots_n(...)
  variables <- .data %>%
    dplyr::mutate(..., .before = 0)
  if (all(purrr::map_lgl(variables[, 1:n_var], is.logical))) {
    res <-  variables %>%
      dplyr::filter(dplyr::if_any(1:n_var)) %>%
      dplyr::select(!1:n_var)
    return(res)
  }
  cli::cli_abort("You didn't provide logical expressions. See {.help dplyr::filter}")
}


#' Elegant wrapper around filter and pull
#'
#' It can be very useful when trying to extract a value from somewhere,
#' and you have one col that represents the unique id.
#'
#' @param data The data
#' @param filter the filter
#' @param name The variable for the name (by default, will look for `rownames`), can be quoted (safer).
#' @inheritParams dplyr::pull
#' @param length A fixed length to check for the output
#' @param unique A logical. Should return unique values?
#' @return A (named) character vector (if name is specified)
#' @export
#'
#' @examples
#' # extract the skin_color for C-3PO
#' library(magrittr)
#' extract_cell_value(
#' data = dplyr::starwars,
#'  var = skin_color,
#'  filter = name == "C-3PO",
#'  length = 1 # ensure the length will be 1.
#' )
#' # will return a named vector of mpg (as mtcars has rownames.)
#' mtcars %>%
#'  extract_cell_value(
#'  var = mpg,
#'  filter = vs == 0
#'  )
#' # Extract hair color for all people
#' extract_cell_value(
#'  data = dplyr::starwars,
#'  var = skin_color,
#'  filter = TRUE,
#'  name = "name" # ensure it is a named vector that corresponds to their unique ID
#' )
extract_cell_value <- function(data, var, filter, name = NULL, length = NULL, unique = FALSE) {
  if (missing(var) || missing(filter)) {
    cli::cli_abort(c(
      "Must provide the `var` and `filter` you want to extract. "
    ))
  }
  # rlang:::check_arg(var)
  #rlang:::check_arg(filter)
  if (is.null(name)) {
    if (tibble::has_rownames(data)) {
      data <- tibble::rownames_to_column(data)
    }
    if ("rowname" %in% names(data)) {
      name <- "rowname"
    }
  }

    res <- dplyr::filter(data, {{ filter }})
    res2 <- dplyr::pull(res, var = {{ var }}, name = {{ name }})

    if (unique) {
      res2 <- unique_named(res2)
    }
    if (!is.null(length)) {
      # TODO use `check_length()` when implemented.
      if (!rlang::has_length(res2, length)) {
        cli::cli_abort(c(
          "Expected an output of {length}",
          "Got an output of {length(res2)}"
        )
        )
      }
    }

    res2
}

# dplyr extra ------------------------------------------
#' Count observations by group and compute percentage
#'
#' `count_pct()` lets you quickly count the unique values of one or more
#' variables: `df |> count_pct(a, b)` It calculates the percentage by group
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
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   count_pct(cyl)
#'
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   count_pct(cyl, label = TRUE)
#'
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   count_pct(cyl, label = TRUE, accuracy = 0.1)
count_pct <- function(.data, ..., label = FALSE, accuracy = NULL, name = NULL, sort = FALSE) {
  counted_data <- dplyr::count(.data, ..., name = name, sort = sort)
  res_counted <- dplyr::mutate(counted_data, percent = prop.table(.data[[name %||% "n"]]))

  if (label) {
    check_installed("scales", "to label percentages.")
    res_counted$percent <- scales::percent(res_counted$percent, accuracy = accuracy)
  }
  res_counted
}

#' Subset rows using their positions
#'
#' A wrapper around `dplyr::bind_rows()`, `dplyr::slice_min()`, `dplyr::slice_max()`
#' @param each If `FALSE`, `n` and `prop` passed to `dplyr::slice_min()` and
#' `dplyr::slice_max()` will be divided by 2. (will use `ceiling()` if n is)
#' @param ascending Return the output in ascending order. (min on top)
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
#' mtcars |> dplyr::slice_min(cyl, n = 1)
#' # Use with_ties = FALSE to return exactly n matches
#' mtcars |> dplyr::slice_min(cyl, n = 1, with_ties = FALSE)
#' # Use each = FALSE to have n divided in each place
#' mtcars |> slice_min_max(cyl, n = 2)
#' # Using each = TRUE (to retun n = 2, for min, n = 2 for max)
#' mtcars |> slice_min_max(cyl, each = TRUE, n = 2)
slice_min_max <- function(.data,
                          order_by,
                          ...,
                          n,
                          prop,
                          by = NULL,
                          with_ties = TRUE,
                          na_rm = FALSE,
                          each = TRUE,
                          ascending = TRUE) {
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
  # So values are displayed in order.
  max <- dplyr::arrange(max, {{ order_by }})
  res <- dplyr::bind_rows(list(min = min, max = max), .id = "minmax")

  if (!ascending) {
    res <- dplyr::arrange(res, dplyr::desc({{ order_by }}))
  }
  res
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
#' slice_group_sample(mtcars, group_var = vs)
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   slice_group_sample()
slice_group_sample <- function(data, group_var = NULL, n_groups = 1) {
  is_grouped <- dplyr::is_grouped_df(data)

  single_group_var <- dplyr::n_groups(data) == 1
  if (!is_grouped && rlang::quo_is_null(enquo(group_var))) {
    cli::cli_abort(c(
      "Either `data` must be grouped by 1 variable, or {.arg group_var} is provided."
    ))
  }

  if (is_grouped) {
    group_var_name <- dplyr::group_vars(data)
    if (length(group_var_name) != 1) {
      cli::cli_abort("Currently only works with a single group.")
    }
    # FIXME Doesn't work, problem with symbols here
    if (!rlang::quo_is_null(enquo(group_var))) {
      cli::cli_inform("Ignoring `group_var`, using current groups")
    }
  }

  if (!is_grouped) {
    check_required(group_var)
    data <- dplyr::group_by(data, {{ group_var }})
  }
  if (!dplyr::is_grouped_df(data)) {
    cli::cli_abort(
      c(
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
  if (!is_grouped) {
    # return ungrouped if x was ungrouped
    sample <- dplyr::ungroup(sample)
  }
  sample
}
#' Keep rows that match one of the conditions
#'
#' The `filter_if_any()` function is used to subset a data frame, retaining all
#' rows that satisfy **at least one of** your conditions.
#' To be retained, the row must produce a value of `TRUE` for
#' **one of the conditions**. Note that when a condition evaluates to `NA` the
#' row will be dropped, (hence this function) unlike base subsetting with `[`.
#'
#' The reason to be of this function is to simplify a call like
#'
#' ```r
#' # with dplyr::filter
#' dat |> dplyr::filter(vs == 1 | is.na(vs))
#' data |>
#'   dplyr::mutate(cond1 = vs == 1, cond2 = is.na(vs)) |>
#'   dplyr::filter(dplyr::if_any(starts_with("cond")))
#' dat |> filter_if_any(vs == 1, is.na(vs))
#' ```
#'
#' Basically, this is just a shortcut to
#' `mutate(.data, new_lgl_vars)` + `filter(if_any(new_lgl_vars))` + `select(-new_lgl_vars)`.
#' It allows mutate_like syntax in `filter(if_any(...))`.
#'
#' Caution: still doesn't work with [dplyr::across()], use the regular
#' `filter(if_any())` syntax.
#'
#' @param .data A data frame
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output.
#'
#'   The value can be:
#'
#'   * A logical vector, which will be recycled to the correct length.
#'   * A logical vector the same length as the current group (or the whole data frame
#'     if ungrouped).
#'
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expressions that
#'   return a logical value, and are defined in terms of the variables in
#'   `.data`. If multiple expressions are included, they are combined with the
#'   `|` operator. Only rows for which **one of the conditions** evaluate to
#'   `TRUE` are kept.
#' @param .by See [dplyr::dplyr_by].
#' @param .keep_new_var If `TRUE`, will remove newly created variables.
#' @returns An object of the same type as `.data`.
#'   The output has the following properties:
#'
#'  * Rows are a subset of the input, but appear in the same order.
#'  * Columns are not modified (if `.keep_new_var = FALSE`.
#'  * Data frame attributes are preserved.
#' @export
#' @examples
#' mtcars |> dplyr::filter(cyl > 5 | mpg == 2)
#' mtcars |> dplyr::filter(!(!cyl > 5 & !mpg == 2))
#' mtcars |> filter_if_any(cyl > 5, vs == 0)
filter_if_any <- function(.data, ..., .by = NULL, .keep_new_var = FALSE) {
  check_by_typo()
  n_var <- rlang::dots_n(...)
  variables <- dplyr::mutate(.data, ..., .before = 0, .by = {{ .by }})

  if (all(purrr::map_lgl(variables[, 1:n_var], is.logical))) {
    res <- dplyr::filter(variables, dplyr::if_any(.cols = seq_len(n_var)), .by = {{ .by }})

    if (.keep_new_var) {
      cli::cli_warn("You have modified the original data")
    } else {
      res <- res[-seq_len(n_var)]
    }

    return(res)
  }

  cli::cli_abort(c(
    x = "You must provide logical expressions to {.arg {cli::symbol$ellipsis}}",
    i = "See {.help dplyr::filter} for more information on how it works."
  ))
}
#' Elegant wrapper around filter and pull
#'
#' It can be very useful when trying to extract a value from somewhere, and you
#' have one col that represents the unique id.
#'
#' @param data A data.frame
#' @param filter the filter
#' @param name The variable for the name (by default, will look for `rownames`),
#'   can be quoted (safer).
#' @inheritParams dplyr::pull
#' @param length A fixed length to check for the output
#' @param unique A logical. Should return unique values?
#' @return A (named) character vector (if name is specified)
#' @export
#'
#' @examples
#' # extract the skin_color for C-3PO
#' extract_cell_value(
#'   data = dplyr::starwars,
#'   var = skin_color,
#'   filter = name == "C-3PO",
#'   length = 1 # ensure the length will be 1.
#' )
#' # will return a named vector of mpg (as mtcars has rownames.)
#' mtcars |>
#'   extract_cell_value(
#'     var = mpg,
#'     filter = vs == 0
#'   )
#'
#' # Extract hair color for all people
#' extract_cell_value(
#'   data = dplyr::starwars,
#'   var = skin_color,
#'   filter = TRUE,
#'   name = "name" # ensure it is a named vector that corresponds to their unique ID
#' )
extract_cell_value <- function(data, var, filter, name = NULL, length = NULL, unique = FALSE) {
  check_required(data)
  check_required(var)

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

  if (!is.null(length) && !rlang::has_length(res2, length)) {
    # TODO use `check_length()` when implemented. r-lib/rlang#1618
    cli::cli_abort(c(
      "Expected an output of {length}",
      "Got an output of {length(res2)}"
    ))
  }

  res2
}

# summarise with total ---------------------------------------------------------
#' Compute a summary for one group with the total included.
#'
#' This function is useful to create end tables, apply the same formula to a group and to its overall.
#' You can specify a personalized `Total` value with the `.label` argument. You
#' You should only use the output from `summarise_with_total()` with `tidyr::pivot_wider()`,
#' write data to a spreadsheet, `gt::gt()` after that. Don't try to do more computing afterwards.
#' It can also be used for plotting
#' Changes the `.by` variable to a factor.
#'
#'
#' @inheritParams dplyr::summarise
#' @param .label Label of the total value
#' @param .first Should the total be on top
#' @return An ungrouped data frame with the total included in the first or last row.
#' @export
#'
#' @examples
#' # works with `.by`
#'
#' mtcars |>
#'   summarise_with_total(
#'     x = mean(mpg),
#'     .by = vs,
#'     .label = "All vs"
#'   )
#'
#' # works with `group_by()`
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   summarise_with_total(
#'     x = mean(mpg),
#'     .label = "All vs"
#'   )
summarise_with_total <- function(.data, ..., .by = NULL, .label = "Total", .first = TRUE) {
  check_string(.label)
  # check_dots_used()

  # Computing summary (depending if .data is grouped or uses `.by`)
  if (dplyr::is_grouped_df(.data)) {
    group_var <- dplyr::group_vars(.data)

    if (length(group_var) != 1) {
      cli::cli_abort(c(
        "Must supply a single group"
      ))
    }

    by_summary <- dplyr::summarise(.data, ...)

    summary <- dplyr::summarise(
      .data = dplyr::ungroup(.data),
      "{group_var}" := .label,
      ...
    )
  } else {
    # compute summary by variable
    by_summary <- dplyr::summarise(.data, ..., .by = {{ .by }})

    # Compute the summary for total
    summary <- dplyr::summarise(.data, "{{ .by }}" := .label, ...)
  }

  # Decide how to arrange the data.
  summary_levels <- if (.first) {
    c(.label, as.character(levels(by_summary[[1]]) %||% unique(by_summary[[1]])))
  } else {
    c(as.character(levels(by_summary[[1]]) %||% unique(by_summary[[1]])), .label)
  }

  if (is.factor(by_summary[[1]])) {
    by_summary[[1]] <- factor(by_summary[[1]], levels = summary_levels)
    summary[[1]] <- factor(summary[[1]], levels = summary_levels)
  } else if (!is.character(by_summary[[1]])) {
    by_summary[[1]] <- factor(by_summary[[1]], levels = summary_levels)
    summary[[1]] <- factor(summary[[1]], levels = summary_levels)
  }


  # .first decides which ones to bind
  if (.first) {
    res <- dplyr::bind_rows(
      summary, by_summary
    )
  } else {
    res <- dplyr::bind_rows(
      by_summary, summary
    )
  }
  res
}

#' Transform to NA any of the condition
#'
#' This function is similar to `dplyr::na_if()`, but it has 2 differences. the
#' values of `y` are never recycled. There are two ways to provide the condition.
#' As values or as a logical vector.
#' @param x A vector.
#' @param values A vector of values. If the length of values = 1, it is actually
#'   the preferable to use `dplyr::na_if()` for clarity.
#' @param expr A logical vector same length as x
#' @return `x` with `NA` values when required.
#' @export
#'
#' @examples
#' vec <- c(0, 1, 1, 2)
#' vec2 <- c("Here", "not", NA, "Here")
#' # NA all 2s
#' # You can actually use dplyr::na_if() in this case
#' dplyr::na_if(vec, 2)
#' # NA all 1 and 2
#' na_if2(vec, c(1, 2))
#' na_if2(vec, expr = vec2 == "Here")
na_if2 <- function(x, values, expr) {
  switch(rlang::check_exclusive(expr, values),
    expr = {
      if (!is.logical(expr)) {
        cli::cli_abort("{.arg expr} must be a logical vector the same size as x, not {.obj_type_friendly {expr}}")
      }
      x[expr] <- NA
      x
    },
    values = {
      x[x %in% values] <- NA
      x
    }
  )
}

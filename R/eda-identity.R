# ----- dplyr/base identity helpers --------------------
#' Helpers that return the same value
#'
#' @description
#' They all share the `*_identity` suffix, they are silent in non-interactive
#' sessions. They are very handy to create clickable hyperlinks that do not
#' modify the current state of the analysis.
#'
#' They are inspired by [pillar::glimpse], [tibble::view].
#'
#' Look at the original functions for the other parameters.
#'
#' # Use cases / advantages
#'
#' * Like many other reuseme functions, they are most useful in interactive sessions
#' * print the result in interactive sessions (quiet in non-interactive.)
#' * Create runnable hyperlinks (In August 2023, RStudio forbids runnable
#'   hyperlinks of base functions, or non-package functions. (i.e. that don't have `::`))
#' * Use in pipelines to explore the data
#' * Use [rlang::is_interactive()] over [base::interactive()] as it's easier to
#' control and test with `options(rlang_interactive)`
#' * Use the original functions for your final results.
#' * `count_identity()` also prints percentages.
#' * `slice_identity()` can be useful to resolve many-to-many warnings from
#'   dplyr join functions.
#'
#' # Caution
#'
#' * Don't put those at the end of a pipeline
#' * Don't name the first argument, to avoid conflicts in case a column in the
#'   data is named `x`.
#' * Some functions have small tweaks
#'   * `mutate_identity()` only prints the distinct values, and uses
#'     `.keep = "used"`, `.before = 0`, unless specified to improve the display.
#'   * `count_identity()` is a wrapper of [count_pct()]
#'     (itself a wrapper of `dplyr::count()`),
#'   * `count_identity()` may fail if there is already a variable named `n`.
#'   * `slice_min/max_identity()` relocates the target column at the beginning.
#'   * `filter_identity()` prints a short message if no rows are returned.
#'
#' @param x The main object (a data.frame, but some functions accept a vector.)
#'   (aka `.data` in some `dplyr` functions, but naming it `x` throughout.)
#' @param extra_msg A character vector of observations that will print to
#'   console, notes taken related to the transformation.
#' @param nrows Number of rows to print.
#' @param name,sort,.keep_all,.by,by,n_groups,group_var,...,n,prop,with_ties,order_by,.keep,.before,each,na_rm,weight_by,replace,.by_group,.keep_new_var,.preserve,ascending Check original functions.
#'
#' @returns `x`, the original input is (invisibly) returned.
#'   (allowing the `*_identity()` functions to be used in a pipeline) will print
#'    `extra_msg` to the console in interactive sessions.
#' @seealso
#' * [dplyr::distinct()]
#' * [dplyr::filter()]
#' * [dplyr::slice()]
#' * [dplyr::mutate()]
#' * [dplyr::arrange()]
#' * [count_pct()]
#' * [slice_min_max()]
#' * [slice_group_sample()]
#'
#' @name eda-identity
#' @examples
#' withr::local_options(rlang_interactive = TRUE)
#' # Workflow to explore mtcars
#' mtcars |>
#'   filter_identity(mpg > 21, extra_msg = c("Wow, these rows are very interesting.")) |>
#'   count_identity(
#'     vs,
#'     extra_msg = c(
#'       "Woo, there are 14 obs with vs = 1, 18 obs with vs = 0",
#'       "The split is 56%-43%"
#'     )
#'   ) |>
#'   dplyr::filter(disp > 150) # after all, I need only disp > 150
#'
NULL
# base identity functions ------------------------------------------------------
#' @rdname eda-identity
#' @export
names_identity <- function(x, nrows = NULL, extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  print(names(x))
  invisible(x)
}
# dplyr identity functions with small tweaks ----------

#' @export
#' @rdname eda-identity
count_identity <- function(x, ..., sort = TRUE, name = NULL, nrows = NULL, extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  count <- count_pct(.data = x, ..., sort = sort, name = name, label = TRUE)

  if (nrow(count) > 0) {
    print(count, n = nrows)
    cli::cli_alert_info(extra_msg)
  }

  invisible(x)
}
#' @rdname eda-identity
#' @export
mutate_identity <- function(x,
                            ...,
                            .keep = NULL,
                            .before = NULL,
                            nrows = NULL,
                            extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  new_var <- dplyr::mutate(
    .data = x,
    ...,
    .keep = .keep %||% "used",
    .before = .before %||% 0
  )

  print(dplyr::distinct(new_var), n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}
#' @rdname eda-identity
#' @export
slice_identity <- function(x,
                           ...,
                           .by = NULL,
                           .preserve = FALSE,
                           nrows = NULL,
                           extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- dplyr::slice(
    .data = x,
    ...,
    .by = {{ .by }},
    .preserve = .preserve
  )
  print(res, n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}
#' @rdname eda-identity
#' @export
slice_min_identity <- function(x,
                               order_by,
                               ...,
                               n,
                               prop,
                               by = NULL,
                               with_ties = TRUE,
                               na_rm = TRUE,
                               nrows = NULL,
                               extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- dplyr::slice_min(
    .data = x,
    order_by = {{ order_by }},
    ...,
    n = n,
    prop = prop,
    by = {{ by }},
    with_ties = with_ties,
    na_rm = na_rm
  )

  res <- dplyr::relocate(res, {{ order_by }})
  print(res, n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}
#' @rdname eda-identity
#' @export
slice_max_identity <- function(x,
                               order_by,
                               ...,
                               n,
                               prop,
                               by = NULL,
                               with_ties = TRUE,
                               na_rm = TRUE,
                               nrows = NULL,
                               extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- dplyr::slice_max(
    .data = x,
    order_by = {{ order_by }},
    ...,
    n = n,
    prop = prop,
    by = {{ by }},
    with_ties = with_ties,
    na_rm = na_rm
  )

  res <- dplyr::relocate(res, {{ order_by }})
  print(res, n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}
# dplyr identity without tweaks ------------------------------------------------
#' @export
#' @rdname eda-identity
arrange_identity <- function(x, ..., .by_group = FALSE, nrows = NULL, extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- dplyr::arrange(.data = x, ..., .by_group = .by_group)
  if (nrow(res) > 0) {
    print(res, n = nrows)
    cli::cli_alert_info(extra_msg)
  }
  invisible(x)
}
#' @export
#' @rdname eda-identity
distinct_identity <- function(x, ..., .keep_all = FALSE, .arrange = FALSE, nrows = NULL, extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- dplyr::distinct(.data = x, ..., .keep_all = .keep_all)
  if (nrow(res) > 0) {
    if (.arrange) res <- dplyr::arrange(res, ...)

    if (inherits(res, "tbl_df")) {
      print(res, n = nrows)
    } else {
      print(res)
    }
    cli::cli_alert_info(extra_msg)
  }
  invisible(x)
}
#' @export
#' @rdname eda-identity
filter_identity <- function(x,
                            ...,
                            .by = NULL,
                            nrows = NULL,
                            extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }
  filtered <- dplyr::filter(.data = x, ..., .by = {{ .by }})
  if (nrow(filtered) > 0) {
    print(filtered, n = nrows)
    cli::cli_alert_info(extra_msg)
  } else {
    cli::cli_inform(c("{.fn reuseme::filter_identity} returned no row.", extra_msg))
  }
  invisible(x)
}
#' @rdname eda-identity
#' @export
slice_sample_identity <- function(x,
                                  ...,
                                  n,
                                  prop,
                                  by = NULL,
                                  weight_by = NULL,
                                  replace = FALSE,
                                  nrows = NULL,
                                  extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- dplyr::slice_sample(
    .data = x,
    ...,
    n = n,
    prop = prop,
    by = {{ by }},
    weight_by = {{ weight_by }},
    replace = replace
  )

  print(res, n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}

# dplyr extensions identity ----------------------------------------------------
#' @rdname eda-identity
#' @export
filter_if_any_identity <- function(x,
                                   ...,
                                   .by = NULL,
                                   .keep_new_var = FALSE,
                                   nrows = NULL,
                                   extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  filtered <- filter_if_any(.data = x, ..., .by = {{ .by }}, .keep_new_var = .keep_new_var)

  if (nrow(filtered) > 0) {
    print(filtered, n = nrows)
    cli::cli_alert_info(extra_msg)
  } else {
    cli::cli_inform(c("{.fn reuseme::filter_if_any} returned no row.", extra_msg))
  }

  invisible(x)
}
#' @export
#' @rdname eda-identity
slice_min_max_identity <- function(x,
                                   order_by,
                                   ...,
                                   n,
                                   prop,
                                   by = NULL,
                                   with_ties = TRUE,
                                   na_rm = FALSE,
                                   each = TRUE,
                                   ascending = TRUE,
                                   nrows = NULL,
                                   extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- slice_min_max(
    .data = x,
    order_by = {{ order_by }},
    ...,
    n = n,
    prop = prop,
    by = {{ by }},
    with_ties = with_ties,
    each = each,
    ascending = ascending,
    na_rm = na_rm
  )

  res <- dplyr::relocate(res, {{ order_by }})
  print(res, n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}
#' @rdname eda-identity
#' @export
slice_group_sample_identity <- function(x,
                                        group_var = NULL,
                                        n_groups = 1,
                                        nrows = NULL,
                                        extra_msg = NULL) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }

  res <- slice_group_sample(
    data = x,
    n_groups = n_groups,
    group_var = {{ group_var }}
  )

  print(res, n = nrows)
  cli::cli_alert_info(extra_msg)
  invisible(x)
}

# helpers -----
# Copy everywherere
identity_if_non_interactive <- function(x) {
  if (!rlang::is_interactive()) {
    return(invisible(x))
  }
}
divide_ceiling_null <- function(x, div) {
  if (is.null(x)) {
    return(x)
  }

  ceiling(x / div)
}

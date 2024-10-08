% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-plus.R
\name{summarise_with_total}
\alias{summarise_with_total}
\title{Compute a summary for groups with the total included.}
\usage{
summarise_with_total(.data, ..., .by = NULL, .label = "Total", .first = TRUE)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{...}{<\code{\link[rlang:args_data_masking]{data-masking}}> Name-value pairs of
summary functions. The name will be the name of the variable in the result.

The value can be:
\itemize{
\item A vector of length 1, e.g. \code{min(x)}, \code{n()}, or \code{sum(is.na(y))}.
\item A data frame, to add multiple columns from a single expression.
}

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Returning values with size 0 or >1 was
deprecated as of 1.1.0. Please use \code{\link[dplyr:reframe]{reframe()}} for this instead.}

\item{.by}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> Optionally, a selection of columns to
group by for just this operation, functioning as an alternative to \code{\link[dplyr:group_by]{group_by()}}. For
details and examples, see \link[dplyr:dplyr_by]{?dplyr_by}.}

\item{.label}{Label of the total value}

\item{.first}{Should the total be on top}
}
\value{
An ungrouped data frame with the total included in the first or last row.
}
\description{
This function is useful to create end tables, apply the same formula to a group and to its overall.
You can specify a personalized \code{Total} value with the \code{.label} argument. You
You should only use the output from \code{summarise_with_total()} with \code{tidyr::pivot_wider()},
write data to a spreadsheet, \code{gt::gt()} after that. Don't try to do more computing afterwards.
It can also be used for plotting
Changes the \code{.by} variable to a factor.
}
\examples{
# works with `.by`

mtcars |>
  summarise_with_total(
    x = mean(mpg),
    .by = vs,
    .label = "All vs"
  )

# works with `group_by()`
mtcars |>
  dplyr::group_by(vs) |>
  summarise_with_total(
    x = mean(mpg),
    .label = "All vs"
  )
}

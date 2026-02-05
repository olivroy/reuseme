# Compute a summary for groups with the total included.

This function is useful to create end tables, apply the same formula to
a group and to its overall. You can specify a personalized `Total` value
with the `.label` argument. You You should only use the output from
`summarise_with_total()` with
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
write data to a spreadsheet,
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) after that. Don't
try to do more computing afterwards. It can also be used for plotting
Changes the `.by` variable to a factor.

## Usage

``` r
summarise_with_total(.data, ..., .by = NULL, .label = "Total", .first = TRUE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.

  - A data frame with 1 row, to add multiple columns from a single
    expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .label:

  Label of the total value

- .first:

  Should the total be on top

## Value

An ungrouped data frame with the total included in the first or last
row.

## Examples

``` r
# works with `.by`

mtcars |>
  summarise_with_total(
    x = mean(mpg),
    .by = vs,
    .label = "All vs"
  )
#>       vs        x
#> 1 All vs 20.09062
#> 2      0 16.61667
#> 3      1 24.55714

# works with `group_by()`
mtcars |>
  dplyr::group_by(vs) |>
  summarise_with_total(
    x = mean(mpg),
    .label = "All vs"
  )
#> # A tibble: 3 × 2
#>   vs         x
#>   <fct>  <dbl>
#> 1 All vs  20.1
#> 2 0       16.6
#> 3 1       24.6
```

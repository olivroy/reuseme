# Subset rows using their positions

A wrapper around
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
[`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)

## Usage

``` r
slice_min_max(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  each = TRUE,
  ascending = TRUE
)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- order_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variable or function of variables to order by. To order by multiple
  variables, wrap them in a data frame or tibble.

- ...:

  Arguments are passed on to methods.

- n, prop:

  Provide either `n`, the number of rows, or `prop`, the proportion of
  rows to select. If neither are supplied, `n = 1` will be used. If `n`
  is greater than the number of rows in the group (or `prop > 1`), the
  result will be silently truncated to the group size. `prop` will be
  rounded towards zero to generate an integer number of rows.

  A negative value of `n` or `prop` will be subtracted from the group
  size. For example, `n = -2` with a group of 5 rows will select 5 - 2 =
  3 rows; `prop = -0.25` with 8 rows will select 8 \* (1 - 0.25) = 6
  rows.

- by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- with_ties:

  Should ties be kept together? The default, `TRUE`, may return more
  rows than you request. Use `FALSE` to ignore ties, and return the
  first `n` rows.

- na_rm:

  Should missing values in `order_by` be removed from the result? If
  `FALSE`, `NA` values are sorted to the end (like in
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)), so
  they will only be included if there are insufficient non-missing
  values to reach `n`/`prop`.

- each:

  If `FALSE`, `n` and `prop` passed to
  [`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html)
  and
  [`dplyr::slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)
  will be divided by 2. (will use
  [`ceiling()`](https://rdrr.io/r/base/Round.html) if n is)

- ascending:

  Return the output in ascending order. (min on top)

## Value

An object of the same type as `.data.` The output has the following
properties:

- Each row may appear 0, 1, or many times in the output.

- A `minmax` column is added to show which is min, which is max.

- Groups are not modified.

- Data frame attributes are preserved.

## See also

Other dplyr extensions:
[`count_pct()`](https://olivroy.github.io/reuseme/reference/count_pct.md)

## Examples

``` r
# in the presence of ties.
mtcars |> dplyr::slice_min(cyl, n = 1)
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
# Use with_ties = FALSE to return exactly n matches
mtcars |> dplyr::slice_min(cyl, n = 1, with_ties = FALSE)
#>             mpg cyl disp hp drat   wt  qsec vs am gear carb
#> Datsun 710 22.8   4  108 93 3.85 2.32 18.61  1  1    4    1
# Use each = FALSE to have n divided in each place
mtcars |> slice_min_max(cyl, n = 2)
#>                     minmax  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710             min 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 240D              min 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230               min 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Fiat 128               min 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic            min 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla         min 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona          min 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Fiat X1-9              min 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2          min 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa           min 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E             min 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> Hornet Sportabout      max 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360             max 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 450SE             max 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL             max 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC            max 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood     max 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental    max 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial      max 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Dodge Challenger       max 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin            max 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28             max 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird       max 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Ford Pantera L         max 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Maserati Bora          max 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
# Using each = TRUE (to retun n = 2, for min, n = 2 for max)
mtcars |> slice_min_max(cyl, each = TRUE, n = 2)
#>                     minmax  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710             min 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 240D              min 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230               min 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Fiat 128               min 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic            min 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla         min 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona          min 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Fiat X1-9              min 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2          min 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa           min 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E             min 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> Hornet Sportabout      max 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360             max 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 450SE             max 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL             max 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC            max 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood     max 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental    max 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial      max 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Dodge Challenger       max 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin            max 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28             max 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird       max 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Ford Pantera L         max 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Maserati Bora          max 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
```

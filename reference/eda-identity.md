# Helpers that return the same value

They all share the `*_identity` suffix, they are silent in
non-interactive sessions. They are very handy to create clickable
hyperlinks that do not modify the current state of the analysis.

They are inspired by
[`pillar::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html),
[`tibble::view()`](https://tibble.tidyverse.org/reference/view.html).

Look at the original functions for the other parameters.

## Usage

``` r
names_identity(x, nrows = NULL, extra_msg = NULL)

count_identity(
  x,
  ...,
  sort = TRUE,
  name = NULL,
  nrows = NULL,
  extra_msg = NULL
)

mutate_identity(
  x,
  ...,
  .keep = NULL,
  .before = NULL,
  nrows = NULL,
  extra_msg = NULL
)

slice_identity(
  x,
  ...,
  .by = NULL,
  .preserve = FALSE,
  nrows = NULL,
  extra_msg = NULL
)

slice_min_identity(
  x,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = TRUE,
  nrows = NULL,
  extra_msg = NULL
)

slice_max_identity(
  x,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = TRUE,
  nrows = NULL,
  extra_msg = NULL
)

arrange_identity(x, ..., .by_group = FALSE, nrows = NULL, extra_msg = NULL)

distinct_identity(
  x,
  ...,
  .keep_all = FALSE,
  .arrange = FALSE,
  nrows = NULL,
  extra_msg = NULL
)

filter_identity(x, ..., .by = NULL, nrows = NULL, extra_msg = NULL)

slice_sample_identity(
  x,
  ...,
  n,
  prop,
  by = NULL,
  weight_by = NULL,
  replace = FALSE,
  nrows = NULL,
  extra_msg = NULL
)

filter_if_any_identity(
  x,
  ...,
  .by = NULL,
  .keep_new_var = FALSE,
  nrows = NULL,
  extra_msg = NULL
)

slice_min_max_identity(
  x,
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
  extra_msg = NULL
)

slice_group_sample_identity(
  x,
  group_var = NULL,
  n_groups = 1,
  nrows = NULL,
  extra_msg = NULL
)
```

## Arguments

- x:

  The main object (a data.frame, but some functions accept a vector.)
  (aka `.data` in some `dplyr` functions, but naming it `x` throughout.)

- nrows:

  Number of rows to print.

- extra_msg:

  A character vector of observations that will print to console, notes
  taken related to the transformation.

- name, sort, .keep_all, .by, by, n_groups, group_var, ..., n, prop,
  with_ties, order_by, .keep, .before, each, na_rm, weight_by, replace,
  .by_group, .keep_new_var, .preserve, ascending:

  Check original functions.

- .arrange:

  Should arrange output?

## Value

`x`, the original input is (invisibly) returned. (allowing the
`*_identity()` functions to be used in a pipeline) will print
`extra_msg` to the console in interactive sessions.

## Use cases / advantages

- Like many other reuseme functions, they are most useful in interactive
  sessions

- print the result in interactive sessions (quiet in non-interactive.)

- Create runnable hyperlinks (In August 2023, RStudio forbids runnable
  hyperlinks of base functions, or non-package functions. (i.e. that
  don't have `::`))

- Use in pipelines to explore the data

- Use
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  over [`base::interactive()`](https://rdrr.io/r/base/interactive.html)
  as it's easier to control and test with `options(rlang_interactive)`

- Use the original functions for your final results.

- `count_identity()` also prints percentages.

- `slice_identity()` can be useful to resolve many-to-many warnings from
  dplyr join functions.

## Caution

- Don't put those at the end of a pipeline

- Don't name the first argument, to avoid conflicts in case a column in
  the data is named `x`.

- Some functions have small tweaks

  - `mutate_identity()` only prints the distinct values, and uses
    `.keep = "used"`, `.before = 0`, unless specified to improve the
    display.

  - `count_identity()` is a wrapper of
    [`count_pct()`](https://olivroy.github.io/reuseme/reference/count_pct.md)
    (itself a wrapper of
    [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)),

  - `count_identity()` may fail if there is already a variable named
    `n`.

  - `slice_min/max_identity()` relocates the target column at the
    beginning.

  - `filter_identity()` prints a short message if no rows are returned.

## See also

- [`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)

- [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

- [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html)

- [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

- [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

- [`count_pct()`](https://olivroy.github.io/reuseme/reference/count_pct.md)

- [`slice_min_max()`](https://olivroy.github.io/reuseme/reference/slice_min_max.md)

- [`slice_group_sample()`](https://olivroy.github.io/reuseme/reference/slice_group_sample.md)

## Examples

``` r
withr::local_options(rlang_interactive = TRUE)
# Workflow to explore mtcars
mtcars |>
  filter_identity(mpg > 21, extra_msg = c("Wow, these rows are very interesting.")) |>
  count_identity(
    vs,
    extra_msg = c(
      "Woo, there are 14 obs with vs = 1, 18 obs with vs = 0",
      "The split is 56%-43%"
    )
  ) |>
  dplyr::filter(disp > 150) # after all, I need only disp > 150
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
```

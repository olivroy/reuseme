# Give a more informative error in case of tidyselect errors

Give a more informative error in case of tidyselect errors

## Usage

``` r
select_check(.data, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

## Examples

``` r
try(mtcars |> select_check(vs3))
#> Error in (function (e)  : 
#>   Available columns are `mpg`, `cyl`, `disp`, `hp`, `drat`, `wt`, `qsec`,
#> `vs`, `am`, `gear`, and `carb`
```

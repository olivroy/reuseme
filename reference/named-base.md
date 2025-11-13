# Helpers that can return a named vector

Base R keeps names in various places, but drops them elsewhere These
functions are some that I use frequently, like `max`, or `unique`

## Usage

``` r
min_named(x, na.rm = FALSE, all_matches = FALSE)

max_named(x, na.rm = FALSE, all_matches = FALSE)

unique_named(x)
```

## Arguments

- x:

  A vector

- na.rm:

  Should remove `NA`?

- all_matches:

  If `FALSE` (default), will only return the first match, similar to
  [`which.min()`](https://rdrr.io/r/base/which.min.html) /
  [`which.max()`](https://rdrr.io/r/base/which.min.html) If `TRUE`, will
  return a named vector of all values corresponding to the max value.

## Value

- `min/max_named(all_matches = FALSE)`, a named vector of length 1.

- Otherwise, a named vector.

## Examples

``` r
max_named(c("this guy" = 2, "that guy" = 3))
#> that guy 
#>        3 

unique_named(c("this guy" = 2, "that guy" = 3, "this guy" = 2))
#> this guy that guy 
#>        2        3 
# returns the same as base R for unnamed input
unique_named(c(1, 2, 3, 3))
#> [1] 1 2 3
# returns all values
min_named(c("x" = 1, "y" = 1, "ww" = 2), all_matches = FALSE)
#> x 
#> 1 

# TODO is usable with `extract_cell_value()`
```

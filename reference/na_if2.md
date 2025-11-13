# Transform to NA any of the condition

This function is similar to
[`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html),
but it has 2 differences. the values of `y` are never recycled. There
are two ways to provide the condition. As values or as a logical vector.

## Usage

``` r
na_if2(x, ..., values, expr)
```

## Arguments

- x:

  A vector.

- ...:

  These dots are for future extensions and must be empty.

- values:

  A vector of values. If the length of values = 1, it is actually the
  preferable to use
  [`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)
  for clarity.

- expr:

  A logical vector same length as x

## Value

`x` with `NA` values when required.

## Examples

``` r
vec <- c(0, 1, 1, 2)
vec2 <- c("Here", "not", NA, "Here")
# NA all 2s
# You can actually use dplyr::na_if() in this case
dplyr::na_if(vec, 2)
#> [1]  0  1  1 NA
# NA all 1 and 2
na_if2(vec, values = c(1, 2))
#> [1]  0 NA NA NA
na_if2(vec, expr = vec2 == "Here")
#> [1] NA  1  1 NA
```

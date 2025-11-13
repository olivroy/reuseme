# Filter rows by pattern

Shortcut for
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)

## Usage

``` r
filter_detect(.data, pattern, .cols = dplyr::everything(), ignore.case = TRUE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- pattern:

  character string containing a [regular
  expression](https://rdrr.io/r/base/regex.html) (or character string
  for `fixed = TRUE`) to be matched in the given character vector.
  Coerced by [`as.character`](https://rdrr.io/r/base/character.html) to
  a character string if possible. If a character vector of length 2 or
  more is supplied, the first element is used with a warning. Missing
  values are allowed except for `regexpr`, `gregexpr` and `regexec`.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to transform. You can't select grouping columns because they
  are already automatically handled by the verb (i.e.
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  or [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)).

- ignore.case:

  logical. if `FALSE`, the pattern matching is *case sensitive* and if
  `TRUE`, case is ignored during matching.

## Value

A data frame with relocated columns at first

## Examples

``` r
# don't specify column
dplyr::band_members |>
  filter_detect("Beatles")
#> # A tibble: 2 × 2
#>   band    name 
#>   <chr>   <chr>
#> 1 Beatles John 
#> 2 Beatles Paul 
# specify columns
dplyr::band_members |>
  filter_detect("Beatles", band)
#> # A tibble: 2 × 2
#>   band    name 
#>   <chr>   <chr>
#> 1 Beatles John 
#> 2 Beatles Paul 
```

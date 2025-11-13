# Count observations by group and compute percentage

`count_pct()` lets you quickly count the unique values of one or more
variables: `df |> count_pct(a, b)` It calculates the percentage by group
afterwards

## Usage

``` r
count_pct(
  .data,
  ...,
  label = FALSE,
  accuracy = NULL,
  name = NULL,
  sort = FALSE
)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

- label:

  A logical, If `TRUE`, will return the vector as character

- accuracy:

  A number to round to. Use (e.g.) `0.01` to show 2 decimal places of
  precision. If `NULL`, the default, uses a heuristic that should ensure
  breaks have the minimum number of digits needed to show the difference
  between adjacent values.

  Applied to rescaled data.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- sort:

  If `TRUE`, will show the largest groups at the top.

## Value

An object of the same type as `.data`. `count()` and `add_count()` group
transiently, so the output has the same groups as the input.

## Details

Wrapper function around
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)

## See also

Other dplyr extensions:
[`slice_min_max()`](https://olivroy.github.io/reuseme/reference/slice_min_max.md)

## Examples

``` r
count_pct(mtcars, cyl)
#>   cyl  n percent
#> 1   4 11 0.34375
#> 2   6  7 0.21875
#> 3   8 14 0.43750
mtcars |>
  dplyr::group_by(vs) |>
  count_pct(cyl)
#> # A tibble: 5 × 4
#> # Groups:   vs [2]
#>      vs   cyl     n percent
#>   <dbl> <dbl> <int>   <dbl>
#> 1     0     4     1  0.0556
#> 2     0     6     3  0.167 
#> 3     0     8    14  0.778 
#> 4     1     4    10  0.714 
#> 5     1     6     4  0.286 

mtcars |>
  dplyr::group_by(vs) |>
  count_pct(cyl, label = TRUE)
#> # A tibble: 5 × 4
#> # Groups:   vs [2]
#>      vs   cyl     n percent
#>   <dbl> <dbl> <int> <chr>  
#> 1     0     4     1 5.6%   
#> 2     0     6     3 16.7%  
#> 3     0     8    14 77.8%  
#> 4     1     4    10 71.4%  
#> 5     1     6     4 28.6%  

mtcars |>
  dplyr::group_by(vs) |>
  count_pct(cyl, label = TRUE, accuracy = 0.1)
#> # A tibble: 5 × 4
#> # Groups:   vs [2]
#>      vs   cyl     n percent
#>   <dbl> <dbl> <int> <chr>  
#> 1     0     4     1 5.6%   
#> 2     0     6     3 16.7%  
#> 3     0     8    14 77.8%  
#> 4     1     4    10 71.4%  
#> 5     1     6     4 28.6%  
```

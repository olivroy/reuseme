# Explore all rows in a random group

Compared to `slice_sample()` `slice_group_sample` will return all rows
corresponding to a group.

## Usage

``` r
slice_group_sample(data, group_var = NULL, n_groups = 1)
```

## Arguments

- data:

  A `data.frame`

- group_var:

  if the data is grouped, will be ignored.

- n_groups:

  Number of groups to sample. (passed to `sample(size = n_groups)`)

## Value

A data frame with a sample group.

## Examples

``` r
set.seed(10)
slice_group_sample(mtcars, group_var = vs)
#> # A tibble: 18 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  4  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  5  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#>  6  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3
#>  7  15.2     8  276.   180  3.07  3.78  18       0     0     3     3
#>  8  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
#>  9  10.4     8  460    215  3     5.42  17.8     0     0     3     4
#> 10  14.7     8  440    230  3.23  5.34  17.4     0     0     3     4
#> 11  15.5     8  318    150  2.76  3.52  16.9     0     0     3     2
#> 12  15.2     8  304    150  3.15  3.44  17.3     0     0     3     2
#> 13  13.3     8  350    245  3.73  3.84  15.4     0     0     3     4
#> 14  19.2     8  400    175  3.08  3.84  17.0     0     0     3     2
#> 15  26       4  120.    91  4.43  2.14  16.7     0     1     5     2
#> 16  15.8     8  351    264  4.22  3.17  14.5     0     1     5     4
#> 17  19.7     6  145    175  3.62  2.77  15.5     0     1     5     6
#> 18  15       8  301    335  3.54  3.57  14.6     0     1     5     8
mtcars |>
  dplyr::group_by(vs) |>
  slice_group_sample()
#> # A tibble: 18 × 11
#> # Groups:   vs [1]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  4  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  5  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#>  6  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3
#>  7  15.2     8  276.   180  3.07  3.78  18       0     0     3     3
#>  8  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
#>  9  10.4     8  460    215  3     5.42  17.8     0     0     3     4
#> 10  14.7     8  440    230  3.23  5.34  17.4     0     0     3     4
#> 11  15.5     8  318    150  2.76  3.52  16.9     0     0     3     2
#> 12  15.2     8  304    150  3.15  3.44  17.3     0     0     3     2
#> 13  13.3     8  350    245  3.73  3.84  15.4     0     0     3     4
#> 14  19.2     8  400    175  3.08  3.84  17.0     0     0     3     2
#> 15  26       4  120.    91  4.43  2.14  16.7     0     1     5     2
#> 16  15.8     8  351    264  4.22  3.17  14.5     0     1     5     4
#> 17  19.7     6  145    175  3.62  2.77  15.5     0     1     5     6
#> 18  15       8  301    335  3.54  3.57  14.6     0     1     5     8
```

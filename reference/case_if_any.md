# case-when, but checks for all matches, returns a character

Each case is evaluated for **all** cases and a character vector match
for each element determines the corresponding value in the output
vector. If no cases match, the `.default` is used. The function allows
you to assign multiple values to a character value, which can be very
handy for EDA.

## Usage

``` r
case_if_any(..., .default = "", .sep = ";", .drop_empty = TRUE)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A sequence of two-sided formulas. The left hand side (LHS) determines
  which values match this case. The right hand side (RHS) provides the
  replacement value.

  The LHS inputs must evaluate to logical vectors.

  The RHS inputs will be coerced to their common type.

  All inputs will be recycled to their common size. That said, we
  encourage all LHS inputs to be the same size. Recycling is mainly
  useful for RHS inputs, where you might supply a size 1 input that will
  be recycled to the size of the LHS inputs.

  `NULL` inputs are ignored.

- .default:

  The value used when all of the LHS inputs return either `FALSE` or
  `NA`.

  `.default` must be size 1 or the same size as the common size computed
  from `...`.

  `.default` participates in the computation of the common type with the
  RHS inputs.

  `NA` values in the LHS conditions are treated like `FALSE`, meaning
  that the result at those locations will be assigned the `.default`
  value. To handle missing values in the conditions differently, you
  must explicitly catch them with another condition before they fall
  through to the `.default`. This typically involves some variation of
  `is.na(x) ~ value` tailored to your usage of `case_when()`.

  If `NULL`, the default, a missing value will be used.

- .sep, :

  the separator between answers. (default is `;`), can't be a substring
  of any of the text

- .drop_empty:

  drop if no match is returned. (Defaults to `TRUE` for legibility), but
  if `FALSE`, can be used more easily with
  `tidyr::separate_wider/longer_delim()`

## Value

A vector with the same size as the common size computed from the inputs
in `...` and the same type as the common type of the RHS inputs in
`...`.

## Examples

``` r
case_if_any(
  mtcars$vs == 1 ~ "vs = 1",
  mtcars$mpg > 150 ~ "I have mpg > 150"
)
#>  [1] ""       ""       "vs = 1" "vs = 1" ""       "vs = 1" ""       "vs = 1"
#>  [9] "vs = 1" "vs = 1" "vs = 1" ""       ""       ""       ""       ""      
#> [17] ""       "vs = 1" "vs = 1" "vs = 1" "vs = 1" ""       ""       ""      
#> [25] ""       "vs = 1" ""       "vs = 1" ""       ""       ""       "vs = 1"
case_if_any(
  mtcars$vs == 1 ~ "Woww",
  mtcars$mpg > 15 ~ "QW",
  mtcars$qsec > 18 ~ "ooh lalal",
  .sep = ";",
  .default = NA
)
#>  [1] "QW"                "QW"                "Woww;QW;ooh lalal"
#>  [4] "Woww;QW;ooh lalal" "QW"                "Woww;QW;ooh lalal"
#>  [7] NA                  "Woww;QW;ooh lalal" "Woww;QW;ooh lalal"
#> [10] "Woww;QW;ooh lalal" "Woww;QW;ooh lalal" "QW"               
#> [13] "QW"                "QW"                NA                 
#> [16] NA                  NA                  "Woww;QW;ooh lalal"
#> [19] "Woww;QW;ooh lalal" "Woww;QW;ooh lalal" "Woww;QW;ooh lalal"
#> [22] "QW"                "QW"                NA                 
#> [25] "QW"                "Woww;QW;ooh lalal" "QW"               
#> [28] "Woww;QW"           "QW"                "QW"               
#> [31] NA                  "Woww;QW;ooh lalal"
```

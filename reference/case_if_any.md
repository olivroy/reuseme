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

  For `case_when()`:

  - The LHS inputs must be logical vectors. For backwards compatibility,
    scalars are
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html),
    but we no longer recommend supplying scalars.

  - The RHS inputs will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to their common type, and will be
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
    to the common size of the LHS inputs.

  For `replace_when()`:

  - The LHS inputs must be logical vectors the same size as `x`.

  - The RHS inputs will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to the type of `x` and
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
    to the size of `x`.

  `NULL` inputs are ignored.

- .default:

  The value used when all of the LHS inputs return either `FALSE` or
  `NA`.

  - If `NULL`, the default, a missing value will be used.

  - If provided, `.default` will follow the same type and size rules as
    the RHS inputs.

  `NA` values in the LHS conditions are treated like `FALSE`, meaning
  that the result at those locations will be assigned the `.default`
  value. To handle missing values in the conditions differently, you
  must explicitly catch them with another condition before they fall
  through to the `.default`. This typically involves some variation of
  `is.na(x) ~ value` tailored to your usage of `case_when()`.

- .sep, :

  the separator between answers. (default is `;`), can't be a substring
  of any of the text

- .drop_empty:

  drop if no match is returned. (Defaults to `TRUE` for legibility), but
  if `FALSE`, can be used more easily with
  `tidyr::separate_wider/longer_delim()`

## Value

For `case_when()`, a new vector where the size is the common size of the
LHS inputs, the type is the common type of the RHS inputs, and the names
correspond to the names of the RHS elements used in the result.

For `replace_when()`, an updated version of `x`, with the same size,
type, and names as `x`.

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

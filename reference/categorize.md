# Categorize a vector of free text

Related to
[`case_if_any()`](https://olivroy.github.io/reuseme/reference/case_if_any.md)
but simpler!

## Usage

``` r
categorize(variable, ..., sep = ";", ignore_case = TRUE)
```

## Arguments

- variable:

  a character vector

- ...:

  dynamic dots, list of recoding

## Value

A named character vector

## Details

It works with the same syntax as
[`forcats::fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html)

## Examples

``` r
categorize(
c("banana apple", "apple orange pear", "apple", "cucumber"),
 "yellow fruits" = c("banana", "pear"),
 "bad fruits" = c("orange")
)
#> [1] "yellow fruits"            "yellow fruits;bad fruits"
#> [3] ""                         ""                        
```

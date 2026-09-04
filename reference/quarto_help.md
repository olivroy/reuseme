# Show links to Quarto documentation of interest

Very opinionated of links I need to access periodically. Easily
accessible from R console.

## Usage

``` r
quarto_help(subject = NULL)
```

## Arguments

- subject:

  A character vector (optional)

## Value

All possible links for help invisibly

## See also

- [Quoncierge](https://github.com/lkwhite/Quoncierge) for an automated
  Quarto initialization

- [froggeR](https://github.com/kyleGrealis/froggeR) to automate Quarto

## Examples

``` r
gt_help <- quarto_help() |>
  tibble::enframe() |>
  gt::gt() |>
  gt::fmt_url(value)
gt_help


  

name
```

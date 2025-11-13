# Create a cli href with a markdown link

Transforms `[text](url)` -\> `{.href [text](url)}`

## Usage

``` r
markup_href(x)
```

## Arguments

- x:

  A character vector

## Value

A character vector with substrings changed

## See also

Other inline markup internal helpers:
[`link_gh_issue()`](https://olivroy.github.io/reuseme/reference/link_gh_issue.md)

## Examples

``` r
markup_href(c("[link](https://google.com)", "{.href [link](https://google.com)}"))
#> [1] "{.href [link](https://google.com)}" "{.href [link](https://google.com)}"
```

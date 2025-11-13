# Create a markdown link to a GitHub issue

In RStudio, links to issues are automatically recognized. This function
creates intermediate markdown links to entries of the form
rstudio/rstudio#1100

## Usage

``` r
link_gh_issue(x, home_repo = NULL)
```

## Arguments

- x:

  A string, usually lines of files that contains issue numbers.

- home_repo:

  Optional, but if supplied, will be stripped.

## Value

A markdown link linked issue to GitHub issue

## Details

Note: doesn't (yet) support without /

Basically transform repo/org#xx -\>
`[repo/org#xx](https://github.com/repo/org/issues/xx)`.

Afterwards, we use
[`markup_href()`](https://olivroy.github.io/reuseme/reference/markup_href.md)
to create a cli link

## See also

Other inline markup internal helpers:
[`markup_href()`](https://olivroy.github.io/reuseme/reference/markup_href.md)

## Examples

``` r
link_gh_issue(c("We really need rstudio/gt#1469 to be fixed."))
#> [1] "We really need [rstudio/gt#1469](https://github.com/rstudio/gt/issues/1469) to be fixed."
```

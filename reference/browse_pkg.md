# Browse pkgdown site if it exists

A wrapper around
[`usethis::browse_package()`](https://usethis.r-lib.org/reference/browse-this.html)
that aims at identifying the package website. It looks up for a link in
DESCRIPTION.

## Usage

``` r
browse_pkg(
  package = NULL,
  open = FALSE,
  news_only = FALSE,
  ref_only = FALSE,
  vignettes_show = TRUE
)
```

## Arguments

- package:

  Name of package. If `NULL`, the active project is targeted, regardless
  of whether it's an R package or not. If `"<org>/<repo>"` is supplied,
  will jump to GitHub homepage.

- open:

  Whether to open the pkgdown site in the browser.

- news_only:

  Should only the news link be shown?

- ref_only:

  Should only the reference index be show?

- vignettes_show:

  Should the vignette information be displayed on console?

## Value

The package website URL, invisibly. (If `ref_only`, or `news_only`, the
reference index URL or Changelog URL).

## Examples

``` r
if (FALSE) { # interactive()
browse_pkg("reuseme")
browse_pkg()
}
```

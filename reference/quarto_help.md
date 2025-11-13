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

value

dashboard

[https://quarto.org/docs/dashboards/data-display.html](https://quarto.org/docs/dashboards/data-display.html)

manuscript

[https://quarto.org/docs/manuscripts/authoring/rstudio.html](https://quarto.org/docs/manuscripts/authoring/rstudio.html)

revealjs

[https://quarto.org/docs/presentations/revealjs/](https://quarto.org/docs/presentations/revealjs/)

blog

[https://quarto.org/docs/websites/website-blog.html](https://quarto.org/docs/websites/website-blog.html)

theme

[https://quarto.org/docs/dashboards/theming.html#theme-options](https://quarto.org/docs/dashboards/theming.html#theme-options)

param

[https://quarto.org/docs/computations/parameters.html#knitr](https://quarto.org/docs/computations/parameters.html#knitr)

quartopub

[https://quarto.org/docs/publishing/quarto-pub.html](https://quarto.org/docs/publishing/quarto-pub.html)

gh-actions

[https://quarto.org/docs/publishing/github-pages.html](https://quarto.org/docs/publishing/github-pages.html)

netlify

[https://quarto.org/docs/publishing/netlify.html](https://quarto.org/docs/publishing/netlify.html)

gallery

[https://quarto.org/docs/gallery/](https://quarto.org/docs/gallery/)

news

[https://quarto.org/docs/download/](https://quarto.org/docs/download/)

shortcode

[https://quarto.org/docs/extensions/shortcodes.html](https://quarto.org/docs/extensions/shortcodes.html)

content-hide

[https://quarto.org/docs/authoring/conditional.html](https://quarto.org/docs/authoring/conditional.html)

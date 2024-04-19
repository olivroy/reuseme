#' Show links to Quarto documentation of interest
#'
#' Very opinionated of links I need to access periodically. Easily
#' accessible from R console.
#'
#' @param subject A character vector (optional)
#'
#' @return All possible links for help invisibly
#' @export
#'
#' @examplesIf rlang::is_installed("gt")
#' quarto_help() |>
#'   tibble::enframe() |>
#'   gt::gt() |>
#'   gt::fmt_url(value)
quarto_help <- function(subject = NULL) {
  # need to update snapshot as we enhance this!
  links_help <- c(
    "dashboard" = "https://quarto.org/docs/dashboards/data-display.html",
    "manuscript" = "https://quarto.org/docs/manuscripts/authoring/rstudio.html",
    "revealjs" = "https://quarto.org/docs/presentations/revealjs/",
    "blog" = "https://quarto.org/docs/websites/website-blog.html",
    "theme" = "https://quarto.org/docs/dashboards/theming.html#theme-options",
    "param" = "https://quarto.org/docs/computations/parameters.html#knitr",
    "quartopub" = "https://quarto.org/docs/publishing/quarto-pub.html",
    "gh-actions" = "https://quarto.org/docs/publishing/github-pages.html",
    "netlify" = "https://quarto.org/docs/publishing/netlify.html",
    "gallery" = "https://quarto.org/docs/gallery/",
    "news" = "https://quarto.org/docs/download/",
    "shortcode" = "https://quarto.org/docs/extensions/shortcodes.html"
  )
  if (!is.null(subject)) {
    subject <- rlang::arg_match(subject, names(links_help), multiple = TRUE)
    urls <- href_name_url(links_help[subject])
    cli::cli_text(urls)
  }
  invisible(links_help)
}

# creates a link from named vector [name(x)](x)
href_name_url <- function(x) {
  links <- stringr::str_glue("{{.href [{names(x)}]({x})}}")
  cli::ansi_collapse(links)
}

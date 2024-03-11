#' Browse pkgdown site if it exists
#'
#' A wrapper around [usethis::browse_package()] that aims at identifying the
#' package website. It looks up for a link in DESCRIPTION.
#'
#' @param package Name of package. If `NULL`, the active project is targeted,
#'   regardless of whether it's an R package or not.
#' @param open Whether to open the pkgdown site in the browser.
#' @param news_only Should only the news link be shown?
#' @param ref_only Should only the reference index be show?
#' @param vignettes_show Should the vignette information be displayed on console?
#'
#' @return The package website URL, invisibly. (If `ref_only`, or `news_only`, the reference index URL or Changelog URL).
#' @export
#'
#' @examplesIf interactive()
#' browse_pkg("reuseme")
#' browse_pkg()
browse_pkg <- function(package = NULL,
                       open = rlang::is_interactive(),
                       news_only = FALSE,
                       ref_only = FALSE,
                       vignettes_show = TRUE) {
  # Using default package
  package <- package %||% fs::path_file(proj_get2())
  open <- open

  withr::local_options(
    list(
      # make sure usethis doesn't open links
      rlang_interactive = FALSE,
      # faster rendering https://github.com/r-lib/cli/issues/607
      cli.num_colors = cli::num_ansi_colors()
    )
  )
  urls <- usethis::browse_package(package)

  pkgdown <-
    stringr::str_subset(
      urls,
      "github.com/.*/|cran\\.|contact.html",
      negate = TRUE
    )

  # If there are more than one link.
  if (length(pkgdown) > 1) {
    # using common URLs
    pkgdown <- stringr::str_subset(urls, "github.io|docs.ropensci|r.igraph")

    if (length(pkgdown) > 1) {
      pkgdown <- stringr::str_subset(pkgdown, package)
    }
  }

  cran_home <- suppressMessages(usethis::browse_cran(package))
  withCallingHandlers(
    github_home <- suppressMessages(usethis::browse_github(package)),
    warning = function(cnd) {
      cli::cli_warn("Package {.pkg {package}} has no gh URLs, using CRAN mirror.")
      # https://stackoverflow.com/questions/77647149/how-to-overwrite-a-warning-in-r#77647281
      tryInvokeRestart("muffleWarning")
    },
    error = function(cnd) {
      cli::cli_abort("Can't do it", parent = cnd)
    }
  )

  # identified pkgdown situation
  if (rlang::has_length(pkgdown, 1)) {
    if (interactive() && open) {
      utils::browseURL(pkgdown)
    }

    pkgdown_tabs <- c(
      "news",
      "articles",
      "dev",
      "reference"
    )

    pkgdown <- stringr::str_remove(pkgdown, "/$")
    pkgdown_tabs_url <- paste0(pkgdown, "/", pkgdown_tabs)

    if (news_only) {
      return(cli::style_hyperlink("news", pkgdown_tabs_url[1]))
    }

    if (ref_only) {
      return(pkgdown_tabs_url[4])
    }

    bullets <- c(
      paste0("{.href [", pkgdown_tabs, "](", pkgdown_tabs_url, ")}"),
      "{.href [cran]({cran_home})}",
      "{.href [github]({github_home})}",
      ""
    )
    hrefs <- paste0("{.href [", package, "](", pkgdown, ")}")
    cli::cli_h2(hrefs)
    cli::cli_bullets(bullets)

    if (vignettes_show) {
      the_vignette(package, browser = FALSE)
    }

    return(invisible(pkgdown))
  }

  # No gh site.

  # for a github site
  if (news_only) {
    github_news <- glue::glue(github_home, "/blob/HEAD/NEWS.md")
    return(cli::style_hyperlink(text = "news", github_news))
  }

  cli::cli_h2("{package}")
  cli::cli_bullets(c("{.href [cran]({cran_home})}", ""))
  cli::cli_text(c("No pkgdown found. ", "{.url {urls}}"))

  if (vignettes_show) {
    the_vignette(package, browser = FALSE)
  }

  invisible(urls)
}


the_vignette <- function(package, browser = TRUE) {
  check_string(package)

  if (!rlang::is_installed(package)) {
    return(invisible())
  }

  if (browser) {
    return(utils::browseVignettes(package = package))
  }

  vignettes <- tools::getVignetteInfo(package = package)[, "Topic"]

  if (rlang::has_length(vignettes, 0)) {
    return(invisible())
  }

  bullets <- paste0("{.vignette ", package, "::", vignettes, "}")
  cli::cli_h2("Vignettes")
  cli::cli_bullets(bullets)
}

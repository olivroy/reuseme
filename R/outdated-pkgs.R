#' Looks for outdated packages
#'
#' @description
#' Only checks for binaries, which has the advantage of not notifying you when
#' packages are newly available on CRAN, but without the binaries available.
#' Installing packages from source can be longer, hence the advantage of
#' waiting a few days for binaries.
#'
#' It takes advantage of pak's capacities to allow you to install packages on
#' Windows without restarting session.
#' @return A list of packages that can be updated, with links to news, the pkgdown site.
#' @export
#'
#' @examples
#' \donttest{
#' outdated_pkgs()
#' }
outdated_pkgs <- function() {
  if (rlang::is_installed("curl")) {
    default_repo <- getOption("repos")[[1]]
    default_repo <- stringr::str_remove(default_repo, "/$")
    default_repo <- stringr::str_remove(default_repo, "https://")

    access_repo <- tryCatch(curl::nslookup(default_repo),
      error = function(e) {
        FALSE
      }
    )
  } else {
    access_repo <- TRUE
  }


  if (isFALSE(access_repo)) {
    cli::cli_warn(c(
      "Couldn't check for outdated packages.",
      "Can't connect to your default repository.",
      "Are you offline?"
    ))
    return(invisible())
  }

  # similar to install.packages default
  outdated_pkg_mat <- utils::old.packages(type = "binary", lib.loc = .libPaths()[1])

  if (rlang::has_length(outdated_pkg_mat, 0)) {
    cli::cli_alert_success("All packages are up to date.")
    return(invisible())
  }

  outdated_pkg <-
    as.data.frame(outdated_pkg_mat) %>%
    purrr::list_transpose(.names = rownames(outdated_pkg_mat)) %>%
    purrr::imap(function(x, pkg_name) {
      withr::local_options(usethis.quiet = TRUE)
      url <- browse_pkg(pkg_name, open = FALSE, news_only = TRUE)
      list(url = url, ReposVer = x$ReposVer, Installed = x$Installed)
    })

  if (rlang::has_name(outdated_pkg, "pak")) {
    cli::cli_alert_success("There is a new version of pak.")
    cli::cli_alert_info("Update pak with {.run pak::pak_update()}")
    cli::cli_alert_info("Restart R session then run `outdated_pkgs()` again.")
    return(invisible())
  }
  withr::local_options(list(usethis.quiet = TRUE))
  # Nice to have use column output
  pkgs <- purrr::iwalk(
    outdated_pkg,
    .f = function(x, pkg) cli::cli_bullets("{.pkg {pkg}} ({x$Installed} -> {x$ReposVer}), {x$url}, {.run [install](pak::pak('{pkg}'))}, {.run [cran](usethis::browse_cran('{pkg}'))}.")
  )

  pkgs <- names(pkgs)
  pkgs <- deparse(pkgs)
  pkgs <- paste0(pkgs, collapse = "\n")

  cli::cli_bullets(c("Update all with", "pak::pak({pkgs})}"))
}

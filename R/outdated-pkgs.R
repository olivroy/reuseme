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
#' @param type Type of package to look for (`"binary"` or `"source"`)
#' @return A list of packages that can be updated, with links to news, the pkgdown site.
#' @export
#'
#' @examples
#' \donttest{
#' outdated_pkgs()
#' }
outdated_pkgs <- function(type = c("binary", "source")) {
  type <- rlang::arg_match(type)
  if (rlang::is_installed("curl")) {
    default_repo <- getOption("repos")[[1]]
    default_repo <- sub("/$", "", default_repo)
    default_repo <- sub("https://", "", default_repo, fixed = TRUE)

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
      "Could not check for outdated packages.",
      "Can't connect to your default repository.",
      "Are you offline?"
    ))
    return(invisible())
  }

  # similar to install.packages default
  outdated_pkg_mat <- utils::old.packages(type = type, lib.loc = .libPaths()[1])

  if (rlang::has_length(outdated_pkg_mat, 0)) {
    cli::cli_alert_success("All packages are up to date.")
    return(invisible())
  }

  fields_names <- colnames(outdated_pkg_mat)

  # Each element is a vector that contains installed version, package name
  # new version
  outdated_pkg <-
    t(outdated_pkg_mat) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    as.list() |>
    purrr::map(function(x) purrr::set_names(x, fields_names))

  if (!is.null(getOption("reuseme.ignore_update"))) {
    outdated_pkg <- outdated_pkg[!getOption("reuseme.ignore_update")]
  }
  # Stop early for pak update before
  if (rlang::has_name(outdated_pkg, "pak")) {
    cli::cli_alert_success("There is a new version of pak.")
    cli::cli_alert_info("Update pak with {.run pak::pak_update()}")
    cli::cli_alert_info("Restart R session then run `outdated_pkgs()` again.")
    return(invisible())
  }

  # Create the bullets of outdated packages, with version number
  # clickable hyperlinks to install or view news.
  outdated_pkg <-
    purrr::map(outdated_pkg, function(x) {
      withr::local_options(usethis.quiet = TRUE)
      url <- browse_pkg(x[["Package"]], open = FALSE, news_only = TRUE)

      # To use for padding
      n_char <- nchar(paste0(x[["Package"]], x[["Installed"]], x[["ReposVer"]]))

      list(
        url       = url,
        ReposVer  = x[["ReposVer"]],
        Installed = x[["Installed"]],
        n_char    = n_char
      )
    })

  withr::local_options(list(usethis.quiet = TRUE))

  # TODO figure out pad :)
  # pad <- max(purrr::map_int(outdated_pkg, "n_char"))
  # Nice to have use column output
  pkgs <- purrr::iwalk(
    outdated_pkg,
    .f = function(x, pkg) {
      cli::cli_bullets("{.pkg {pkg}} ({x$Installed} {cli::symbol$arrow_right} \\
                       {x$ReposVer}) {x$url}, \\
                       {.run [install](pak::pak('{pkg}'))}, \\
                       {.run [cran](usethis::browse_cran('{pkg}'))}")
    }
  )

  pkgs <- names(pkgs)
  pkgs <- deparse(pkgs)
  pkgs <- paste0(pkgs, collapse = "\n")
  if (rlang::is_installed("pak")) {
    fn_install <- "pak::pak"
  } else {
    fn_install <- "utils::install.packages"
  }
  cli::cli_bullets(c(
    "Update all with", "{fn_install}({pkgs})"))
  invisible(names(outdated_pkg))
}

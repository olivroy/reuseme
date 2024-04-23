#' Interact with different RStudio projects
#'
#' The package offers many ways to interact with different local RStudio projects.
#'
#' # Setup
#'
#' To take advantage of this functionality, you first have to set `options(reuseme.reposdir)` in
#' your .Rprofile file. Access it with [usethis::edit_r_profile()].
#'
#' I would recommend you add the following. It works better if you store your RStudio
#' projects in common directories.
#'
#' Inspired by [usethis options][usethis::usethis_options]
#'
#' ```
#' if (interactive()) {
#'   options(reuseme.reposdir = c("~/rrr/", "~/packages", "~/rrr-work/"))
#'
#' }
#' ```
#'
#' # Capabilities.
#'
#' Assumes that you have a project named `"learning"`
#' A project outline
#'
#' ```
#' proj_outline(proj = "learning)
#' ```
#'
#' Add a TODO item to the `learning` project
#'
#' ```
#' use_todo("learning::Learn this")
#' ```
#'
#' Get file [outline][proj_outline()] of the `file.R` in "learning"
#'
#' ```
#' proj_file("file", "learning")
#' ```
#'
#' Move to a new project in the same session
#'
#' ```
#' proj_switch("learning")
#' ```
#'
#' A lot of these features are already present in RStudio and with usethis.
#' However, when managing many projects, the recent projects list can be more difficult
#' to handle.
#' Passing the full project name to `usethis::proj_activate()` was too long.
#'
#' @name proj-reuseme
#' @family project management helpers
NULL

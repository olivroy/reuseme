#' Add a TODO list by project to a TODO.R file in the base directory
#'
#' Project, By default it will write in the current RStudio project.
#'
#' @param todo A character vector of lines to add to the TODO file
#' @param proj By default, the active project, an arbitrary directory, or a RStudio project name in the
#'   following directories `options(reuseme.destdir)`, uses [proj_list()] in this case.
#' @param code If `TRUE`, will render code output
#' @seealso [usethis::write_union()]
#' @return A `TODO.R` file appended with the `todo` string.
#' @export
#'
#' @examples
#' \dontrun{
#' use_todo("I need to do that")
#' use_todo(c("I need to do that again", "youppi"))
#' use_todo("c(x, y)", code = TRUE)
#' }
use_todo <- function(todo, proj = proj_get(), code = FALSE) {
  is_active_proj <- identical(proj, proj_get())
  check_character(todo)

  if (!code) {
    todo_lines <- paste("# TODO", todo)
  } else {
    todo_lines <- todo
  }
  path_todo <- "TODO.R"
  if (!fs::dir_exists(proj)) { # when referring to a project by name.
    all_projects <- proj_list()
    rlang::arg_match0(proj, values = names(all_projects))
    proj_path <- all_projects[proj]
  } else {
    proj_path <- proj
  }

  full_path_todo <- if (is_active_proj) path_todo else fs::path(proj_path, path_todo)

  # TODO nice to have, but would need to extract duplicates (ideally changes in usethis)
  # Change the default write_union message.
  write_union2(full_path_todo, lines = todo_lines, quiet = FALSE)
}

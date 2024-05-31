# This file is for roxygen comments parsing
## Use {.file tests/testthat/_outline/roxy-general2.R} for output testing -----------

## Complete block for exported function with headings --------------------------
# Mix md and `@section`
#' A title to be included
#'
#' A description not to be included
#'
#' ## A second-level heading in description to be included?
#'
#' # A detail first level-heading to be included
#'
#' Content not to be included
#'
#' ## A detail second-level heading to be included
#'
#' Content not to be included2
#'
#' @section `First code` to be included:
#'
#' Content not to be included
#'
#' @examples
#' # Commented code not included
#'
#' ggplot2::ggplot(mtcars) +
#'   labs(
#'     title = "A title not to be included"
#'   )
#' @export
#' @family a family to include
f_to_be_index_in_outline <- function() {

}

# block not to index -----------------------------------------------------------
#' A title not to be included (internal function)
#'
#' An internal description not to include
#'
#' # Internal heading not to be included
#'
#' content
#' @keywords internal
#' @export
f_not_to_index <- function() {

}
# Topic to index -----------------------------------

#' A title to be included
#'
#' A description not to be included
#'
#' ## A second-level heading in description to be included?
#'
#' # A detail first level-heading to be included
#'
#' Content not to be included
#'
#' ## A detail second-level heading to be included
#'
#' Content not to be included2
#'
#' # First to be included
#'
#' Content not to be included
#'
#' @examples
#' # Commented code not included
#'
#' ggplot2::ggplot(mtcars) +
#'   labs(
#'     title = "A title not to be included"
#'   )
#' @name topic-name-to-include
#' @family a family to include
NULL

#' Opens a RStudio project in a new session
#' @description
#' If not specified, will generate hyperlinks that call [usethis::proj_activate()].
#' `proj_switch()` looks at `options(reuseme.reposdir)`.
#'
#' ## second-level heading in desc
#'
#' content
#'
#' # Details + 2nd level heading
#' content
#'
#' ## second heading
#'
#' Content
#'
NULL
# data to index ----------------------------------------------------------------

# I think I'd want the outline to show as "outline": title (but Ctrl + . does a good job for this) (maybe document this)

#' My data
#'
#' Another dataset
"dataset"

# Keep this line last: content to test for edge cases should be put in {.file tests/testthat/_outline/roxy-general2.R}

## multiple tags + name parsing issue --------------------------
# Mix md and `@section`
#' A title to be included
#'
#' A description not to be included
#'
#' @section a section:
#'
#'
#' @section another section:
#'
#'  A second-level heading in description to be included?
#'
#' # A detail first level-heading to be included
#' @name xxx
NULL
#' @section another sectio2n:
#'
#'  A second-level heading in description to be included?
#'
#' # A detail first level-heading to be included
#' @name yyy
NULL


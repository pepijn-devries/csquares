#' Test if a csquares object is valid
#' 
#' Tests if a `csquares` object is correctly specified and can be translated into valid coordinates
#' @param x An object of class `csquares` to be evaluated.
#' @returns Returns a `logical` value indicating whether the `csquares` object is valid or not.
#' @examples
#' validate_csquares(
#'   as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1")
#' )
#' @include helpers.R
#' @author Pepijn de Vries
#' @export
validate_csquares <- function(x) {
  if (!inherits(x, "csquares"))
    rlang::abort(c(
      x = "'x' is not of class 'csquares'",
      i = "Create a csquares object first with 'new_csquare' or 'as_csquares'"
    ))
  x <- if(inherits(x, "character")) {
    x
  } else {
    x[[attributes(x)$csquares_col]]
  }
  check <- .csquares_to_coords(x)
  all(check$check1 & check$check2 & check$check3 & check$check4)
}
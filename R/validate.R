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
  x <- if(typeof(x) == "character" || inherits(x, c("character", "vctrs_vctr"))) {
    x
  } else {
    x[[attributes(x)$csquares_col]]
  }
  if (length(x[!is.na(x)]) == 0) return(TRUE)
  strsplit(x[!is.na(x)], "[|]") |>
    unlist() |>
    .check_csquare_validity() |>
    all()
}
#' Drop c-square information from object
#' 
#' Drops c-square data from an object, but keeps the parent class of the object
#' intact. You cannot deselect the csquare column from a `csquares` object as this
#' will render the object invalid. Use `drop_csquares` instead.
#' @param x An object of class `csquares` from which the c-square information
#' needs to be dropped.
#' @param ... ignored
#' @returns Returns a copy of `x` inheriting its parent classes but with out
#' csquares info.
#' @examples
#' csq <- as_csquares("1000")
#' drop_csquares(csq)
#' 
#' csq <-
#'   data.frame(csquares = "1000", foo = "bar") |>
#'   as_csquares(csquares = "csquares")
#' 
#' drop_csquares(csq)
#' @author Pepijn de Vries
#' @export
drop_csquares <- function(x, ...) {
  class(x) <- setdiff(class(x), "csquares")
  .by <- attributes(x)$csquares_col
  attributes(x)$csquare_col <- NULL
  if (length(.by) > 0)
    x <- dplyr::select(x, !dplyr::any_of(.by))
  return(x)
}
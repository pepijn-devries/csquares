#' Basic csquares methods
#' 
#' Basic methods for `csquares` objects for formatting and printing the objects
#' 
#' @param x A `csquares` object to be handled by the s3 methods
#' @param ... Ignored
#' @returns Returns (a formatted version of) x
#' @export format.csquares
#' @rdname csquare-methods
#' @export
format.csquares <- function(x, ...) {
  if (inherits(x, "character")) x else NextMethod("format")
}

#' @rdname csquare-methods
#' @export show
show <- function(x, ...){
  UseMethod("show")
}

#' @rdname csquare-methods
#' @export show.csquares
#' @export
show.csquares <- function(x, ...) {
  format.csquares(x, ...)
}

#' @rdname csquare-methods
#' @export
print.csquares <- function(x, ...) {
  NextMethod("print")
  # if (inherits(x, "character")) {
  #   cat(paste(format.csquares(x[seq_len(getOption("max.print"))], ...), collapse = "\n"))
  # } else NextMethod("print")
}

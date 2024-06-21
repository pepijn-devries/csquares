#' @export
as_csquares_code <- function(x) {
  if (is.character(x)) {
    ## TODO check validity
    class(x) <- "csquares_code"
    x
  }
}

#' @export format.csquares_code
#' @export
format.csquares_code <- function(x, ...) {
  "hoi"
}

#' @export show
show <- function(x, ...){
  UseMethod("show")
}

#' @export show.csquares_code
#' @export
show.csquares_code <- function(x, ...) {
  format.csquares_code(x, ...)
}

#' @export
print.csquares_code <- function(x, ...) {
  cat(paste(format.csquares_code(x[seq_len(getOption("max.print"))], ...), collapse = "\n"))
}

#' Basic csquares methods
#' 
#' Basic S3 methods for `csquares` objects for formatting and printing the objects
#' 
#' @param x,object A `csquares` object to be handled by the s3 methods
#' @param ... Ignored
#' @returns Returns (a formatted version of) x
#' @export format.csquares
#' @rdname csquare-methods
#' @include helpers.R
#' @export
format.csquares <- function(x, ...) {
  if (inherits(x, c("character", "vctrs_vctr"))) {
    dplyr::tibble(codes = strsplit(x, "[|]")) |>
      dplyr::mutate(rown  = dplyr::row_number()) |>
      tidyr::unnest("codes") |>
      dplyr::group_by(.data$rown) |>
      dplyr::summarise(
        n      = dplyr::n(),
        quad   = ifelse(.data$n > 1, NA, .get_quadrant(.data$codes)),
        res    = ifelse(.data$n > 1, NA, .nchar_to_csq_res(.data$codes)),
        format = if(.data$n > 1) {
          paste(format(.data$n, width = 3), "squares")
        } else {
          paste0(substr(.data$quad, 1, 1),
                 format(
                   as.numeric(substr(.data$codes, 2, 2))*10,
                   width = 2),
                 ", ",
                 substr(.data$quad, 2, 2),
                 format(
                   as.numeric(substr(.data$codes, 3, 4))*10,
                   width = 3), " (", .data$res, "\u00B0)")
        }
      ) |>
      dplyr::pull("format")
  } else NextMethod()
}

show.csquares <- function(x, ...) {
  format.csquares(x, ...)
}

#' @rdname csquare-methods
#' @export
print.csquares <- function(x, ...) {
  if (inherits(x, c("character", "vctrs_vctr"))) {
    cat(paste(format.csquares(x[seq_len(min(length(x), getOption("max.print")))], ...),
              collapse = "\n"))
  } else NextMethod()
}

#' @rdname csquare-methods
#' @export
as.character.csquares <- function(x, ...) {
  if (inherits(x, c("character", "vctrs_vctr"))) {
    unclass(x)
  } else NextMethod()
}

#' @rdname csquare-methods
#' @export
summary.csquares <- function(object, ...) {
  if (inherits(object, c("character", "vctrs_vctr")))
    summary(unclass(object)) else NextMethod()
}

#' @rdname csquare-methods
#' @export
as.data.frame.csquares <- function(x, ...) {
  if (inherits(x, "character")) {
    x <- data.frame(csquares = unclass(x))
    class(x$csquares) <- union(c("csquares", "vctrs_vctr"), class(x$csquares))
    .by <- "csquares"
  } else {
    .by = attributes(x)$csquares_col
    class(x) <- setdiff(class(x), "csquares")
    x <- as.data.frame(x)
  }
  attributes(x)$csquares_col <- .by
  class(x) <- union("csquares", class(x))
  x
}

#' Basic csquares methods
#' 
#' Basic S3 methods for handling `csquares` objects
#' 
#' @param x,object A `csquares` object to be handled by the s3 methods
#' @param y A `data.frame` to be merged with `x`
#' @inheritParams base::rbind
#' @param i,j,name Indices/name for selecting subsets of `x`
#' @param drop `logical` value indicating if unused dimensions should be dropped
#' @param value Replacement values for a subset. a `csquares` object or a `character` string that can be coerced
#' to a `csquares` object
#' @param short `logical` option to print `csquares` `vctrs_vec`. If `TRUE` it will only print one line, if
#' `FALSE` it will print up to `options("max.print")` records.
#' @param ... Passed on to generic methods
#' @returns Returns (a subsetted / formatted / modified version of) x
#' @export format.csquares
#' @rdname csquares-methods
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
          ifelse(
            is.na(.data$codes),
            "empty",
            paste0(substr(.data$quad, 1, 1),
                   format(
                     as.numeric(substr(.data$codes, 2, 2))*10,
                     width = 2),
                   " ",
                   substr(.data$quad, 2, 2),
                   format(
                     as.numeric(substr(.data$codes, 3, 4))*10,
                     width = 3), " (", .data$res, "\u00B0)")
          )
        }
      ) |>
      dplyr::pull("format")
  } else NextMethod()
}

#' @rdname csquares-methods
#' @export
print.csquares <- function(x, short = TRUE, ...) {
  if (inherits(x, c("character", "vctrs_vctr"))) {
    if (short) {
      vctrs::obj_str(x)
    } else vctrs::obj_print(x)
  } else NextMethod()
}

#' @rdname csquares-methods
#' @export
as.character.csquares <- function(x, ...) {
  if (inherits(x, c("character", "vctrs_vctr"))) {
    unclass(x)
  } else NextMethod()
}

#' @rdname csquares-methods
#' @export
summary.csquares <- function(
    object, ...) {
  if (inherits(object, c("character", "vctrs_vctr")))
    summary(unclass(object)) else NextMethod()
}

#' @rdname csquares-methods
#' @export
as.data.frame.csquares <- function(x, ...) {
  if (inherits(x, "character")) {
    x <- unclass(x)
    x <- NextMethod()
    class(x[,1]) <- union(c("csquares", "vctrs_vctr"), class(x[,1]))
    if (is.null(names(x))) {
      .by <- 1
    } else {
      names(x)[[1]] <- "csquares"
      .by <- "csquares"
    }

  } else if (inherits(x, "data.frame")) {
    return(x)
  } else {
    .by <- attributes(x)$csquares_col
    class(x) <- setdiff(class(x), "csquares")
    x <- as.data.frame(x, ...)
  }
  .s3_finalise(x, .by)
}

#' @rdname csquares-methods
#' @export
c.csquares <- function(...) {
  .no_stars(list(...)[[1]], "c")
  if (.all_of_class(..., my_class = "character")) {
    if (.all_of_class(..., my_class = "csquares")) NextMethod() else {
      elements <- lapply(list(...), as_csquares)
      do.call(c, elements)
    }
  } else {
    elements <- lapply(list(...), as_csquares)
    do.call(rbind, elements)
  }
}

#' @rdname csquares-methods
#' @export
rbind.csquares <- function(..., deparse.level = 1) {
  .no_stars_or_char(list(...)[[1]], "rbind")
  .by <- attributes(list(...)[[1]])$csquares_col
  result <- lapply(list(...), \(x) {
    if (!is.null(x)) {
      if (!inherits(x, "csquares")) x <- as_csquares(x, use_centroids = FALSE)
      attributes(x)$csquares_col <- NULL
      class(x) <- setdiff(class(x), "csquares")
    }
    x
  })
  result <- do.call(rbind, result)
  .s3_finalise(result, .by)
}

#' @rdname csquares-methods
#' @export
cbind.csquares <- function(..., deparse.level = 1) {
  .no_stars_or_char(list(...)[[1]], "cbind")
  .by <- attributes(list(...)[[1]])$csquares_col
  idx <- which(.by == names(list(...)[[1]]))
  result <- do.call(dplyr::bind_cols, list(...))
  .by <- names(result)[[idx]]

  result <- lapply(list(...), \(x) {
    if (!is.null(x)) {
      attributes(x)$csquares_col <- NULL
      class(x) <- setdiff(class(x), "csquares")
    }
    x
  })
  result <- do.call(cbind, result)
  .s3_finalise(result, .by)
}

#' @rdname csquares-methods
#' @export
`[.csquares` <- function(x, i, j, ..., drop = FALSE) {
  .by <- .s3_df_stars_prep(x, "[", allow_all_types = TRUE)
  class(x) <- setdiff(class(x), "csquares")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname csquares-methods
#' @export
`[[.csquares` <- function(x, i) {
  NextMethod()
}

#' @rdname csquares-methods
#' @export
`$.csquares` <- function(x, name) {
  NextMethod()
}

#' @rdname csquares-methods
#' @export
`[<-.csquares` <- function(x, i, j, value) {
  .by <- .s3_df_stars_prep(x, "[<-", allow_all_types = TRUE)
  class(x) <- setdiff(class(x), "csquares")
  if (!inherits(x, "data.frame"))
    class(value) <- setdiff(class(x), "csquares")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname csquares-methods
#' @export
`[[<-.csquares` <- function(x, i, value) {
  .by <- .s3_df_stars_prep(x, "[[<-", allow_all_types = TRUE)
  class(x) <- setdiff(class(x), "csquares")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname csquares-methods
#' @export
`$<-.csquares` <- function(x, i, value) {
  .by <- .s3_df_stars_prep(x, "$<-", allow_all_types = TRUE)
  class(x) <- setdiff(class(x), "csquares")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname csquares-methods
#' @export
merge.csquares <- function(x, y, ...) {
  .by <- .s3_df_stars_prep(x, "merge", allow_all_types = TRUE)
  class(x) <- setdiff(class(x), "csquares")
  attributes(x)$csquares_col <- NULL
  .s3_finalise(NextMethod(), .by)
}

#' @rdname csquares-methods
#' @export
`names<-.csquares` <- function(x, value) {
  .by <- .s3_df_stars_prep(x, "merge", allow_all_types = TRUE)
  .by <- value[names(x) == .by]
  class(x) <- setdiff(class(x), "csquares")
  .s3_finalise(NextMethod(), .by)
}

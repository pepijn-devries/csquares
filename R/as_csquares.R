#' Convert lon-lat coordinates into c-square codes
#' 
#' Takes WGS84 longitude and latitude coordinates and finds the closest
#' matching c-squares for a given resolution.
#'
#' @param x An object to be coerced to a `csquares` object. `x` can be a vector
#' of `character` strings representing c-squares code. It can also be a
#' `numeric` `matrix` with two columns containing the x and y coordinates. `x`
#' can also be a simple features object ([`sf`][sf::st_as_sf]) or a spatial
#' arrays object ([`stars`][stars::st_as_stars]).
#' @param resolution Resolution (in WGS84 degrees) to be used for creating
#' c-squares codes. As per c-square specifications, the resolution should be
#' 10 or less, yet greater than 0. It should be a tenfold of 1 or 5. Valid
#' resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.
#' @param csquares If `x` is not a vector of `character` strings (but for
#' instance a `data.frame`), the `csquares` argument should specify the name
#' of the element of `x` containing the c-square codes as `character` strings.
#' @param validate A `logical` value indicating whether the created object
#' needs to be validated. Defaults to `TRUE`. Validation can be time-consuming
#' so set to `FALSE` to save computing time.
#' @param ... Currently ignored
#' @param use_centroids In case `x` is a simple features object and
#' `use_centroids` is `TRUE`, the centroid of each geometry is used for
#' deriving c-squares. If it is `FALSE` all coordinates in the geometry are
#' used.
#' @returns Returns a `csquares` object that contains c-squares codes.
#' @examples
#' as_csquares(cbind(x = 5.2399066, y = 52.7155812), resolution = 1)
#' pts <-
#'   expand.grid(
#'     x = seq(-0.1, 0.1, by = 0.01),
#'     y = seq(-0.1, 0.1, by = 0.01)
#'   ) |>
#'   sf::st_as_sf(coords = c("x", "y"), crs = 4326)
#' as_csquares(pts, 0.05)
#' orca_csq <- as_csquares(orca, csquares = "csquares")
#' @include helpers.R
#' @author Pepijn de Vries
#' @rdname as_csquares
#' @name as_csquares
#' @family csquares-class
#' @export
as_csquares <- function(x, resolution, csquares, ...) {
  UseMethod("as_csquares", x)
}

#' @rdname as_csquares
#' @name as_csquares
#' @export
as_csquares.default <- function(x, resolution, csquares, ...) {
  rlang::abort(
    paste("No as_csquare method available for objects of class", class(x)[1])
  )
}

#' @rdname as_csquares
#' @name as_csquares
#' @export
as_csquares.character <- function(
    x, resolution, csquares, validate = TRUE, ...) {
  x <- vctrs::new_vctr(x, class = c("csquares", "character"))
  if (validate && length(x) > 0) {
    check <- tryCatch({
      has_wildcards <- grepl("*", x, fixed = TRUE)
      if (any( has_wildcards )) {
        x[has_wildcards] <- expand_wildcards(x[has_wildcards] |> unclass())
      }
      validate_csquares(x)
    }, error = function(e) FALSE)
    if (!check)
      rlang::abort(c(
        x = "Could not translate coordinates",
        i = "Check the formating of your c-squares codes"
      ))
  }
  x
}

#' @rdname as_csquares
#' @export
as_csquares.numeric <- function(x, resolution = 1, csquares, ...) {
  if (inherits(x, "matrix")) {
    as_csquares(data.frame(x = x[,1], y = x[,2]), resolution)
  } else {
    rlang::abort(
      c(
        x = paste("Numeric values can only be converted to c-squares in a two",
                  "column matrix form"),
        i = paste("Make sure your numbers represent two columns (x and y",
                  "coordinates) in a matrix")
      )
    )
  }
}

#' @rdname as_csquares
#' @export
as_csquares.data.frame <- function(x, resolution = 1, csquares, ...) {
  if (missing(csquares)) {
    if (inherits(x, "csquares")) return(x)
    resolution <- .check_resolution(resolution)
    
    dat <- .csquares_generic(x, resolution)
    csquares <- make.names(c(colnames(x), "csquares"), unique = TRUE)
    csquares <- csquares[[length(csquares)]]
    x[[csquares]] <- dat
    
  } else {
    x[[csquares]] <- as_csquares(x[[csquares]], ...)
  }
  
  .s3_finalise(x, csquares)
}

#' @rdname as_csquares
#' @export
as_csquares.sf <- function(
    x, resolution = 1, csquares, ..., use_centroids = TRUE) {
  if (inherits(x, "csquares")) return(x)
  if (use_centroids) x <- sf::st_centroid(x)
  .csquares_spatial(x, resolution)
}

#' @rdname as_csquares
#' @export
as_csquares.sfc <- function(
    x, resolution = 1, csquares, ..., use_centroids = TRUE) {
  if (use_centroids) x <- sf::st_centroid(x)
  .csquares_spatial(x, resolution)
}

#' @rdname as_csquares
#' @export
as_csquares.stars <- function(x, resolution = 1, csquares, ...) {
  if (!missing(csquares)) {
    rlang::warn("Specified csquares column is ignored for stars object x")
  }
  .csquares_spatial(x, resolution)
}

.csquares_spatial <- function(x, resolution) {
  resolution <- .check_resolution(resolution)
  x <- drop_csquares(x)
  csq <-
    x |>
    sf::st_transform(4326) |>
    sf::st_coordinates() |>
    .csquares_generic(resolution)
  if (inherits(x, "sfc")) return (csq)
  nms <- make.names(c(names(x), "csquares"), unique = TRUE)
  nms <- nms[[length(nms)]]
  x[[nms]] <- csq
  .s3_finalise(x, nms)
}

.csquares_generic <- function(x, resolution) {
  l10 <- as.integer(attributes(resolution)$l10)
  l5  <- as.integer(attributes(resolution)$l5)
  res <- as.numeric(resolution)
  
  # Exact power-of-10 scale factor (no 32-bit integer overflow; doubles
  # are exact up to 2^53)
  dec_places <- max(0L, l10)
  scale      <- 10^dec_places
  res_int    <- round(res * scale)
  
  x <-
    x |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    dplyr::rename(x = 1, y = 2)
  
  if (!"L3" %in% colnames(x)) {
    x <-
      x |>
      dplyr::mutate(
        L3 = dplyr::row_number()
      )
  }
  
  x <-
    x |>
    dplyr::mutate(
      quadrant = (0L + 4L * (.data$x < 0)) +
        (2L + 2L * xor(.data$x >= 0, .data$y >= 0)) - 1L,
      # Convert coordinates to exact scaled integer-valued doubles
      xi = trunc(signif(abs(.data$x), 15) * scale),
      yi = trunc(signif(abs(.data$y), 15) * scale),
      # Clamp +/-180 and +/-90 inward by 1 scaled unit
      xi = ifelse(.data$xi >= 180 * scale, 180 * scale - 1, .data$xi),
      yi = ifelse(.data$yi >=  90 * scale,  90 * scale - 1, .data$yi),
      # Snap to exact lower-limit cell in scaled integer units
      xi = (.data$xi %/% res_int) * res_int,
      yi = (.data$yi %/% res_int) * res_int,
      # Extract 10-degree block indices via exact integer division
      x_cur = .data$xi %/% (10 * scale),
      y_cur = .data$yi %/% (10 * scale),
      digits = {
        array(
          rep(seq_len(l10 + 1L), each = length(.data$xi)),
          dim = c(length(.data$xi), l10 + 1L, 2L)
        ) |>
          apply(2, function(idx, x1, y1) {
            k          <- idx[1L]
            last_digit <- k == (l10 + 1L)
            div        <- 10^(dec_places - (k - 1L))
            vals       <- (cbind(y1, x1) %/% div) %% 10
            vals       <- cbind(.digit_check(vals), vals)
            vals[last_digit & l5 == 1L, 2:3] <- ""
            apply(vals, 1, paste0, collapse = "")
          }, x1 = .data$xi, y1 = .data$yi) |>
          (\(m) if (is.null(dim(m))) {
            paste0(m[m != ""], collapse = ":")
          } else {
            apply(m, 1, paste0, collapse = ":")
          })()
      },
      csquares = cbind(
        sprintf("%1i%1i%02i", .data$quadrant, .data$y_cur, .data$x_cur),
        .data$digits
      ) |>
        apply(1, \(r) paste0(r[r != ""], collapse = ":"))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("L3"))) |>
    dplyr::summarise(csquares = paste0(.data$csquares, collapse = "|")) |>
    dplyr::pull("csquares") |>
    c()
  
  vctrs::new_vctr(x, class = c("csquares", "character"))
}

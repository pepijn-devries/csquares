#' Create a simple features object from c-squares
#' 
#' Converts a `character` string of c-squares in a spatially explicit simple features object
#' ([`sf`][sf::st_sf]. It can also convert `data.frame`s with a column of c-squares codes to
#' an [`sf`][sf::st_sf] object.
#' @param x A `vector` of `character` strings. Each element should hold a valid
#' c-square code. `x` can also be a `data.frame` with a column of c-square codes.
#' (Note that wildcard characters are not supported)
#' @param use_geometry If `use_geometry` is `TRUE` and `x` inherits a spatial feature,
#' its geometry will be used to cast the object. This is much faster than its alternative
#' when `use_geometry` is `FALSE`. In the latter case, the c-square codes are first translated
#' into explicit spatial information. The latter is more reliable as it does not rely on
#' the assumption that the geometry of `x` corresponds with the csquares codes in the object.
#' In short: use `TRUE` for speed, use `FALSE` for reliability.
#' @param ... Ignored
#' @returns In case of `st_as_sfc.csquares` a list of geometries ([`sfc`][sf::st_sfc],
#' (MULTI)POLYGONS) is returned. In case of `st_as_sf.csquares` an object of class
#' ([`sf`][sf::st_sf]) is returned.
#' @examples
#' library(sf)
#' st_as_sfc(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
#' st_as_sf(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
#' @name st_as_sf
#' @rdname st_as_sf
#' @author Pepijn de Vries
#' @export
st_as_sf.csquares <- function(x, use_geometry = TRUE, ...) {
  is_spatial <- inherits(x, c("stars", "sf"))
  if (use_geometry && is_spatial) {
    result <- NextMethod()
  } else {
    if (is_spatial) {
      if (inherits(x, "sf")) {
        rlang::warn("Replacing existing geometry!")
        result <- sf::st_drop_geometry(x)
      }
      result <- dplyr::as_tibble(x)
    } else if (inherits(x, c("character", "vctrs_vctr"))) {
      .by <- "csquares"
      result <- dplyr::tibble(csquares = vctrs::new_vctr(x, class = "csquares"))    
    } else {
      result <- x
    }
    if (!inherits(x, c("character", "vctrs_vctr"))) {
      .by <- attributes(x)$csquares_col
      if (is.null(.by)) {
        rlang::warn("csquare column is not specified, assuming it is called 'csquares'")
        attributes(x)$csquares_col <- .by <- "csquares"
      }
    }
    class(result) <- setdiff(class(x), "csquares")
    result <-
      result |>
      dplyr::mutate(
        geom = st_as_sfc.csquares(.data[[.by]], ...)
      ) |>
      sf::st_as_sf(crs = 4326)
    
    attributes(result)$csquares_col <- .by
  }
  return(result)
}

#' @name st_as_sfc
#' @rdname st_as_sf
#' @export
st_as_sfc.csquares <- function(x, use_geometry = TRUE, ...) {
  if (use_geometry && inherits(x, c("sf", "stars"))) {
    result <- NextMethod()
    return(result)
  }
  x <- .csquares_to_coords(x)
  if (any(!(x$check1 & x$check2 & x$check3 & x$check4)))
    rlang::warn("Malformed csquares, introduced empty geometries.")
  x |>
    dplyr::pull("geom")
}
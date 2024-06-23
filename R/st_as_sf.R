#' Create a simple features object from c-squares
#' 
#' Converts a `character` string of c-squares in a spatially explicit simple features object
#' ([`sf`][sf::st_sf]. It can also convert `data.frame`s with a column of c-squares codes to
#' an [`sf`][sf::st_sf] object.
#' @param x A `vector` of `character` strings. Each element should hold a valid
#' c-square code. `x` can also be a `data.frame` with a column of c-square codes.
#' (Note that wildcard characters are not supported)
#' @param csquares In case `x` is a `data.frame`, `csquare` should specify the column
#' name that holds the c-square codes.
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
st_as_sf.csquares <- function(x, csquares = "csquares") {
  if (inherits(x, "data.frame")) {
    if (inherits(x, "sf")) {
      rlang::warn("Replacing existing geometry!")
      result <- sf::st_drop_geometry(x)
    }
    result <- dplyr::as_tibble(x)
  } else {
    result <- dplyr::tibble(csquares = x)    
  }
  result |>
    dplyr::mutate(
      geom = st_as_sfc.csquares(.data[[csquares]])
    ) |>
    sf::st_as_sf(crs = 4326)
}

#' @name st_as_sfc
#' @rdname st_as_sf
#' @export
st_as_sfc.csquares <- function(x) {
  x <- .csquares_to_coords(x)
  if (any(!(x$check1 & x$check2 & x$check3 & x$check4)))
    rlang::warn("Malformed csquares, introduced empty geometries.")
  x |>
    dplyr::pull("geom")
}
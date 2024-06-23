#' Create a c-squares raster from a bounding box
#' 
#' Creates a spatial raster ([`stars`][stars::st_as_stars]) with c-square codes for a specified bounding box,
#' using a specified resolution. The raster will be conform c-squares specifications.
#' 
#' @param x An object of class [`bbox`][sf::st_bbox] or an object that can be coerced to a `bbox`.
#' It defines the bounding box for the c-squares grid created by this function.
#' @inheritParams as_csquares
#' @param crs The projection to be used for the created grid. By default it is WGS84 (EPSG:4326).
#' @returns Returns a [`stars`][stars::st_as_stars] and `csquares` object based on the provided bounding box and
#' resolution.
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' new_csquares(nc)
#' @include as_csquares.R helpers.R
#' @author Pepijn de Vries
#' @export
new_csquares <-
  function(x, resolution = 1, crs = 4326) {

    resolution <- .check_resolution(resolution)
    
    if (inherits(x, "stars"))
      if (requireNamespace("stars"))
        rlang::abort(c(x = "Could not load namespace 'stars'.",
                       i = "Install package 'stars' and try again."))
    
    crs_in <- tryCatch({sf::st_crs(x)}, error = function(e) NA)
    if (is.na(crs_in)) {
      rlang::warn("Object 'x' crs is unknown, assuming it is EPSG:4326.")
      crs_in <- sf::st_crs(4326)
    }
    
    x <-
      x |>
      sf::st_bbox(crs = crs_in) |>
      sf::st_as_sfc() |>
      sf::st_transform(crs = 4326) |>
      sf::st_bbox()
    
    x <-
      sf::st_bbox(
        c(
          xmin = resolution*round((x[["xmin"]] - 0.499999*resolution)/resolution),
          xmax = resolution*round((x[["xmax"]] + 0.499999*resolution)/resolution),
          ymin = resolution*round((x[["ymin"]] - 0.499999*resolution)/resolution),
          ymax = resolution*round((x[["ymax"]] + 0.499999*resolution)/resolution)
        ),
        crs = 4326)
    
    result <-
      x |>
      stars::st_as_stars(dx = resolution, dy = resolution)
    
    if (sf::st_crs(x)$input != "EPSG:4326") {
      result <-
        result |>
        sf::st_transform(crs = crs)
    }
    
    result[["values"]] <- NULL
    result[["csquares"]] <- as_csquares.stars(result, resolution = resolution)[["csquares"]]
    class(result) <- c("csquares", class(result))
    return (result)
  }

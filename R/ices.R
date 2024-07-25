#' Valid ICES rectangle columns
#' 
#' `r lifecycle::badge('experimental')` Get all valid column codes of ICES rectangles.
#' Note that ICES subrectangles are not compatible with csquares. For more details
#' see `vignette("ices")`.
#' @returns A `character` vector with all allowed codes for the columns in ICES
#' rectangles.
#' @examples
#' ices_columns()
#' @export
ices_columns <- function() {
  ices_columns <-
    expand.grid(
      letter = LETTERS[LETTERS != "I" & LETTERS <= "M"],
      number = 0:9
    ) |>
    apply(1, paste0, collapse = "") |>
    utils::head(-1)
  ices_columns <-
    ices_columns[ices_columns < "A4" | ices_columns > "A9"] |>
    sort()
  ices_columns
}

#' Get ICES geometries
#' 
#' `r lifecycle::badge('experimental')` Functions to convert ICES rectangles
#' @param ices_rect A `character` vector containing valid ICES rectangle codes
#' @param csquares A `csquares` object, or an object that can be coerced with
#' `as_csquares()`.
#' @returns In case of `ices_centroids` a `sf::st_sf()` object is returned, with
#' `POINT` geometries representing the centroids of the ICES rectangles.
#' 
#' In case of `ices_rectangles` a `sf::st_sf()` object is returned, with
#' `POLYGON` geometries representing the outline of the ICES rectangles.
#' 
#' In case of `ices_to_csquares` a `csquares` object inheriting from  `sf::st_sf()`
#' is returned, the csquares code should represent the ICES rectangles.
#' 
#' In case of `ices_from_csquares` a `character` vector is returned with
#' ICES rectangle codes that correspond with the csquares. The method is
#' fast yet crude: it only checks in which ICES rectangles the centroids of the
#' csquares are located. It does not check if the resolution matches. `NA` values
#' are returned when csquares are situated outside the area covered by ICES rectangles.
#' @examples
#' ices_rects <-
#'   c("31F21", "31F22", "31F23", "31F24", "31F25", "31F26", "31F27", "31F28", "31F29",
#'     "32F2", "33F2", "34F2", "35F2",
#'     "31F3", "32F3", "33F3", "34F3", "35F3",
#'     "31F4", "32F4", "33F4", "34F4", "35F4")
#' ices_centroids(ices_rects)
#' ices_rectangles(ices_rects)
#' ices_csq <- ices_to_csquares(ices_rects)
#' ices_from_csquares(ices_csq)
#' @author Pepijn de Vries
#' @rdname ices_geom
#' @export
ices_centroids <- function(ices_rect) {
  ices_columns <- ices_columns()
  ices_rect <- toupper(ices_rect)
  
  result <-
    data.frame(
      ICES = ices_rect,
      lon = -44.5 + match(stringr::str_sub(ices_rect, 3L, 4L), ices_columns),
      lat = 35.75 + as.integer(stringr::str_sub(ices_rect, 1L, 2L))/2,
      offset_x = as.integer(stringr::str_sub(ices_rect, 5L, 5L))
    ) |>
    dplyr::mutate(
      invalid  = !is.na(.data$offset_x) & .data$offset_x == 0,
      offset_y = ((9L - .data$offset_x)%%3L -1L)/6L,
      offset_x = floor((.data$offset_x -1L)/3L - 1L)/3L,
      subrect  = !is.na(.data$offset_x),
      offset_x = ifelse(.data$subrect, .data$offset_x, 0),
      offset_y = ifelse(.data$subrect, .data$offset_y, 0),
      lon      = .data$lon + .data$offset_x,
      lat      = .data$lat + .data$offset_y,
      invalid  = .data$invalid | is.na(.data$lon) | is.na(.data$lat)
    )
  if (any(result$invalid))
    rlang::abort(c(x = "One or more ICES codes are invalid",
                   i = "Check the ICES code format"))
  
  result |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    dplyr::select("ICES", "subrect", "geometry")
}

#' @rdname ices_geom
#' @export
ices_rectangles <- function(ices_rect) {
  poly <-
    ices_centroids(ices_rect)
  poly <-
    poly |>
    dplyr::bind_cols(sf::st_coordinates(poly)) |>
    dplyr::mutate(
      geometry =
        mapply(
          \(x, y, res) {
            arguments <- unlist(as.list(environment()))
            sf::st_polygon(
              list(
                cbind(x + res*c(-.5, .5, .5, -.5, -.5),
                      y + 0.5*res*c(-.5, -.5, .5, .5, -.5))
              )
            )
          }, x = .data$X, y = .data$Y, res = ifelse(.data$subrect, 1/3, 1), SIMPLIFY = FALSE
        ) |>
        sf::st_as_sfc()
    ) |>
    dplyr::select(!dplyr::any_of(c("X", "Y"))) |>
    sf::st_make_valid()
  sf::st_crs(poly) <- 4326
  poly
}

#' @rdname ices_geom
#' @export
ices_to_csquares <- function (ices_rect) {
  result <-
    ices_rect |>
    ices_centroids() |>
    dplyr::mutate(
      geom2    = sf::st_sfc(.data$geometry + c(0.25, 0), crs = 4326) |> suppressWarnings(),
      geometry = sf::st_sfc(.data$geometry + c(-0.25, 0), crs = 4326) |> suppressWarnings(),
      csquares = ifelse(
        .data$subrect,
        as_csquares(as.character(NA)),
        as_csquares(.data$geometry, 0.5, use_centroids = TRUE)),
      csquares2 = ifelse(
        .data$subrect,
        as_csquares(as.character(NA)),
        as_csquares(.data$geom2, 0.5, use_centroids = TRUE)),
      csquares = {
        csq <- .data$csquares
        csq[!.data$subrect] <-
          as_csquares(paste(.data$csquares[!.data$subrect],
                            .data$csquares2[!.data$subrect], sep = "|"), validate = FALSE)
        csq |> as_csquares(validate = FALSE)
      }
    ) |>
    sf::st_drop_geometry() |>
    dplyr::select("ICES", "subrect", "csquares") |>
    as_csquares(csquares = "csquares", validate = FALSE)
  if (any(result$subrect))
    rlang::warn("Input contains ICES subrectangles which are incompatible with csquares.")
  result
}

#' @rdname ices_geom
#' @export
ices_from_csquares <- function(csquares) {
  if (!inherits(csquares, "csquares")) csquares <- as_csquares(csquares)
  if (typeof(csquares) != "character")
    csquares <- csquares[[attributes(csquares)$csquares_col]]
  csquares <- unclass(csquares)
  idx <- !is.na(csquares)
  result <- rep(as.character(NA), length(idx))

  if (sum(idx) > 0) {
    geom <- dplyr::tibble(
      csq = strsplit(csquares[idx], "[|]")
    ) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      tidyr::unnest("csq") |>
      as_csquares(csquares = "csq", validate = FALSE) |>
      sf::st_as_sf()
    
    result[idx] <-
      geom |>
      dplyr::bind_cols(
        sf::st_centroid(geom) |>
          suppressWarnings() |>
          sf::st_coordinates() |>
          as.data.frame()
      ) |>
      dplyr::mutate(
        X = floor(.data$X) + 45,
        X = ifelse(dplyr::between(.data$X, 1L, 113L),
                   ices_columns()[.data$X], NA),
        Y = floor(.data$Y*2L - 71L),
        valid = .data$Y > 0 & .data$Y < 100 & !is.na(.data$X),
        ICES = paste0(.data$Y, .data$X)
      ) |>
      sf::st_drop_geometry() |>
      dplyr::group_by(.data$id) |>
      dplyr::summarise(
        ICES = unique(.data$ICES) |> sort() |> paste0(collapse = "|"),
        valid = all(.data$valid)
      ) |>
      dplyr::mutate(ICES = ifelse(.data$valid, .data$ICES, NA)) |>
      dplyr::pull("ICES")
  }
  result
}

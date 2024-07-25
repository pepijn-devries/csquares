#' Coerce csqaures object into a stars object
#' 
#' Take a `csquares` object created with [new_csquares] or [as_csquares] and
#' coerce it to a spatiotemporal array ([stars][stars::st_as_stars]).
#' 
#' @param x An object of class `csquares` created with [new_csquares] or [as_csquares]
#' @param ... ignored.
#' @returns Returns a spatiotemporal array ([stars][stars::st_as_stars]) object based on `x`.
#' @examples
#' library(stars)
#' st_as_stars(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
#' st_as_stars(as_csquares(orca, csquares = "csquares"))
#' @include as_csquares.R helpers.R
#' @author Pepijn de Vries
#' @export
st_as_stars.csquares <-
  function(x, ...) {
    if (inherits(x, c("character", "vctrs_vctr"))) {
      x <- strsplit(x, "[|]")
      if (length(x) != 1 && any((lapply(x, length) |> unlist()) > 1))
        rlang::abort(c(
          x = "Cannot convert csquares object with multiple csquares code per element into stars",
          i = "Make sure that csquares codes don't contain pipe character ('|')"
        ))
      x <- unlist(x)
      resolution <- .nchar_to_csq_res(x)
      if (any(resolution[[1]] != resolution))
        rlang::abort(c(
          x = "Cannot convert csquares object with csquares codes of variable resolutions",
          i = "Make sure that all csquares codes have the same number of characters"
        ))
      resolution <- resolution[[1]]
      result <-
        x |>
        as_csquares(validate = FALSE) |>
        sf::st_as_sf() |>
        new_csquares(resolution = resolution)
      class(result) <- union("csquares", class(result))
      return(result)
    } else if (inherits(x, "stars")) {
      return(x)
    } else if (inherits(x, "sf")) {
      rlang::abort(c(
        x = "csquares objects inheriting from 'sf' cannot be converted into a 'stars' object"
      ))
    } else if (inherits(x, "data.frame")) {
      .by <- attributes(x)$csquares_col
      new_cols <- x |> drop_csquares() |> colnames()
      grd <-
        stars::st_as_stars(x[[.by]]) |>
        dplyr::mutate(
          !!!stats::setNames(rep(NA, length(new_cols)), new_cols),
          dplyr::across(dplyr::any_of(new_cols), ~{
            x[[dplyr::cur_column()]] [
              match(.data[[.by]], .env$x[[.by]])
            ]
          })
        )
      class(grd) <- union("csquares", class(grd))
      attributes(grd)$csquares_col <- .by
      return(grd)
    }
    NextMethod()
  }

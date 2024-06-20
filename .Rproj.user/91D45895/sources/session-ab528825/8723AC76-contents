#' Summarise c-square data to a lower resolution
#' 
#' This function acts very much like [dplyr::summarise], but instead of using a column to group,
#' c-square codes are used to aggregate to a lower resolution and summarise the data to that lower
#' resolution.
#' 
#' @param x The object to be summarised. Can be a `data.frame`, [`sf`][sf::st_sf], or
#' [`stars`][stars::st_as_stars] object.
#' @param .by The column name that holds the c-squares codes that need to be aggregated.
#' @param tiers_down The number of tiers down from the current resolution to which you wish to
#' summarise. If the current resolution is 5x5 degrees, the tier down would be 10x10 degrees
#' (as is the case in the example below).
#' @inheritParams dplyr::summarise
#' @returns Returns the summarised object inheriting its class from `x`
#' @examples
#' orca |>
#'   st_as_sf.csquares("csquares") |>
#'   summarise.csquare(
#'     .by = "csquares",
#'     orcinus_orca = any(na.omit(.data$orcinus_orca)))
#' 
#' @author Pepijn de Vries
#' @export
summarise.csquare <- function(x, ..., .by, tiers_down = 1L) {
  if (any(grepl("[|]", x[[.by]]))) {
    rlang::abort(
      c(x = "Cannot handle records with multiple c-squares.",
        i = "Make sure the c-squares codes don't contain pipe characters ('|').")
    )
  }
  is_sf    <- inherits(x, "sf")
  is_stars <- inherits(x, "stars")

  if (is_stars) {
    if (!requireNamespace("stars"))
      rlang::abort(c(x = "Could not load namespace 'stars'.",
                     i = "Install package 'stars' and try again."))
  }
  
  crs   <- if(is_sf || is_stars) sf::st_crs(x) else sf::st_crs(4326)

  if (is_stars) x <- dplyr::as_tibble(x)

  x <-
    x |>
    dplyr::mutate(
      new_len = nchar(.data[[.by]]) - 2L*tiers_down,
      new_len = ifelse(.data$new_len < 4L, 4L, .data$new_len),
      !!.by := substr(.data[[.by]], 1L, .data$new_len)
    ) |>
    dplyr::select(-"new_len") |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data[[.by]]) |>
    dplyr::summarise(...)
  
  if (is_sf || is_stars) x <- st_as_sf.csquares(x, .by)

  if (is_stars) {
    resolution <- x$csquares |> nchar()
    resolution <- min(resolution/4)
    resolution <- ifelse(resolution %% 1 > 0, 5, 1)*10^-floor(resolution - 1)

    grd <- st_as_stars.csquares(x, resolution = resolution, add_csquares = TRUE) |>
      dplyr::rename(!!.by := "csquares")
    ret <- grd |>
      .to_df() |>
      dplyr::left_join(x |> sf::st_drop_geometry(), by = .by)
    x <- stars::st_as_stars(.set_dim(ret, dim(grd)),
                            dimensions = stars::st_dimensions(grd))
  }
  
  if ((is_sf || is_stars) && crs$input != sf::st_crs(x)$input) {
    x <- sf::st_transform(x, crs)
  }

  x
}
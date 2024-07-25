#' Resample csquares to a different resolution
#' 
#' Resample csquares objects to higher or lower resolutions.
#' 
#' @param x A `csquares` object to be resampled to a different resolution
#' @param method Method for determining the resolution of the resulting csquares.
#' Should be one of `"target"`, `"min"`, `"max"`, `"up"`, or `"down"`.
#' `"target"` will resample `x` to the level specified with `resolution`
#' @param ... When `x` inherits the `stars` class and the resulting object has
#' a lower resolution than `x`, the dots are passed on to `dplyr::summarise()`.
#' This allows you to summarise columns to the lower resolution.
#' @inheritParams as_csquares
#' @param magnitude When `method == "up"` or `"down"`, this parameter specifies
#' the number of steps to increase or decrease the resolution. Should be a positive
#' integer.
#' @returns A `csquares` object based on `x`
#' @examples
#' csq    <- as_csquares(c("1000", "5000:2|5000:100", "3000:100:100"))
#' csq_df <- as_csquares(data.frame(csq = csq, foobar = letters[1:3]), csquares = "csq")
#' 
#' ## Resample csquares based on the one with the lowest resolution:
#' resample_csquares(csq,    "min")
#'
#' ## Resample csquares to a specific resolution
#' resample_csquares(csq,    "target", resolution = 5)
#'
#' ## Same, but applied to a csquares object inheriting from a data.frame
#' resample_csquares(csq_df, "target", resolution = 5)
#'
#' ## Same, but applied to a csquares object inheriting the `sf` class
#' ## Note that the geometry is updated based on the resampled csquares
#' if (requireNamespace("sf")) {
#'   library(sf)
#'   csq_sf <- st_as_sf(csq_df)
#'   resample_csquares(csq_sf, "target", resolution = 5)
#' }
#'
#' ## Resample csquares one step down.
#' resample_csquares(csq,    "down")
#' resample_csquares(csq_df, "down")
#' 
#' if (requireNamespace(c("dplyr", "stars"))) {
#'   ## Csquares objects can inherit from the stars class as well.
#'   ## These too can be resampled. But additional columns need
#'   ## to be summarised when the resulting resolution is lower
#'   ## than the original:
#'   g <-
#'     sf::st_bbox(c(xmin = 4.0, xmax = 6.5, ymin = 52.5, ymax = 53), crs = 4326) |>
#'       new_csquares(resolution = 0.1) |>
#'       ## add a column with some random positive numbers:
#'       dplyr::mutate(random = .data$csquares |> length() |> rnorm() |> exp())
#' 
#'   ## Resample stars object to lower resolution
#'   g_sum <- resample_csquares(g, resolution = 10, random = sum(random, na.rm = TRUE))
#' 
#'   ## And back to a higher resolution (note that you have lost information as it was summarised
#'   ## in the previous step)
#'   resample_csquares(g_sum, "up", random = sum(random, na.rm = TRUE))
#' }
#' @author Pepijn de Vries
#' @export
resample_csquares <- function(x, method = "target", ..., resolution, magnitude = 1L) {
  .by <- .s3_df_stars_prep(x, allow_all_types = TRUE)
  method   <- rlang::arg_match(method, c("target", "min", "max", "up", "down"))
  res_mis  <- missing(resolution)
  is_sf    <- inherits(x, "sf")
  is_stars <- inherits(x, "stars")
  is_df    <- inherits(x, "data.frame")
  
  if (!res_mis && method != "target")
    rlang::warn(sprintf("`resolution` is specified but not used for method \"%s\"", method))
  
  if (is_stars) {
    col_names <- names(x)
    x <- as.data.frame(x)
  }

  if (is_sf) x <- sf::st_drop_geometry(x)

  if (is_df || is_stars) {
    csq <- dplyr::tibble(csq = x[[.by]])
  } else {
    csq <- dplyr::tibble(csq = x)
  }

  csq <-
    csq |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::unnest("csq") |>
    dplyr::mutate(
      len = nchar(.data$csq),
      target_len = {
        switch(
          method,
          min = min(.data$len),
          max = max(.data$len),
          target = {
            if (res_mis)
              rlang::abort(c(
                x = "`resolution` not specified while method \"target\" requires it",
                i = "specify `resolution` and try again"))
            resolution <- .check_resolution(resolution)
            6L + attr(resolution, "l10")*4L + (1L - attr(resolution, "l5"))*2L
          },
          up   = .data$len + 2L*magnitude,
          down = .data$len - 2L*magnitude
        )
      },
      target_len = ifelse(.data$target_len < 4L, 4L, .data$target_len),
      csq = ifelse(.data$len < .data$target_len, {
        suffix <- 
          mapply(\(x, y){
            seq(from = pmin(y, x + 1), to = y, by = 1L) %in%
              seq(from = 5L, to = y, by = 4L)
          }, x = .data$len, y = .data$target_len, SIMPLIFY = FALSE) |>
          lapply(\(x) c("*", ":")[as.numeric(x) + 1] |> paste0(collapse = "")) |>
          unlist()
        paste0(.data$csq, suffix)
      }, substr(.data$csq, 1L, .data$target_len))
    ) |>
    expand_wildcards(csquares = "csq") |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(csq = .data$csq |> as.character() |> unique() |> sort() |> paste0(collapse = "|")) |>
    dplyr::pull("csq")
  
  if (is_df || is_stars) x[[.by]] <- csq else x <- csq
  if (is_sf) x <- sf::st_as_sf(x)
  if (is_stars) {
    class(x) <- setdiff(class(x), "csquares")
    if (x[[.by]] |> duplicated() |> any()) {
      x <-
        x |>
        dplyr::summarise(..., .by = dplyr::all_of(.by))
    } else {
      x <-
        x |>
        dplyr::select(col_names) |>
        dplyr::mutate(!!.by := as.character(.data[[.by]])) |>
        tidyr::separate_longer_delim(cols = .by, delim = "|") |>
        as_csquares(csquares = .by, validate = FALSE)
    }
    resolution <- .nchar_to_csq_res(
      x$csquares[[1]] |> unclass() |> strsplit("[|]") |> unlist()) |> min()
    
    grd <- new_csquares(x |> as_csquares(csquares = .by, validate = FALSE) |>
                          sf::st_as_sf(), resolution = resolution) |>
      dplyr::rename(!!.by := "csquares")
    ret <- x |> sf::st_drop_geometry()
    class(ret) <- setdiff(class(ret), "csquares")
    ret <- grd |>
      .to_df() |>
      dplyr::mutate(!!.by := as_csquares(.data[[.by]])) |>
      dplyr::left_join(ret, by = .by)
    x <- stars::st_as_stars(.set_dim(ret, dim(grd)),
                            dimensions = stars::st_dimensions(grd))
    x <- .s3_finalise(x, .by)
  }
  x
}
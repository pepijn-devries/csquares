#' Join `csquares` objects using tidyverse conventions
#' 
#' When a `csquares` object inherits from class `data.frame`, you can apply tidyverse
#' joins to the object (`?dplyr::join`). The functions implemented here make sure that
#' the csquares properties are preserved. The functions should be called via the `dplyr`
#' generics. So load the `dplyr` package first, then call the function without the `.csquares`
#' suffix (see examples). When `x` inherits from `stars`, only `left_join` is supported.
#' @rdname tidyjoins
#' @name join
#' @inheritParams dplyr::inner_join
#' @inheritParams sf::st_join
#' @examples
#' if (requireNamespace(c("sf", "dplyr"))) {
#'   library(csquares)
#'   library(sf)
#'   library(dplyr)
#'   orca_sf <- orca |> as_csquares(csquares = "csquares") |> st_as_sf()
#'   right_table <- data.frame(csquares = c("1000:1", "1004:1"), foo = "bar")
#'   
#'   orca_join <- left_join (orca_sf, right_table, by = "csquares")
#'   orca_join <- right_join(orca_sf, right_table, by = "csquares")
#'   orca_join <- inner_join(orca_sf, right_table, by = "csquares")
#'   orca_join <- anti_join (orca_sf, right_table, by = "csquares")
#'   orca_join <- semi_join (orca_sf, right_table, by = "csquares")
#'   orca_grid <- new_csquares(orca_sf, 5)
#'   orca_grid <- left_join(orca_grid, orca, by = "csquares")
#' }
#' 
#' @author Pepijn de Vries
inner_join.csquares = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .no_stars(x)
  .by <- .s3_df_stars_prep(x, "inner_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  result <- NextMethod()
  if (!.by %in% names(result)) .by <- paste0(.by, suffix[[1]])
  .s3_finalise(result, .by)
}

#' @name join
#' @rdname tidyjoins
left_join.csquares = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .by <- .s3_df_stars_prep(x, "left_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  if (inherits(x, "stars")) {
    ret <- x |>
      .to_df() |>
      dplyr::mutate(!!.by := as_csquares(.data[[.by]])) |>
      dplyr::left_join(y, by = by, copy = copy, suffix = suffix, ...)
    x <- stars::st_as_stars(.set_dim(ret, dim(x)),
                            dimensions = stars::st_dimensions(x))
  } else{
    x <- NextMethod()
    if (!.by %in% names(x)) .by <- paste0(.by, suffix[[1]])
  }
  
  .s3_finalise(x, .by)
}

#' @name join
#' @rdname tidyjoins
right_join.csquares = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .no_stars(x)
  .by <- .s3_df_stars_prep(x, "right_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  result <- NextMethod()
  if (!.by %in% names(result)) .by <- paste0(.by, suffix[[1]])
  .s3_finalise(result, .by)
}

#' @name join
#' @rdname tidyjoins
full_join.csquares = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .no_stars(x)
  .by <- .s3_df_stars_prep(x, "full_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  result <- NextMethod()
  if (!.by %in% names(result)) .by <- paste0(.by, suffix[[1]])
  .s3_finalise(result, .by)
}

#' @name join
#' @rdname tidyjoins
semi_join.csquares = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .no_stars(x)
  .by <- .s3_df_stars_prep(x, "semi_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  result <- NextMethod()
  if (!.by %in% names(result)) .by <- paste0(.by, suffix[[1]])
  .s3_finalise(result, .by)
}

#' @name join
#' @rdname tidyjoins
anti_join.csquares = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .no_stars(x)
  .by <- .s3_df_stars_prep(x, "semi_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  result <- NextMethod()
  if (!.by %in% names(result)) .by <- paste0(.by, suffix[[1]])
  .s3_finalise(result, .by)
}

#' @name join
#' @rdname tidyjoins
st_join.csquares = function(x, y, join, ..., suffix = c(".x", ".y")) {
  .by <- .s3_df_stars_prep(x, "st_join")
  class(x) <- setdiff(class(x), "csquares")
  class(y) <- setdiff(class(y), "csquares")
  result <- NextMethod()
  if (!.by %in% names(result)) .by <- paste0(.by, suffix[[1]])
  .s3_finalise(result, .by)
}

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
#' st_as_sfc.csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1")
#' @name st_as_sf.csquares
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

#' @name st_as_sfc.csquares
#' @rdname st_as_sf
#' @export
st_as_sfc.csquares <- function(x) {
  x <- strsplit(x, "[|]")
  
  result <-
    x |>
    purrr::map(\(x) dplyr::tibble(code = x, id = seq_along(x))) |>
    dplyr::tibble() |>
    dplyr::rename(split = 1) |>
    dplyr::mutate(line_number = dplyr::row_number()) |>
    tidyr::unnest("split") |>
    dplyr::mutate(
      check1    = !grepl("[^0-9^:]", .data$code),
      precision = nchar(.data$code)/4,
      precision = (10^-(floor(.data$precision) - 2)) *
        ifelse(.data$precision %% 1 > 0, 0.5, 1),
      code      = ifelse(.data$check1, .data$code, NA),
      code_part = strsplit(.data$code, "[:]")
    ) |>
    tidyr::unnest("code_part") |>
    dplyr::group_by(.data$line_number, .data$id) |>
    dplyr::mutate(
      code_order = dplyr::row_number(),
      last       = .data$code_order == max(.data$code_order)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      step_size  = 10^(2L - .data$code_order),
      check2     = ifelse(is.na(.data$code_part), 0L,
                          nchar(.data$code_part)),
      check2     = ifelse(.data$code_order == 1L,
                          .data$check2 == 4L,
                          ifelse(.data$last,
                                 .data$check2 %in% c(1L, 3L),
                                 .data$check2 == 3L)),
      quadrant   = ifelse(
        .data$code_order == 1L,
        dplyr::case_match(
          substr(.data$code_part, 1L, 1L),
          "7" ~ "NW",
          "1" ~ "NE",
          "5" ~ "SW",
          "3" ~ "SE",
          .default = "--"),
        NA)
    ) |>
    tidyr::fill(.data$quadrant, .direction = "down") |>
    dplyr::mutate(
      coord = {
        stp  <- .data$step_size
        quad <- .data$quadrant
        ord  <- .data$code_order
        cd   <- .data$code_part
        dplyr::tibble(
          check3 = nchar(cd) != 3 | grepl("[1,2,3,4]", substr(cd, 1, 1)),
          x_sgn  = ifelse(grepl("E", .env$quad), 1, -1),
          y_sgn  = ifelse(grepl("N", .env$quad), 1, -1),
          num    = stringr::str_sub(cd, 2L) |> as.numeric(),
          x      = .data$num %% ifelse(ord == 1, 100L, 10L),
          y      = trunc(.data$num/ifelse(ord == 1, 100L, 10L))
        ) |>
          dplyr::mutate(
            x      = ifelse(is.na(.data$x),
                            ifelse(cd %in% c("1", "3"), 0, 5),
                            .data$x),
            y      = ifelse(is.na(.data$y),
                            ifelse(cd %in% c("1", "2"), 0, 5),
                            .data$y),
            check4 = nchar(cd) != 3 | .digit_check(cbind(.data$y, .data$x)) == substr(cd, 1L, 1L),
            x      = stp * .data$x_sgn * .data$x,
            y      = stp * .data$y_sgn * .data$y) |>
          dplyr::select("x", "y", "x_sgn", "y_sgn", "check3", "check4")
      }
    ) |>
    tidyr::unnest("coord") |>
    dplyr::group_by(.data$id, .data$line_number) |>
    dplyr::summarise(
      n_digits   = sum(nchar(.data$code_part)),
      check1    = !any(!.data$check1),
      check2    = !any(!.data$check2),
      check3    = !any(!.data$check3),
      check4    = !any(!.data$check4),
      precision = .data$precision[[1]],
      x_sgn     = .data$x_sgn[[1]],
      y_sgn     = .data$y_sgn[[1]],
      x         = sum(.data$x),
      y         = sum(.data$y),
      .groups   = "keep"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geom      = mapply(
        \(x, y, x_sgn, y_sgn, precision, checks) {
          arguments <- unlist(as.list(environment()))
          if (any(is.na(arguments)) || any(!checks)) {
            sf::st_polygon()
          } else {
            sf::st_polygon(
              list(
                cbind(x + x_sgn * precision * c(0, 1, 1, 0, 0),
                      y + y_sgn * precision * c(0, 0, 1, 1, 0))
              )
            )
          }
        },
        x = .data$x, y = .data$y, x_sgn = .data$x_sgn,
        y_sgn = .data$y_sgn, precision = .data$precision,
        check = .data$check1 & .data$check2 & .data$check3 & .data$check4,
        SIMPLIFY = FALSE)
    ) |>
    dplyr::group_by(.data$line_number) |>
    dplyr::summarise(
      geom = list({
        gms <- .data$geom
        is_empty <- lapply(gms, sf::st_is_empty) |> unlist()
        do.call(c, .data$geom[!is_empty])
      }),
      check1 = !any(!.data$check1),
      check2 = !any(!.data$check2),
      check3 = !any(!.data$check3),
      check4 = !any(!.data$check4),
      .groups = "drop")
  if (any(!(result$check1 & result$check2 & result$check3 & result$check4)))
    rlang::warn("Malformed csquares, introduced empty geometries.")
  result |>
    dplyr::pull("geom")
}

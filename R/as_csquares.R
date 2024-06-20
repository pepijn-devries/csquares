#' Convert lon-lat coordinates into c-square codes
#' 
#' Takes WGS84 longitude and latitude coordinates and finds the closest matching c-squares
#' for a given resolution.
#'
#' @param x An object to be encoded as c-squares. `x` can be a `numeric` `matrix` with two columns
#' containing the x and y coordinates. `x` can also be a simple features object
#' ([`sf`][sf::st_as_sf]) or a spatial arrays object ([`stars`][stars::st_as_stars]).
#' @param resolution Resolution (in WGS84 degrees) to be used for creating c-squares codes.
#' As per c-square specifications, the resolution should be 10 or less, yet greater than 0.
#' It should be a tenfold of 1 or 5. Valid resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.
#' @returns Returns a `vector` with the same number of items as rows in `x`. It contains
#' c-square codes corresponding with `x`.
#' @examples
#' as_csquares(cbind(x = 5.2399066, y = 52.7155812), resolution = 1)
#' @include helpers.R
#' @author Pepijn de Vries
#' @export
as_csquares <- function(x, resolution = 1) {
  resolution <- .check_resolution(resolution)
  l10 <- ceiling(-log10(resolution))
  l5  <- round((resolution/(10^-l10))/5)

  result <- x
  
  if (inherits(x, c("sf", "stars"))) {
    result <- result |>
      sf::st_transform(4326) |>
      sf::st_coordinates()
  }

  result <-
    result |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    dplyr::rename(x = 1, y = 2)

  if (!"L3" %in% colnames(result))
    result <-
    result |>
    dplyr::mutate(
      L3 = dplyr::row_number()
    )

  result |>
    dplyr::mutate(
      quadrant = (0L + 4L*(.data$x < 0)) + (2L + 2L*xor(.data$x >= 0, .data$y >= 0)) - 1L,
      x_cur    = abs(trunc(.data$x/10)),
      x_cur    = ifelse(.data$x_cur > 17, 17, .data$x_cur),
      y_cur    = abs(trunc(.data$y/10)),
      y_cur    = ifelse(.data$y_cur > 8, 8, .data$y_cur),
      x        = .data$x - ifelse(.data$x >= 0, 1, -1)*resolution/2,
      y        = .data$y - ifelse(.data$y >= 0, 1, -1)*resolution/2,
      digits   = {
        array(rep(seq_len(l10 + 1), each = length(.data$x)),
              dim = c(length(.data$x), l10 + 1, 2)) |>
          apply(2, function(x, x1, y1) {

            last_digit <- x[1] == (l10 + 1)
            if (last_digit) fun <- round else fun <- floor
            x <-
              (fun((10^(x - 1)) * abs(cbind(y1, x1))) %% 10)
            x <- cbind(
              .digit_check(x), x)
            x[last_digit & l5 == 1,2:3] <- ""

            apply(x, 1, paste0, collapse = "")
          }, x1 = .data$x, y1 = .data$y) |>
          (\(x) if(is.null(dim(x)))
            paste0(x[x != ""], collapse = ":") else
              apply(x, 1, paste0, collapse = ":"))()
          
      },
      csquares = cbind(
        sprintf("%1i%1i%02i", .data$quadrant,
                abs(.data$y_cur), abs(.data$x_cur)),
        .data$digits) |>
        apply(1, \(x) paste0(x[x != ""], collapse = ":"))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("L3"))) |>
    dplyr::summarise(csquares = paste0(.data$csquares, collapse = "|")) |>
    dplyr::pull("csquares")
}
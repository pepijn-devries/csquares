#' Convert lon-lat coordinates into c-square codes
#' 
#' Takes WGS84 longitude and latitude coordinates and finds the closest matching c-squares
#' for a given resolution.
#'
#' @param x An object to be coerced to a `csquares` object. `x` can be a vector of `character` strings
#' representing c-squares code. It can also be a `numeric` `matrix` with two columns
#' containing the x and y coordinates. `x` can also be a simple features object
#' ([`sf`][sf::st_as_sf]) or a spatial arrays object ([`stars`][stars::st_as_stars]).
#' @param resolution Resolution (in WGS84 degrees) to be used for creating c-squares codes.
#' As per c-square specifications, the resolution should be 10 or less, yet greater than 0.
#' It should be a tenfold of 1 or 5. Valid resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.
#' @param csquares If `x` is not a vector of `character` strings (but for instance a `data.frame`),
#' the `csquares` argument should specify the name of the element of `x` containing the c-square
#' codes as `character` strings.
#' @param validate A `logical` value indicating whether the created object needs to be validated.
#' Defaults to `TRUE`. Validation can be time-consuming so set to `FALSE` to save computing time.
#' @param ... Currently ignored
#' @returns Returns a `csquares` object that contains c-squares codes.
#' @examples
#' as_csquares(cbind(x = 5.2399066, y = 52.7155812), resolution = 1)
#' as_csquares(orca, csquares = "csquares")
#' @include helpers.R
#' @author Pepijn de Vries
#' @rdname as_csquares
#' @name as_csquares
#' @export
as_csquares <- function(x, resolution, csquares, ...) {
  UseMethod("as_csquares", x)
}

#' @rdname as_csquares
#' @name as_csquares
#' @export
as_csquares.default <- function(x, resolution, csquares, ...) {
  rlang::abort(
    paste("No as_csquare method available for objects of class", class(x)[1])
  )
}

#' @rdname as_csquares
#' @name as_csquares
#' @export
as_csquares.character <- function(x, resolution, csquares, validate = TRUE, ...) {
  class(x) <- union("csquares", class(x))
  if (validate) {
    check <- tryCatch({
      validate_csquares(x)
    }, error = function(e) FALSE)
    if (!check)
      rlang::abort(c(
        x = "Could not translate coordinates",
        i = "Check the formating of your c-squares codes"
      ))
  }
  x
}

#' @rdname as_csquares
#' @export
as_csquares.numeric <- function(x, resolution = 1, csquares, ...) {
  if (inherits(x, "matrix")) {
    as_csquares.data.frame(data.frame(x = x[,1], y = x[,2]), resolution)
  } else {
    rlang::abort(
      c(
        x = "Numeric values can only be converted to c-squares in a two column matrix form",
        i = "Make sure your numbers represent two columns (x and y coordinates) in a matrix"
      )
    )
  }
}

#' @rdname as_csquares
#' @export
as_csquares.data.frame <- function(x, resolution = 1, csquares, ...) {
  if (missing(csquares)) {
    resolution <- .check_resolution(resolution)
    
    csquares <- .csquares_generic(x, resolution)
    
    csq_col <- make.names(c(colnames(x), "csquares"), unique = TRUE)
    csq_col <- csq_col[[length(csq_col)]]
    x[[csq_col]] <- csquares
    attributes(x)$csquares_col <- csq_col
    
  } else {
    x[[csquares]] <- as_csquares(x[[csquares]])
    attributes(x)$csquares_col <- csquares
  }
  
  class(x) <- union("csquares", class(x))
  
  x
}

#' @rdname as_csquares
#' @export
as_csquares.sf <- function(x, resolution = 1, csquares, ...) {
  .csquares_spatial(x, resolution)
}

#' @rdname as_csquares
#' @export
as_csquares.stars <- function(x, resolution = 1, csquares, ...) {
  .csquares_spatial(x, resolution)
}

.csquares_spatial <- function(x, resolution) {
  resolution <- .check_resolution(resolution)
  
  csq <-
    x |>
    sf::st_transform(4326) |>
    sf::st_coordinates() |>
    .csquares_generic(resolution)
  class(x)
  nms <- make.names(c(names(x), "csquares"), unique = TRUE)
  nms <- nms[[length(nms)]]
  x[[nms]] <- csq
  attributes(x)$csquares_col <- nms
  x
}

.csquares_generic <- function(x, resolution) {
  l10 <- attributes(resolution)$l10
  l5  <- attributes(resolution)$l5
  x <-
    x |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    dplyr::rename(x = 1, y = 2)
  
  if (!"L3" %in% colnames(x))
    x <-
      x |>
      dplyr::mutate(
        L3 = dplyr::row_number()
      )
  x <-
    x |>
    dplyr::mutate(
      quadrant = (0L + 4L*(.data$x < 0)) + (2L + 2L*xor(.data$x >= 0, .data$y >= 0)) - 1L,
      x_cur    = floor(abs(trunc(.data$x/10)) + resolution*.025),
      y_cur    = floor(abs(trunc(.data$y/10)) + resolution*.025),
      x        = abs(.data$x) - resolution*0.25,
      y        = abs(.data$y) - resolution*0.25,
      digits   = {
        check_dup <- cbind(xc = .data$x_cur, yc = .data$y_cur, x = .data$x, y = .data$y)
        
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
    dplyr::pull("csquares") |>
    c()
  x <- vctrs::new_vctr(x, class = "csquares")
  x
}
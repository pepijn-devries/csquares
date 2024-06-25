.check_resolution <- function(resolution) {
  ## Already checked? return immediately
  if (all(c("l10", "l5") %in% names(attributes(resolution))))
    return (resolution)
  resolution <- as.numeric(resolution)
  if (any(resolution > 10 | resolution <= 0)) rlang::abort(
    c(x = "Cannot encode resolutions >10 or <= 0.",
      i = "Make sure the value of 'resolution' is 10 or less yet greater than 0.")
  )
  
  l10   <- ceiling(-log10(resolution))
  l5    <- round((resolution/(10^-l10))/5)
  sgnf  <- 10^-l10
  digit <- resolution/sgnf
  
  res_fixed <-
    sgnf *
    ifelse(digit < 7.5 & digit > 2.5, 5,
           ifelse(digit > 5, 10, 1))
  
  if (any(abs((res_fixed/resolution) - 1) > 1e-6)) {
    rlang::warn(c(i = "'resolution' should be a tenfold of 1 or 5."))
  }
  attributes(res_fixed)$l10 <- l10
  attributes(res_fixed)$l5  <- l5
  res_fixed
}

.to_df = function(x) {
  dplyr::as_tibble(lapply(x, function(y) structure(y, dim = NULL)))
}

.set_dim = function(x, d) {
  lapply(x, function(y, dims) structure(y, dim = dims), dims = d)
}

.digit_check <- function(x) {
  apply(x, 1, function(z) 2*(floor(z[[1]]/5) + 1) + floor(z[[2]]/5) - 1)
}

.nchar_to_csq_res <- function(x) {
  res <- nchar(x)/4
  res <- ifelse(res %% 1 > 0, .5, 1)*10^-floor(res - 2)
  return(res)
}

.csquares_to_coords <- function(x) {
  x |>
    strsplit("[|]") |>
    purrr::map(\(x) dplyr::tibble(code = x, id = seq_along(x))) |>
    dplyr::tibble() |>
    dplyr::rename(split = 1) |>
    dplyr::mutate(line_number = dplyr::row_number()) |>
    tidyr::unnest("split") |>
    dplyr::mutate(
      check1    = !grepl("[^0-9^:]", .data$code),
      precision = .nchar_to_csq_res(.data$code),
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
        .get_quadrant(.data$code_part),
        NA)
    ) |>
    tidyr::fill("quadrant", .direction = "down") |>
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
}

.get_quadrant <- function(x) {
  dplyr::case_match(
    substr(x, 1L, 1L),
    "7" ~ "NW",
    "1" ~ "NE",
    "5" ~ "SW",
    "3" ~ "SE",
    .default = "--")
}
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

.check_csquare_validity <- function(x, allow_wildcards = FALSE) {

  num_char <- nchar(x)
  num_char_max <- max(num_char)
  
  ## code consists of at least 4 characters and has an even length
  check  <- num_char >= 4L & (num_char %% 2L) == 0L
  colons <- 5L + (seq_len(ceiling((num_char_max - 5L)/4L)) - 1L)*4L
  if (length(colons) == 0) colons <- num_char_max + 1L
  deg5   <- colons + 1L

  ## root coordinates should not be >17, >8
  checkvalues <- as.numeric(substr(x, 3, 4)) |> suppressWarnings()
  check <- check &
    (is.na(checkvalues) | (checkvalues <= 17 &
       substr(x, 2, 2) != "9"))
  
  ## Colons occur at the correct positions
  check <- check & (
    lapply(x, \(x) all(stringr::str_sub(x, colons, colons) %in% c(":", ""))) |>
      unlist()
  ) & !(num_char %in% colons)
  
  ## first number after colon is 1, 2, 3, or 4
  if (allow_wildcards) wc <- "*" else wc <- NULL
  checkstring <- c(as.character(1:4), wc, "")
  check <- check &
    lapply(x, \(x) all(stringr::str_sub(x, deg5, deg5) %in% checkstring)) |>
    unlist()
  
  ## the number after the colon should encode the following digits
  check <- check &
    dplyr::tibble(
      x = lapply(x, \(x) {
        cbind(
          stringr::str_sub(x, start = deg5 + 1, end = deg5 + 2),
          stringr::str_sub(x, start = deg5, end = deg5)
        )
      })) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::unnest("x") |>
    dplyr::mutate(
      checkvalue = .data$x[,1],
      x = .data$x[,2],
      checkvalue = as.numeric(.data$checkvalue) |> suppressWarnings(),
      checkvalue = .digit_check(
        cbind(floor(.data$checkvalue/10), .data$checkvalue %% 10)
      ),
      check = (is.na(.data$checkvalue) | .data$checkvalue == .data$x)
    ) |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(check = all(.data$check)) |>
    dplyr::pull("check")

  ## very first number should match a quadrant number
  checkstring <- c(as.character((1:4)*2 - 1), wc, "")
  check <- check &
    lapply(x, \(x) all(stringr::str_sub(x, 1L, 1L) %in% checkstring)) |>
    unlist()
  
  ## all other character are numerics between 0 and 9
  checkstring <- c(as.character(0:9), wc, "")
  nums <- seq_len(num_char_max)
  nums <- nums[!nums %in% c(colons, deg5, 1)]
  check <- check &
    lapply(x, \(x) all(stringr::str_sub(x, nums, nums) %in% checkstring)) |>
    unlist()
  
  check
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
      check     = .check_csquare_validity(.data$code),
      precision = .nchar_to_csq_res(.data$code),
      code      = ifelse(!grepl("[^0-9^:]", .data$code), .data$code, NA),
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
            x      = stp * .data$x_sgn * .data$x,
            y      = stp * .data$y_sgn * .data$y) |>
          dplyr::select("x", "y", "x_sgn", "y_sgn")
      }
    ) |>
    tidyr::unnest("coord") |>
    dplyr::group_by(.data$id, .data$line_number) |>
    dplyr::summarise(
      n_digits   = sum(nchar(.data$code_part)),
      precision = .data$precision[[1]],
      x_sgn     = .data$x_sgn[[1]],
      y_sgn     = .data$y_sgn[[1]],
      x         = sum(.data$x),
      y         = sum(.data$y),
      check     = all(.data$check),
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
        checks = .data$check,
        SIMPLIFY = FALSE)
    ) |>
    dplyr::group_by(.data$line_number) |>
    dplyr::summarise(
      geom = list({
        gms <- .data$geom
        is_empty <- lapply(gms, sf::st_is_empty) |> unlist()
        do.call(c, .data$geom[!is_empty])
      }),
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

.no_stars_or_char <- function(x, method) {
  if (inherits(x, c("character", "stars"))) {
    rlang::abort(
      c(x = 
          sprintf("'%s' is not implemented for 'csquares' objects inheriting from 'character' or 'stars'",
                  method),
        i = "Coerce object to 'data.frame', 'tibble' or 'sf' first.")
    )
  }
}

.no_stars <- function(x, method) {
  if (inherits(x, c("stars"))) {
    rlang::abort(
      c(x = 
          sprintf("'%s' is not implemented for 'csquares' objects inheriting from 'stars'",
                  method),
        i = "Coerce object to 'data.frame', 'tibble' or 'sf' first.")
    )
  }
}

.s3_df_stars_prep <- function(x, method, allow_all_types = FALSE) {
  .by <- attributes(x)$csquares_col
  if (!inherits(x, c("data.frame", "stars")) && !allow_all_types) {
    rlang::abort(c(
      x = sprintf("'%s' not available for csquares objects that don't inherit class 'data.frame' or 'stars'",
                  method),
      i = "Coerce your csquares object to 'sf', 'tibble', 'data.frame', or 'stars' first."
    ))
  }
  if (inherits(x, "stars") && !requireNamespace("stars", quietly = TRUE)) {
    rlang::abort(c(
      x = "Package 'stars' is required but not available",
      i = "Please install 'stars' and try again"
    ))
  }
  if (inherits(x, "sf") && !requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(c(
      x = "Package 'sf' is required but not available",
      i = "Please install 'sf' and try again"
    ))
  }
  if (!is.null(.by) && !.by %in% names(x)) {
    if (length(x) == 0L) .by2 <- NULL else
      .by2 <- (lapply(x, inherits, "csquares") |> unlist()) |> which()
    if (length(.by2) < 1L)
      rlang::abort(c(x = "No csquares column found in csquares object!",
                     i = "Make sure you are working with a valid csquares object."))
    .by <- names(x)[[.by2[[1L]]]]
  }
  
  .by
}

.s3_finalise <- function(x, .by) {
  attributes(x)$csquares_col <- .by
  class(x) <- union("csquares", class(x))
  x
}

.all_of_class <- function(..., my_class) {
  lapply(list(...), inherits, my_class) |> unlist() |> all()
}
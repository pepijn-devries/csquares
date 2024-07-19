#' Expand c-squares with wildcards to all matching c-squares
#' 
#' The asterisk (*) can be used as a wildcard, for a compact
#' notation of csquares. `expand_wildcards` will replace all
#' wild cards with valid combinations of values and expands
#' the compact notation to an explicit notation without
#' wildcards.
#' Check out `vignette("Wildcards")` for more details.
#' @param x A `character` string containing csquares codes with
#' wildcards (asterisk character); or a `data.frame` that contains
#' a column with csquares codes with wildcards
#' @param csquares When `x` is `data.frame` this argument should
#' specify the column name that contains the csquares codes with wildcards.
#' @param ... ignored
#' @returns Returns a `csquares` object with full notation
#' @author Pepijn de Vries
#' @examples
#' expand_wildcards("1000:*")
#' expand_wildcards("1000:***")
#' expand_wildcards("1000:1**")
#' expand_wildcards("1000:***:*")
#' expand_wildcards(c("1000:*", "1000:***", "1000:1**", "1000:***:*"))
#' 
#' expand_wildcards(data.frame(csq = "1000:*", foo = "bar"), csquares = "csq")
#' @export
expand_wildcards <- function(x, csquares, ...) {
  is_char <- inherits(x, "character")
  copy <- NULL
  if (missing(csquares)) {
    if (inherits(x, "data.frame"))
      rlang::abort(c(x = "`csquares` argument is missing", i = "Specify which column contains csquares codes"))
  } else {
    if (is_char)
      rlang::abort(c(x = "Cannot assign `csquares` column when `x` inherits from `character`",
                     i = "Coerce `x` to a data.frame or omit the `csquares` argument"))
  }
  if (inherits(x, "data.frame")) {
    copy <- x
    x <- dplyr::tibble(codes = x[[csquares]] |> unclass())
  } else {
    x <- dplyr::tibble(codes = x)
  }
  
  x <-
    x |>
    dplyr::mutate(
      codes = lapply(.data$codes, \(x) strsplit(x, "[|]")[[1]]),
      row   = dplyr::row_number()
    ) |>
    tidyr::unnest("codes") |>
    dplyr::mutate(
      code_id      = dplyr::row_number(),
      valid        = .check_csquare_validity(.data$codes, allow_wildcards = TRUE),
      wildcard_pos = lapply(x, \(x) gregexpr("[*]", x)[[1]])
    ) |>
    tidyr::unnest("wildcard_pos")
  
  wildcard_opts <-
    dplyr::tibble(
      wildcard_pos = x$wildcard_pos |> unique() |> sort()
    ) |>
    dplyr::mutate(
      type = dplyr::case_match(
        .data$wildcard_pos,
        1L ~ 0L,
        2L ~ 1,
        3L ~ 2L,
        .default =  3L + ((.data$wildcard_pos - 5L) %% 4L > 1L)
      )
    ) |>
    dplyr::left_join(
      wildcard_opts <- dplyr::tibble(
        type = 0L:4L,
        opts = list(
          1L + 2L*(0L:3L),
          0L:8L,
          0L:1L,
          1L:4L,
          0L:9L
        )
      ),
      "type"
    )
  
  x <-
    x |>
    dplyr::left_join( wildcard_opts, "wildcard_pos" ) |>
    dplyr::group_by(.data$codes, .data$row, .data$code_id) |>
    dplyr::summarise(
      wildcard_pos = list(.data$wildcard_pos),
      opts = do.call(expand.grid, .data$opts) |>
        apply(1, as.character, simplify = FALSE) |>
        list()
    ) |>
    dplyr::mutate(
      codes_exp = {
        lapply(.data$opts, \(x) {
          result <- rep(.data$codes, length(x))
          regmatches(result, gregexpr("[*]", result)) <- x
          result <- result[.check_csquare_validity(result)] |>
            unique() |>
            sort()
          paste0(result, collapse = "|")
        })
      },
      codes_exp = unlist(.data$codes_exp)
    ) |>
    dplyr::group_by(.data$row) |>
    dplyr::summarise(codes_exp = .data$codes_exp |> unique() |> sort() |> paste0(collapse = "|"),
                     .groups = "keep") |>
    dplyr::arrange(.data$row) |>
    dplyr::pull("codes_exp") |>
    as_csquares()
  if (missing(csquares)) {
    x
  } else {
    dplyr::mutate(copy, !!csquares := x) |>
      as_csquares(csquares = csquares)
  }
}

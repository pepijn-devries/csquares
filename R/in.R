#' Match c-squares against other c-squares (with wildcards)
#' 
#' Checks if csquares codes in `table` matches values in `x`. Wildcards
#' are allowed in `table` for this comparison. Check out `vignette("wildcards")`
#' for more details.
#' @param x An object of class 'csquares' that will be checked for matching
#' values in `table`
#' @param table A `character` string representing a csquares code. The
#' code can contain wildcards (asterisk `*` and percentage `%` characters,
#' both having identical meaning). Any symbol in `x` will result in a positive match
#' against the wildcard. `table` can also be of class `csquares`, but these
#' objects cannot contain wildcards.
#' @param strict When set to `FALSE`, a match is positive when the start
#' of `x`, matches against values in `table`, even when `x` has a higher resolution.
#' When set to `TRUE`, a match is only positive when the resolution of `x` and
#' `table` is identical.
#' @param mode Two modes are allowed: `"all"` and `"any"`. When an element of
#' `x` consists of multiple raster cells, it the mode will determine whether
#' a match is positive or not. In case of `"all"`, all raster cells in the element
#' of `x` need to match with the cells in `table`, for a positive match. In
#' case of `"any"`, any match will do.
#' @param ... Ignored
#' @returns Returns a vector of `logical` values with the same number of
#' elements or rows as `x`
#' @author Pepijn de Vries
#' @examples
#' library(dplyr)
#' 
#' in_csquares(orca$csquares, c("3400:2", "5515:3"))
#' in_csquares(orca$csquares, "3400:2|5515:3")
#' 
#' ## Percentage symbols are interpreted the same as asterisk symbols
#' ## both are wild cards
#' in_csquares(orca$csquares, "1%%%:%") |>
#'   table()
#'
#' ## Same as above
#' in_csquares(orca$csquares, "1***:*") |>
#'   table()
#'  
#' ## Also same as above
#' in_csquares(orca$csquares, "1***", strict = FALSE) |>
#'   table()
#'
#' ## Strict interpretation results in no matches
#' in_csquares(orca$csquares, "1***", strict = TRUE) |>
#'   table()
#' 
#' ## Filter orca data to North Eastern quadrant (1***:*) only:
#' orca |>
#'   filter(
#'     in_csquares(csquares, "1***:*")
#'   ) |>
#'   nrow()
#' 
#' @include helpers.R
#' @export
in_csquares <- function(x, table, strict = FALSE, mode = "any", ...) {
  rlang::arg_match(mode, c("any", "all"))

  fun <- if(mode == "any") any else all
  if (!inherits(x, "character")) {
    .by <- attributes(x)$csquares_col
    x <- x[[.by]]
  }
  if (!inherits(x, c("character", "csquares")))
    rlang::abort(
      c(x = "'x' is not or cannot be converted into a csquares object.",
        i = "Make sure 'x' contains valid csquares codes."))
  x <- strsplit(x, "[|]")

  table <- strsplit(gsub("[%]", "*", table), "[|]") |> unlist()
  check <- .check_csquare_validity(table, allow_wildcards = TRUE)
  if (!all(check))
    rlang::abort(c(x = "`table` is not a vector of valid c-squares codes",
                   i = "Check its format and try again"))
  pat <- paste0("^", gsub("[*]", ".", gsub("[:]", "[:]", table)))
  if (strict) pat <- paste0(pat, "$")
  pat <- paste0(pat, collapse = "|")
  
  lapply(x, \(x) fun(grepl(pat, x))) |> unlist()
}
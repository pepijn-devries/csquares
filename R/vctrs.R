vec_proxy.csquares <- function(x, ...) {
  x
}

vec_restore.csquares <- function(x, to, ...) {
  if (inherits(to, "csquares") && !inherits(x, "csquares")) as_csquares(x) else
    if (inherits(to, "character")) as_csquares(x, validate = FALSE) else
      x
}

#' vctrs methods for csquares objects
#' 
#' Implementations to support csquare vctrs operations. There is no need to call these functions directly.
#' @param x,y Vector types.
#' @param ... Ignored.
#' @param x_arg,y_arg Argument names for `x` and `y`.
#' @param to Types to cast to. If NULL, `x` will be returned as is.
#' @name vctrs
#' @export vec_cast.csquares
#' @export
vec_cast.csquares <- function(x, to, ...) {
  UseMethod("vec_cast.csquares")
}

#' @name vctrs
#' @method vec_cast.csquares csquares
#' @export
vec_cast.csquares.csquares <- function(x, to, ...) {
  if (inherits(to, "csquares")) x else if (inherits(to, "character")) as.character(x) else
    rlang::abort("Unable to cast csquares")
}

#' @name vctrs
#' @method vec_cast.csquares character
#' @export
vec_cast.csquares.character <- function(x, to, ...) {
  if (inherits(to, "csquares")) as_csquares(x) else rlang::abort("Unable to cast csquares")
}

#' @name vctrs
#' @method vec_cast.csquares default
#' @export
vec_cast.csquares.default <- function(x, to, ...) {
  if (inherits(to, "csquares")) as_csquares(x) else rlang::abort("Unable to cast csquares")
}

#' @name vctrs
#' @export vec_ptype2.csquares
#' @export
vec_ptype2.csquares <- function(x, y, ...) {
  UseMethod("vec_ptype2.csquares", y)
}

#' @name vctrs
#' @method vec_ptype2.csquares character
#' @export
vec_ptype2.csquares.character <- function(x, y, ...) {
  .vec_char_only(x)
  as_csquares(character(0))
}

#' @name vctrs
#' @method vec_ptype2.csquares csquares
#' @export
vec_ptype2.csquares.csquares <- function(x, y, ...) {
  .vec_char_only(x)
  as_csquares(character(0))
}

#' @name vctrs
#' @method vec_ptype2.csquares default
#' @export
vec_ptype2.csquares.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  .vec_char_only(x)
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

.vec_char_only <- function(x) {
  if (typeof(x) != "character")
    rlang::abort(c(x = "Vector operations on `csquare` are only allowed if they inherit `character`.",
                   i = "If `x` inherits from `data.frame` or `stars`, pull the csquares column from that object."))
}
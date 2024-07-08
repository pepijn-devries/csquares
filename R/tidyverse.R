#' @include summarise.R
register_all_s3_methods = function() {
  register_s3_method("sf",      "st_as_sf",    "csquares")
  register_s3_method("sf",      "st_as_sfc",   "csquares")
  register_s3_method("stars",   "st_as_stars", "csquares")
  register_s3_method("dplyr",   "select",      "csquares")
  register_s3_method("dplyr",   "as_tibble",   "csquares")
  register_s3_method("dplyr",   "filter",      "csquares")
  register_s3_method("dplyr",   "summarise",   "csquares")
  register_s3_method("methods", "show",        "csquares")
}

# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

filter.csquares <- function(.data, ..., .dots) {
  .by <- attributes(.data)$csquares_col
  if (inherits(.data, "data.frame")) {
    result <- NextMethod()
  } else {
    rlang::abort(c(
      x = "'filter' not available for csquares objects that don't inherit class 'data.frame'",
      i = "Coerce your csquares object to 'sf', 'tibble', or 'data.frame' first."
    ))
  }
  attributes(result)$csquares_col <- .by
  class(result) <- union("csquares", class(result))
  result
}

select.csquares <- function(.data, ...) {
  # If .data is a 'csquares' object, make sure that the 'select' doesn't drop the
  # csquares column
  if (inherits(.data, "data.frame")) {
    if (!requireNamespace("tidyselect", quietly = TRUE)) 
      rlang::abort(c(
        x = "tidyselect required",
        i = "Install it first and try again"))
    loc <- tidyselect::eval_select(quote(c(...)), .data)
    .by <- attributes(.data)$csquares_col
    loc <- union(
      loc,
      tidyselect::eval_select(quote(dplyr::any_of(.by)), .data)
    )
    class(.data) <- setdiff(class(.data), "csquares")
    .data <- .data |> dplyr::select(dplyr::any_of(loc))
    attributes(.data)$csquares_col <- .by
    class(.data) <- union("csquares", class(.data))
  } else {
    rlang::abort(c(
      x = "'select' not available for csquares objects that don't inherit class 'data.frame'",
      i = "Coerce your csquares object to 'sf', 'tibble', or 'data.frame' first."
    ))
  }
  .data
}

as_tibble.csquares <- function(x, ...) {
  if (inherits(x, "character")) {
    x <- dplyr::tibble(csquares = x)
    .by <- "csquares"
  } else {
    .by = attributes(x)$csquares_col
    class(x) <- setdiff(class(x), "csquares")
    x <- NextMethod()
  }
  attributes(x)$csquares_col <- .by
  class(x) <- union("csquares", class(x))
  x
}

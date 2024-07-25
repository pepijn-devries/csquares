#' @importFrom rlang .data .env !! !!! :=
#' @importFrom methods setOldClass
#' 
NULL

setOldClass("csquares")

.onLoad = function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse)
}

register_all_s3_methods = function() {
  ## Note that superseded tidyverse methods are not included, as there are better alternatives
  ## vctrs
  register_s3_method("vctrs",   "vec_cast",      "csquares")
  register_s3_method("vctrs",   "vec_ptype2",    "csquares")
  register_s3_method("vctrs",   "vec_proxy",     "csquares")
  register_s3_method("vctrs",   "vec_restore",   "csquares")
  ## sf
  register_s3_method("sf",      "st_as_sf",      "csquares")
  register_s3_method("sf",      "st_as_sfc",     "csquares")
  register_s3_method("sf",      "st_join",       "csquares")
  ## stars
  register_s3_method("stars",   "st_as_stars",   "csquares")
  ## dplyr
  register_s3_method("dplyr",   "arrange",       "csquares")
  register_s3_method("dplyr",   "as_tibble",     "csquares")
  register_s3_method("dplyr",   "distinct",      "csquares")
  register_s3_method("dplyr",   "filter",        "csquares")
  register_s3_method("dplyr",   "group_by",      "csquares")
  register_s3_method("dplyr",   "group_split",   "csquares")
  register_s3_method("dplyr",   "mutate",        "csquares")
  register_s3_method("dplyr",   "rename",        "csquares")
  register_s3_method("dplyr",   "rename_with",   "csquares")
  register_s3_method("dplyr",   "rowwise",       "csquares")
  register_s3_method("dplyr",   "select",        "csquares")
  register_s3_method("dplyr",   "slice",         "csquares")
  register_s3_method("dplyr",   "summarise",     "csquares")
  register_s3_method("dplyr",   "ungroup",       "csquares")
  register_s3_method("dplyr",   "inner_join",    "csquares")
  register_s3_method("dplyr",   "left_join",     "csquares")
  register_s3_method("dplyr",   "full_join",     "csquares")
  register_s3_method("dplyr",   "anti_join",     "csquares")
  register_s3_method("dplyr",   "right_join",    "csquares")
  register_s3_method("dplyr",   "semi_join",     "csquares")
  ## tidyr
  register_s3_method("tidyr",   "drop_na",       "csquares")
  register_s3_method("tidyr",   "nest",          "csquares")
  register_s3_method("tidyr",   "pivot_longer",  "csquares")
  register_s3_method("tidyr",   "pivot_wider",   "csquares")
  register_s3_method("tidyr",   "unite",         "csquares")
  register_s3_method("tidyr",   "unnest",        "csquares")
  register_s3_method("tidyr",   "unnest",        "csquares_nested")
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

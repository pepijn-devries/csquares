#' @importFrom rlang .data .env !! !!! :=
#' @importFrom methods setOldClass
#' 
NULL

setOldClass("csquares")

.onLoad = function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse)
}
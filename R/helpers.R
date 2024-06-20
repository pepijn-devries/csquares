.check_resolution <- function(resolution) {
  if (any(resolution > 10 | resolution <= 0)) rlang::abort(
    c(x = "Cannot encode resolutions >10 or <= 0.",
      i = "Make sure the value of 'resolution' is 10 or less yet greater than 0.")
  )
  sgnf  <- 10^(-ceiling(-log10(resolution)))
  digit <- resolution/sgnf
  res_fixed <-
    sgnf *
    ifelse(digit < 7.5 & digit > 2.5, 5,
           ifelse(digit > 5, 10, 1))

  if (any(abs((res_fixed/resolution) - 1) > 1e-6)) {
    rlang::warn(c(i = "'resolution' should be a tenfold of 1 or 5."))
  }
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
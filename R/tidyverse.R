#' Tidyverse methods for csquares objects (drop the 'csquares'-suffix)
#' 
#' Tidyverse methods for `csquares` objects that inherit from `data.frame`, `tibble`, `sf`, or
#' in some cases `stars`. Load the tidyverse package containing the generic implementation (`dplyr` or `tidyr`),
#' and call the function without the `.csquares` suffix.
#' See examples for more details. The methods implemented here ensure that the `csquare` class is preserved.
#' 
#' Note that the implementation of `summarise.csquares` has changed since version 0.0.5.002, to better
#' reflect the `dplyr` generic implementation. To get results similar to the earlier implementation please
#' use `resample_csquares()`.
#' @param .data,...,.dots,data,x,add,.fn,.cols,.keep_all,cols,cols_vary,names_to,names_prefix,names_sep,names_pattern,names_ptypes,names_transform,names_repair,values_to,values_drop_na,values_ptypes,values_transform,id_cols,id_expand,names_from,names_glue,names_sort,names_vary,names_expand,values_from,values_fill,values_fn,unused_fn,.tbl,.keep,col,sep,remove,.preserve Passed to tidyverse generic methods. Consult their documentation.
#' @rdname tidyverse
#' @include init.R
#' @include helpers.R
#' @examples
#' if (requireNamespace(c("dplyr", "tidyr"))) {
#'   library(dplyr)
#'   library(tidyr)
#'   
#'   ## Create a csquares object from the orca dataset:
#'   orca_csq <- as_csquares(orca, csquares = "csquares")
#'   
#'   ## Filter values that belong to the killer whale realm:
#'   orca2 <- filter(orca_csq, orcinus_orca == TRUE)
#'   
#'   ## Mutate the object to hold information on the quadrant:
#'   orca_csq <- mutate(orca_csq, quadrant = csquares |> as.character() |> substr(1,1))
#'   
#'   ## Select the quadrant column:
#'   orca2 <- select(orca_csq, quadrant)
#'   
#'   ## Convert it into a tibble:
#'   orca_csq <- as_tibble(orca_csq)
#'   
#'   ## Arrange by quadrant:
#'   orca2 <- arrange(orca_csq, quadrant)
#'   
#'   ## Group by quadrant:
#'   orca_csq <- group_by(orca_csq, quadrant)
#'   
#'   ## Summarise per quadrant:
#'   summarise(orca_csq, realm_frac = sum(orcinus_orca)/n())
#'   
#'   #' Introduce a group split:
#'   orca2 <- group_split(orca_csq)
#'   
#'   ## Ungroup the object:
#'   orca_csq <- ungroup(orca_csq)
#'   
#'   ## Take a slice of the first three rows:
#'   slice(orca_csq, 1:3)
#'   
#'   ## Take a sample of 10 rows with replacement:
#'   slice_sample(orca_csq, n = 10, replace = TRUE)
#'   
#'   ## Rename a column:
#'   rename(orca_csq, quad = "quadrant")
#'   rename_with(orca_csq, toupper, starts_with("quad"))
#'   
#'   ## Distinct will remove any duplicated rows:
#'   orca_csq[c(1, 1, 1),] |> distinct()
#'   
#'   ## Pivot to a wide format:
#'   pivot_wider(orca_csq, names_from = "quadrant", values_from = "orcinus_orca")
#'   pivot_wider(orca_csq, names_from = "orcinus_orca", values_from = "orcinus_orca",
#'               id_cols = "quadrant", values_fn = length)
#'   
#'   ## Pivot to a long format (note that you can't pivot the csquares column to long)
#'   tibble(csq = "1000", a = 1, b = 2, d = 3) |>
#'     as_csquares(csquares = "csq") |>
#'     pivot_longer(c("a", "b", "d"), names_to = "letter", values_to = "numeric")
#'   
#'   ## Unite two columns into one:
#'   unite(orca_csq, "quad_realm", any_of(c("quadrant", "orcinus_orca")))
#'   
#'   ## As the csquares column gets nested in the example below,
#'   ## the resulting object is no longer of class csquares:
#'   orca_nest <- nest(orca_csq, nested_data = c("csquares", "orcinus_orca"))
#'   
#'   ## Unnest it:
#'   unnest(orca_nest, "nested_data")
#' }
#' 
#' @author Pepijn de Vries
filter.csquares <- function(.data, ..., .dots) {
  .by <- .s3_df_stars_prep(.data, "filter")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
select.csquares <- function(.data, ...) {
  # If .data is a 'csquares' object, make sure that the 'select' doesn't drop the
  # csquares column
  if (inherits(.data, c("stars", "data.frame"))) {
    if (!requireNamespace("tidyselect", quietly = TRUE)) 
      rlang::abort(c(
        x = "tidyselect required",
        i = "Install it first and try again"))
    loc <- tidyselect::eval_select(quote(c(...)), .data |> dplyr::as_tibble()) |> names()
    .by <- attributes(.data)$csquares_col
    loc <- union(
      loc,
      tidyselect::eval_select(quote(dplyr::any_of(.by)), .data |> dplyr::as_tibble()) |> names()
    )
    class(.data) <- setdiff(class(.data), "csquares")
    .data <- .data |>
      dplyr::select(dplyr::any_of(loc)) |>
      .s3_finalise(.by)
  } else {
    rlang::abort(c(
      x = "'select' not available for csquares objects that don't inherit class 'data.frame'",
      i = "Coerce your csquares object to 'sf', 'tibble', or 'data.frame' first."
    ))
  }
  .data
}

#' @rdname tidyverse
as_tibble.csquares <- function(x, ...) {
  if (inherits(x, "character")) {
    x <- dplyr::tibble(csquares = x)
    .by <- "csquares"
  } else {
    .by <- .s3_df_stars_prep(x, "as_tibble")
  }
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
arrange.csquares <- function(.data, ..., .dots) {
  .by <- .s3_df_stars_prep(.data, "arrange")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
group_by.csquares <- function(.data, ..., add = FALSE) {
  .by <- .s3_df_stars_prep(.data, "group_by")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
ungroup.csquares <- function(.data, ...) {
  .by <- .s3_df_stars_prep(.data, "ungroup")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
rowwise.csquares <- function(.data, ...) {
  .by <- .s3_df_stars_prep(.data, "rowwise")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
mutate.csquares <- function(.data, ..., .dots) {
  .by <- .s3_df_stars_prep(.data, "mutate")
  result <- NextMethod()
  if (.by %in% names(result)) result <- .s3_finalise(result, .by)
  result
}

#' @rdname tidyverse
rename.csquares <- function(.data, ...) {
  .by    <- .s3_df_stars_prep(.data, "rename")
  loc    <- tidyselect::eval_rename(rlang::expr(c(...)), as.data.frame(.data))
  by_loc <- which(names(.data) == .by)
  if (by_loc %in% loc) .by <- names(loc)[[by_loc]]
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
rename_with.csquares <- function(.data, .fn, .cols, ...) {
  .no_stars(.data)
  .by <- .s3_df_stars_prep(.data, "rename_with")
  loc    <- tidyselect::eval_rename(.cols, .data)
  by_loc <- which(names(.data) == .by)
  result <- NextMethod()
  if (by_loc %in% loc) .by <- names(result)[[by_loc]]
  .s3_finalise(result, .by)
}

#' @rdname tidyverse
slice.csquares <- function(.data, ..., .dots) {
  .by <- .s3_df_stars_prep(.data, "slice")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
distinct.csquares <- function(.data, ..., .keep_all = FALSE) {
  .by <- .s3_df_stars_prep(.data, "distinct")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
summarise.csquares <- function(.data, ..., .dots) {
  .no_stars_or_char(.data, "summarise")
  is_sf <- inherits(.data, "sf")
  if (is_sf) {
    sf_column <- attributes(.data)$sf_column
    .data <-
      .data |>
      dplyr::mutate(
        !!sf_column := lapply(dplyr::cur_group_rows(), \(x) sf::st_polygon()) |> sf::st_sfc()
      )
  }
  .by <- .s3_df_stars_prep(.data, "summarise")
  class(.data) <- setdiff(class(.data), "csquares")
  result <- NextMethod()
  i <- if (inherits(.data, c("grouped_df", "grouped_dt")))
    dplyr::group_indices(.data) else
      rep(1, nrow(.data))
  result[[.by]] <-
    lapply(sort(unique(i)), \(j)
           .data[[.by]][i == j] |>
             as.character() |>
             strsplit("[|]") |>
             unlist() |>
             unique() |>
             sort() |>
             paste0(collapse = "|")) |>
    unlist() |>
    as_csquares()
  result <- .s3_finalise(result, .by)
  if (is_sf) {
    result <-
      result |>
      sf::st_drop_geometry() |>
      sf::st_as_sf()
  }
  result
}

#' @rdname tidyverse
pivot_longer.csquares <- function(
    data, cols, ..., cols_vary = "fastest", names_to = "name", 
    names_prefix = NULL, names_sep = NULL, names_pattern = NULL, 
    names_ptypes = NULL, names_transform = NULL, names_repair = "check_unique", 
    values_to = "value", values_drop_na = FALSE, values_ptypes = NULL, 
    values_transform = NULL) {
  .no_stars(data)
  .by <- .s3_df_stars_prep(data, "pivot_longer")
  if (attributes(data)$csquares_col %in% cols)
    rlang::abort(c(x = "Cannot pivot 'csquares' column", i = "Please pivot other column(s)"))
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
pivot_wider.csquares <- function(
    data, ..., id_cols = NULL, id_expand = FALSE, names_from = NULL, names_prefix = "", 
    names_sep = "_", names_glue = NULL, names_sort = FALSE, names_vary = "fastest", 
    names_expand = FALSE, names_repair = "check_unique", values_from = NULL, values_fill = NULL, 
    values_fn = NULL, unused_fn = NULL) {
  .no_stars(data)
  .by <- .s3_df_stars_prep(data, "pivot_wider")
  ## If csquares column is not among the id_cols, we should drop the geometry,
  ## union the csquares for each id_cols group and include that in the id_cols
  cic  <- utils::getFromNamespace("compat_id_cols", "tidyr")
  bice <- utils::getFromNamespace("build_wider_id_cols_expr", "tidyr")
  id_cols <- cic(id_cols = {{id_cols}}, ..., fn_call = match.call(expand.dots = FALSE))
  id_cols <- bice(data, !!id_cols, !!names_from, !!values_from, rlang::caller_env()) |> eval()
  is_sf <- inherits(data, "sf")
  class(data) <- setdiff(class(data), c("sf", "csquares"))
  attributes(data)$csquares_col <- NULL
  attributes(data)$sf_column    <- NULL
  attributes(data)$agr          <- NULL
  result <-
    tidyr::pivot_wider(
      data, ..., id_cols = id_cols, id_expand = id_expand, names_from = names_from, names_prefix = names_prefix,
      names_sep = names_sep, names_glue = names_glue, names_sort = names_sort, names_vary = names_vary,
      names_expand = names_expand, names_repair = names_repair, values_from = values_from, values_fill = values_fill,
      values_fn = values_fn, unused_fn = unused_fn)
  if (!.by %in% id_cols) {
    result <-
      dplyr::left_join(
        result,
        data |>
          dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) |>
          dplyr::summarise(
            !!.by := 
              .data[[.by]] |> as.character() |> strsplit("[|]") |>
              unlist() |> unique() |> sort() |> paste0(collapse = "|"),
            .groups = "keep"
          ),
        by = id_cols)
  }
  if (.by %in% colnames(result)) result <- .s3_finalise(result, .by)
  if (is_sf) result <- sf::st_as_sf(result)
  result
}

#' @rdname tidyverse
group_split.csquares <- function(.tbl, ..., .keep = TRUE) {
  .by <- .s3_df_stars_prep(.tbl, "group_split")
  class(.tbl) <- setdiff(class(.tbl), "csquares")
  result <- lapply(NextMethod(), .s3_finalise, .by = .by)
}

nest.csquares <- function(.data, ...) {
  .no_stars_or_char(.data, "nest")
  .by <- .s3_df_stars_prep(.data, "nest")
  class(.data) <- setdiff(class(.data), "csquares")
  attributes(.data)$csquares_col <- NULL
  result <- NextMethod()
  nested_col <-
    lapply(result, \(x) inherits(x, "list") && inherits(x[[1]], "data.frame") && .by %in% names(x[[1]])) |>
    unlist() |>
    which()
  if (length(nested_col) == 0) {
    result <- .s3_finalise(result, .by)
  } else {
    result[[nested_col]] <-
      lapply(result[[nested_col]], as_csquares.data.frame, csquares = .by)
    attributes(result)$csquares_col <- .by
    class(result) <- union("csquares_nested", class(result))
  }
  result
}

#' @rdname tidyverse
unite.csquares <- function(data, col, ..., sep = "_", remove = TRUE) {
  .no_stars(data)
  is_sf <- inherits(data, "sf")
  .by <- .s3_df_stars_prep(data, "unite")
  csq <- data[[.by]]
  class(data) <- setdiff(class(data), "csquares")
  attributes(data)$csquares_col <- NULL
  col <- rlang::enquo0(col)
  data <- tidyr::unite(data, !!col, ..., sep = sep, remove = remove)
  if (is_sf) data <-
    sf::st_as_sf(data, sf_column_name = attr(data, "sf_column"))
  
  data[[.by]] <- csq
  .s3_finalise(data, .by)
}

#' @rdname tidyverse
unnest.csquares <- function(data, ..., .preserve = NULL) {
  .by <- .s3_df_stars_prep(data, "unnest")
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
unnest.csquares_nested <- function(data, ..., .preserve = NULL) {
  .by <- attributes(data)$csquares_col
  .s3_finalise(NextMethod(), .by)
}

#' @rdname tidyverse
drop_na.csquares <- function(x, ...) {
  .by <- .s3_df_stars_prep(.data, "drop_na")
  .s3_finalise(NextMethod(), .by)
}
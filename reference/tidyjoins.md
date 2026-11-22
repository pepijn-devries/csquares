# Join `csquares` objects using tidyverse conventions

When a `csquares` object inherits from class `data.frame`, you can apply
tidyverse joins to the object
([`?dplyr::join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)).
The functions implemented here make sure that the csquares properties
are preserved. The functions should be called via the `dplyr` generics.
So load the `dplyr` package first, then call the function without the
`.csquares` suffix (see examples). When `x` inherits from `stars`, only
`left_join` is supported.

## Usage

``` r
inner_join.csquares(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

left_join.csquares(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

right_join.csquares(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

full_join.csquares(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

semi_join.csquares(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

anti_join.csquares(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

st_join.csquares(x, y, join, ..., suffix = c(".x", ".y"))
```

## Arguments

- x, y:

  A pair of data frames, data frame extensions (e.g. a tibble), or lazy
  data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
  more details.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- ...:

  Other parameters passed onto methods.

- join:

  geometry predicate function with the same profile as
  [st_intersects](https://r-spatial.github.io/sf/reference/geos_binary_pred.html);
  see details

## See also

Other tidy-operators:
[`tidyverse`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)

## Author

Pepijn de Vries

## Examples

``` r
# \donttest{
if (requireNamespace(c("sf", "dplyr"))) {
  library(csquares)
  library(sf)
  library(dplyr)
  orca_sf <- orca |> as_csquares(csquares = "csquares") |> st_as_sf()
  right_table <- data.frame(csquares = c("1000:1", "1004:1"), foo = "bar")
  
  orca_join <- left_join (orca_sf, right_table, by = "csquares")
  orca_join <- right_join(orca_sf, right_table, by = "csquares")
  orca_join <- inner_join(orca_sf, right_table, by = "csquares")
  orca_join <- anti_join (orca_sf, right_table, by = "csquares")
  orca_join <- semi_join (orca_sf, right_table, by = "csquares")
  orca_grid <- new_csquares(orca_sf, 5)
  orca_grid <- left_join(orca_grid, orca, by = "csquares")
}
# }
```

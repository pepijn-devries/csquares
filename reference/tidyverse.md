# Tidyverse methods for csquares objects (drop the 'csquares'-suffix)

Tidyverse methods for `csquares` objects that inherit from `data.frame`,
`tibble`, `sf`, or in some cases `stars`. Load the tidyverse package
containing the generic implementation (`dplyr` or `tidyr`), and call the
function without the `.csquares` suffix. See examples and
[`vignette("tidy")`](https://pepijn-devries.github.io/csquares/articles/tidy.md)
for more details. The methods implemented here ensure that the `csquare`
class is preserved.

## Usage

``` r
filter.csquares(.data, ..., .dots)

select.csquares(.data, ...)

as_tibble.csquares(x, ...)

arrange.csquares(.data, ..., .dots)

group_by.csquares(.data, ..., add = FALSE)

ungroup.csquares(.data, ...)

rowwise.csquares(.data, ...)

mutate.csquares(.data, ..., .dots)

rename.csquares(.data, ...)

rename_with.csquares(.data, .fn, .cols, ...)

slice.csquares(.data, ..., .dots)

distinct.csquares(.data, ..., .keep_all = FALSE)

summarise.csquares(.data, ..., .dots)

pivot_longer.csquares(
  data,
  cols,
  ...,
  cols_vary = "fastest",
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = NULL,
  names_transform = NULL,
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = NULL,
  values_transform = NULL
)

pivot_wider.csquares(
  data,
  ...,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = NULL,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_vary = "fastest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = NULL,
  values_fill = NULL,
  values_fn = NULL,
  unused_fn = NULL
)

group_split.csquares(.tbl, ..., .keep = TRUE)

nest.csquares(.data, ...)

unite.csquares(data, col, ..., sep = "_", remove = TRUE)

unnest.csquares(data, ..., .preserve = NULL)

unnest.csquares_nested(data, cols, ...)

drop_na.csquares(x, ...)
```

## Arguments

- .data, ..., .dots, data, x, add, .fn, .cols, .keep_all, cols,
  cols_vary, names_to, names_prefix, names_sep, names_pattern,
  names_ptypes, names_transform, names_repair, values_to,
  values_drop_na, values_ptypes, values_transform, id_cols, id_expand,
  names_from, names_glue, names_sort, names_vary, names_expand,
  values_from, values_fill, values_fn, unused_fn, .tbl, .keep, col, sep,
  remove, .preserve:

  Passed to tidyverse generic methods. Consult their documentation.

## Details

Note that the implementation of `summarise.csquares` has changed since
version 0.0.5.002, to better reflect the `dplyr` generic implementation.
To get results similar to the earlier implementation please use
[`resample_csquares()`](https://pepijn-devries.github.io/csquares/reference/resample_csquares.md).

## See also

Other tidy-operators:
[`join`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)

## Author

Pepijn de Vries

## Examples

``` r
if (requireNamespace(c("dplyr", "tidyr"))) {
  library(dplyr)
  library(tidyr)
  
  ## Create a csquares object from the orca dataset:
  orca_csq <- as_csquares(orca, csquares = "csquares")
  
  ## Filter values that belong to the killer whale realm:
  orca2 <- filter(orca_csq, orcinus_orca == TRUE)
  
  ## Mutate the object to hold information on the quadrant:
  orca_csq <- mutate(orca_csq, quadrant = csquares |> as.character() |> substr(1,1))
  
  ## Select the quadrant column:
  orca2 <- select(orca_csq, quadrant)
  
  ## Convert it into a tibble:
  orca_csq <- as_tibble(orca_csq)
  
  ## Arrange by quadrant:
  orca2 <- arrange(orca_csq, quadrant)
  
  ## Group by quadrant:
  orca_csq <- group_by(orca_csq, quadrant)
  
  ## Summarise per quadrant:
  summarise(orca_csq, realm_frac = sum(orcinus_orca)/n())
  
  #' Introduce a group split:
  orca2 <- group_split(orca_csq)
  
  ## Ungroup the object:
  orca_csq <- ungroup(orca_csq)
  
  ## Take a slice of the first three rows:
  slice(orca_csq, 1:3)
  
  ## Take a sample of 10 rows with replacement:
  slice_sample(orca_csq, n = 10, replace = TRUE)
  
  ## Rename a column:
  rename(orca_csq, quad = "quadrant")
  rename_with(orca_csq, toupper, starts_with("quad"))
  
  ## Distinct will remove any duplicated rows:
  orca_csq[c(1, 1, 1),] |> distinct()
  
  ## Pivot to a wide format:
  pivot_wider(orca_csq, names_from = "quadrant", values_from = "orcinus_orca")
  pivot_wider(orca_csq, names_from = "orcinus_orca", values_from = "orcinus_orca",
              id_cols = "quadrant", values_fn = length)
  
  ## Pivot to a long format (note that you can't pivot the csquares column to long)
  tibble(csq = "1000", a = 1, b = 2, d = 3) |>
    as_csquares(csquares = "csq") |>
    pivot_longer(c("a", "b", "d"), names_to = "letter", values_to = "numeric")
  
  ## Unite two columns into one:
  unite(orca_csq, "quad_realm", any_of(c("quadrant", "orcinus_orca")))
  
  ## As the csquares column gets nested in the example below,
  ## the resulting object is no longer of class csquares:
  orca_nest <- nest(orca_csq, nested_data = c("csquares", "orcinus_orca"))
  
  ## Unnest it:
  unnest(orca_nest, "nested_data")
}
#> # A tibble: 2,058 × 3
#>    quadrant csquares   orcinus_orca
#>  * <chr>    <csquares> <lgl>       
#>  1 3        3603:3     FALSE       
#>  2 3        3413:4     TRUE        
#>  3 3        3109:3     FALSE       
#>  4 3        3016:4     FALSE       
#>  5 3        3602:2     FALSE       
#>  6 3        3700:1     FALSE       
#>  7 3        3015:3     FALSE       
#>  8 3        3106:4     FALSE       
#>  9 3        3317:4     FALSE       
#> 10 3        3507:3     FALSE       
#> # ℹ 2,048 more rows
```

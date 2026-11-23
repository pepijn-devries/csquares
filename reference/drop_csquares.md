# Drop c-square information from object

Drops c-square data from an object, but keeps the parent class of the
object intact. You cannot deselect the csquare column from a `csquares`
object as this will render the object invalid. Use `drop_csquares`
instead.

## Usage

``` r
drop_csquares(x, ...)
```

## Arguments

- x:

  An object of class `csquares` from which the c-square information
  needs to be dropped.

- ...:

  Arguments passed on to
  [`as_csquares`](https://pepijn-devries.github.io/csquares/reference/as_csquares.md)

  `resolution`

  :   Resolution (in WGS84 degrees) to be used for creating c-squares
      codes. As per c-square specifications, the resolution should be 10
      or less, yet greater than 0. It should be a tenfold of 1 or 5.
      Valid resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.

  `csquares`

  :   If `x` is not a vector of `character` strings (but for instance a
      `data.frame`), the `csquares` argument should specify the name of
      the element of `x` containing the c-square codes as `character`
      strings.

  `validate`

  :   A `logical` value indicating whether the created object needs to
      be validated. Defaults to `TRUE`. Validation can be time-consuming
      so set to `FALSE` to save computing time.

  `use_centroids`

  :   In case `x` is a simple features object and `use_centroids` is
      `TRUE`, the centroid of each geometry is used for deriving
      c-squares. If it is `FALSE` all coordinates in the geometry are
      used.

## Value

Returns a copy of `x` inheriting its parent classes but with out
csquares info.

## See also

Other csquares-handlers:
[`expand_wildcards()`](https://pepijn-devries.github.io/csquares/reference/expand_wildcards.md),
[`in_csquares()`](https://pepijn-devries.github.io/csquares/reference/in_csquares.md),
[`resample_csquares()`](https://pepijn-devries.github.io/csquares/reference/resample_csquares.md)

## Author

Pepijn de Vries

## Examples

``` r
csq <- as_csquares("1000")
drop_csquares(csq)
#> <character[1]>
#> [1] 1000

csq <-
  data.frame(csquares = "1000", foo = "bar") |>
  as_csquares(csquares = "csquares")

drop_csquares(csq)
#>   foo
#> 1 bar
```

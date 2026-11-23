# Expand c-squares with wildcards to all matching c-squares

The asterisk (\*) can be used as a wildcard, for a compact notation of
csquares. `expand_wildcards` will replace all wild cards with valid
combinations of values and expands the compact notation to an explicit
notation without wildcards. Check out `vignette("wildcards")` for more
details.

## Usage

``` r
expand_wildcards(x, csquares, ...)
```

## Arguments

- x:

  A `character` string containing csquares codes with wildcards
  (asterisk character); or a `data.frame` that contains a column with
  csquares codes with wildcards

- csquares:

  When `x` is `data.frame` this argument should specify the column name
  that contains the csquares codes with wildcards.

- ...:

  Arguments passed on to
  [`as_csquares`](https://pepijn-devries.github.io/csquares/reference/as_csquares.md)

  `resolution`

  :   Resolution (in WGS84 degrees) to be used for creating c-squares
      codes. As per c-square specifications, the resolution should be 10
      or less, yet greater than 0. It should be a tenfold of 1 or 5.
      Valid resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.

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

Returns a `csquares` object with full notation

## See also

Other csquares-handlers:
[`drop_csquares()`](https://pepijn-devries.github.io/csquares/reference/drop_csquares.md),
[`in_csquares()`](https://pepijn-devries.github.io/csquares/reference/in_csquares.md),
[`resample_csquares()`](https://pepijn-devries.github.io/csquares/reference/resample_csquares.md)

## Author

Pepijn de Vries

## Examples

``` r
expand_wildcards("1000:*")
#>  csquares [1:1]   4 squares
expand_wildcards("1000:***")
#>  csquares [1:1] 100 squares
expand_wildcards("1000:1**")
#>  csquares [1:1]  25 squares
expand_wildcards("1000:***:*")
#>  csquares [1:1] 400 squares
expand_wildcards(c("1000:*", "1000:***", "1000:1**", "1000:***:*"))
#>  csquares [1:4]   4 squares, 100 squares,  25 squares, 400 squares

expand_wildcards(data.frame(csq = "1000:*", foo = "bar"), csquares = "csq")
#>           csq foo
#> 1   4 squares bar
```

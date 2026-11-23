# vctrs methods for csquares objects

Implementations to support csquare vctrs operations. There is no need to
call these functions directly.

## Usage

``` r
vec_cast.csquares(x, to, ...)

# S3 method for class 'csquares'
vec_cast.csquares(x, to, ...)

# S3 method for class 'character'
vec_cast.csquares(x, to, ...)

# Default S3 method
vec_cast.csquares(x, to, ...)

vec_ptype2.csquares(x, y, ...)

# S3 method for class 'character'
vec_ptype2.csquares(x, y, ...)

# S3 method for class 'csquares'
vec_ptype2.csquares(x, y, ...)

# Default S3 method
vec_ptype2.csquares(x, y, ..., x_arg = "x", y_arg = "y")
```

## Arguments

- x, y:

  Vector types.

- to:

  Types to cast to. If NULL, `x` will be returned as is.

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

- x_arg, y_arg:

  Argument names for `x` and `y`.

## Value

`vec_cast` returns a vector of the same length as `x`; `vec_ptype`, and
`vec_ptype2` return unfinalised prototype.

## Examples

``` r
orca_csq <- as_csquares(orca, csquares = "csquares")$csquares[1L:10L]
vctrs::vec_cast(orca_csq, NULL)
#>  csquares [1:10] S60 E 30 (5°), S60 W 40 (5°), N10 W 90 (5°), N 0 W 40 (5°)...
vctrs::vec_ptype(orca_csq)
#>  csquares [1:0] 
vctrs::vec_ptype2(orca_csq, character())
#>  csquares [1:0] 
```

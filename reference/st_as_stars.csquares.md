# Coerce csqaures object into a stars object

Take a `csquares` object created with
[new_csquares](https://pepijn-devries.github.io/csquares/reference/new_csquares.md)
or
[as_csquares](https://pepijn-devries.github.io/csquares/reference/as_csquares.md)
and coerce it to a spatiotemporal array
([stars](https://r-spatial.github.io/stars/reference/st_as_stars.html)).

## Usage

``` r
st_as_stars.csquares(x, ...)
```

## Arguments

- x:

  An object of class `csquares` created with
  [new_csquares](https://pepijn-devries.github.io/csquares/reference/new_csquares.md)
  or
  [as_csquares](https://pepijn-devries.github.io/csquares/reference/as_csquares.md)

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

Returns a spatiotemporal array
([stars](https://r-spatial.github.io/stars/reference/st_as_stars.html))
object based on `x`.

## See also

Other csquares-class:
[`as_csquares()`](https://pepijn-devries.github.io/csquares/reference/as_csquares.md),
[`new_csquares()`](https://pepijn-devries.github.io/csquares/reference/new_csquares.md),
[`validate_csquares()`](https://pepijn-devries.github.io/csquares/reference/validate_csquares.md)

## Author

Pepijn de Vries

## Examples

``` r
library(stars)
#> Loading required package: abind
st_as_stars(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>      csquares  
#>  Length   : 4  
#>  N.unique : 4  
#>  N.blank  : 0  
#>  Min.nchar:10  
#>  Max.nchar:10  
#> dimension(s):
#>   from to offset delta refsys x/y
#> x    1  2   -0.5   0.5 WGS 84 [x]
#> y    1  2     52  -0.5 WGS 84 [y]
# \donttest{
st_as_stars(as_csquares(orca, csquares = "csquares"))
#> stars object with 2 dimensions and 2 attributes
#> attribute(s):
#>      csquares     orcinus_orca   
#>  Length   :2592   Mode :logical  
#>  N.unique :2592   FALSE:1849     
#>  N.blank  :   0   TRUE :209      
#>  Min.nchar:   6   NAs  :534      
#>  Max.nchar:   6                  
#> dimension(s):
#>   from to offset delta refsys x/y
#> x    1 72   -180     5 WGS 84 [x]
#> y    1 36     90    -5 WGS 84 [y]
# }
```

# Create a simple features object from c-squares

Converts a `character` string of c-squares in a spatially explicit
simple features object
([`sf`](https://r-spatial.github.io/sf/reference/sf.html). It can also
convert `data.frame`s with a column of c-squares codes to an
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Usage

``` r
st_as_sf.csquares(x, ..., use_geometry = TRUE)

st_as_sfc.csquares(x, ..., use_geometry = TRUE)
```

## Arguments

- x:

  A `vector` of `character` strings. Each element should hold a valid
  c-square code. `x` can also be a `data.frame` with a column of
  c-square codes. (Note that wildcard characters are not supported)

- ...:

  Ignored

- use_geometry:

  If `use_geometry` is `TRUE` and `x` inherits a spatial feature, its
  geometry will be used to cast the object. This is much faster than its
  alternative when `use_geometry` is `FALSE`. In the latter case, the
  c-square codes are first translated into explicit spatial information.
  The latter is more reliable as it does not rely on the assumption that
  the geometry of `x` corresponds with the csquares codes in the object.
  In short: use `TRUE` for speed, use `FALSE` for reliability.

## Value

In case of `st_as_sfc.csquares` a list of geometries
([`sfc`](https://r-spatial.github.io/sf/reference/sfc.html),
(MULTI)POLYGONS) is returned. In case of `st_as_sf.csquares` an object
of class ([`sf`](https://r-spatial.github.io/sf/reference/sf.html)) is
returned.

## Author

Pepijn de Vries

## Examples

``` r
library(sf)
st_as_sfc(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: 51 xmax: 0.5 ymax: 52
#> CRS:           NA
#> POLYGON ((-0.5 51.5, -0.5 52, 0 52, 0.5 52, 0.5...
st_as_sf(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -0.5 ymin: 51 xmax: 0.5 ymax: 52
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 2
#>   csquares                                                                  geom
#> * <csquares>                                                       <POLYGON [°]>
#> 1 7500:110:3|7500:110:1|1500:110:3|1500:110:1 ((-0.5 51.5, -0.5 52, 0 52, 0.5 5…
```

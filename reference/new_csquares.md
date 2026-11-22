# Create a c-squares raster from a bounding box

Creates a spatial raster
([`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html))
with c-square codes for a specified bounding box, using a specified
resolution. The raster will be conform c-squares specifications.

## Usage

``` r
new_csquares(x, resolution = 1, crs = 4326)
```

## Arguments

- x:

  An object of class
  [`bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html) or an
  object that can be coerced to a `bbox`. It defines the bounding box
  for the c-squares grid created by this function.

- resolution:

  Resolution (in WGS84 degrees) to be used for creating c-squares codes.
  As per c-square specifications, the resolution should be 10 or less,
  yet greater than 0. It should be a tenfold of 1 or 5. Valid
  resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.

- crs:

  The projection to be used for the created grid. By default it is WGS84
  (EPSG:4326).

## Value

Returns a
[`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html)
and `csquares` object based on the provided bounding box and resolution.

## Author

Pepijn de Vries

## Examples

``` r
library(sf)
#> Linking to GEOS 3.14.1, GDAL 3.12.2, PROJ 9.7.1; sf_use_s2() is TRUE
nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
new_csquares(nc)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>      csquares  
#>  Length   :40  
#>  N.unique :40  
#>  N.blank  : 0  
#>  Min.nchar: 8  
#>  Max.nchar: 8  
#> dimension(s):
#>   from to offset delta refsys x/y
#> x    1 10    -85     1 WGS 84 [x]
#> y    1  4     37    -1 WGS 84 [y]
```

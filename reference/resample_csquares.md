# Resample csquares to a different resolution

Resample csquares objects to higher or lower resolutions.

## Usage

``` r
resample_csquares(x, method = "target", ..., resolution, magnitude = 1L)
```

## Arguments

- x:

  A `csquares` object to be resampled to a different resolution

- method:

  Method for determining the resolution of the resulting csquares.
  Should be one of `"target"`, `"min"`, `"max"`, `"up"`, or `"down"`.
  `"target"` will resample `x` to the level specified with `resolution`

- ...:

  When `x` inherits the `stars` class and the resulting object has a
  lower resolution than `x`, the dots are passed on to
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
  This allows you to summarise columns to the lower resolution.

- resolution:

  Resolution (in WGS84 degrees) to be used for creating c-squares codes.
  As per c-square specifications, the resolution should be 10 or less,
  yet greater than 0. It should be a tenfold of 1 or 5. Valid
  resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.

- magnitude:

  When `method == "up"` or `"down"`, this parameter specifies the number
  of steps to increase or decrease the resolution. Should be a positive
  integer.

## Value

A `csquares` object based on `x`

## Author

Pepijn de Vries

## Examples

``` r
csq    <- as_csquares(c("1000", "5000:2|5000:100", "3000:100:100"))
csq_df <- as_csquares(data.frame(csq = csq, foobar = letters[1:3]), csquares = "csq")

## Resample csquares based on the one with the lowest resolution:
resample_csquares(csq,    "min")
#>  csquares [1:3] N 0 E  0 (10°), S 0 W  0 (10°), S 0 E  0 (10°)

## Resample csquares to a specific resolution
resample_csquares(csq,    "target", resolution = 5)
#>  csquares [1:3]   4 squares,   2 squares, S 0 E  0 (5°)

## Same, but applied to a csquares object inheriting from a data.frame
resample_csquares(csq_df, "target", resolution = 5)
#>             csq foobar
#> 1     4 squares      a
#> 2     2 squares      b
#> 3 S 0 E  0 (5°)      c

## Same, but applied to a csquares object inheriting the `sf` class
## Note that the geometry is updated based on the resampled csquares
if (requireNamespace("sf")) {
  library(sf)
  csq_sf <- st_as_sf(csq_df)
  resample_csquares(csq_sf, "target", resolution = 5)
}
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -10 ymin: -5 xmax: 10 ymax: 10
#> Geodetic CRS:  WGS 84
#>             csq foobar                           geom
#> 1     4 squares      a POLYGON ((0 5, 0 10, 5 10, ...
#> 2     2 squares      b POLYGON ((-10 -5, -10 0, -5...
#> 3 S 0 E  0 (5°)      c POLYGON ((0 0, 5 0, 5 -5, 0...

## Resample csquares one step down.
resample_csquares(csq,    "down")
#>  csquares [1:3] N 0 E  0 (10°),   2 squares, S 0 E  0 (0.5°)
resample_csquares(csq_df, "down")
#>               csq foobar
#> 1  N 0 E  0 (10°)      a
#> 2       2 squares      b
#> 3 S 0 E  0 (0.5°)      c

# \donttest{
if (requireNamespace(c("dplyr", "stars"))) {
  ## Csquares objects can inherit from the stars class as well.
  ## These too can be resampled. But additional columns need
  ## to be summarised when the resulting resolution is lower
  ## than the original:
  g <-
    sf::st_bbox(c(xmin = 4.0, xmax = 6.5, ymin = 52.5, ymax = 53), crs = 4326) |>
      new_csquares(resolution = 0.1) |>
      ## add a column with some random positive numbers:
      dplyr::mutate(random = .data$csquares |> length() |> rnorm() |> exp())

  ## Resample stars object to lower resolution
  g_sum <- resample_csquares(g, resolution = 10, random = sum(random, na.rm = TRUE))

  ## And back to a higher resolution (note that you have lost information as it was summarised
  ## in the previous step)
  resample_csquares(g_sum, "up", random = sum(random, na.rm = TRUE))
}
#> stars object with 2 dimensions and 2 attributes
#> attribute(s):
#>      csquares     random      
#>  Length   :4   Min.   :224.3  
#>  N.unique :4   1st Qu.:224.3  
#>  N.blank  :0   Median :224.3  
#>  Min.nchar:6   Mean   :224.3  
#>  Max.nchar:6   3rd Qu.:224.3  
#>                Max.   :224.3  
#> dimension(s):
#>   from to offset delta refsys x/y
#> x    1  2      0     5 WGS 84 [x]
#> y    1  2     60    -5 WGS 84 [y]
# }
```

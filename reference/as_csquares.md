# Convert lon-lat coordinates into c-square codes

Takes WGS84 longitude and latitude coordinates and finds the closest
matching c-squares for a given resolution.

## Usage

``` r
as_csquares(x, resolution, csquares, ...)

# Default S3 method
as_csquares(x, resolution, csquares, ...)

# S3 method for class 'character'
as_csquares(x, resolution, csquares, validate = TRUE, ...)

# S3 method for class 'numeric'
as_csquares(x, resolution = 1, csquares, ...)

# S3 method for class 'data.frame'
as_csquares(x, resolution = 1, csquares, ...)

# S3 method for class 'sf'
as_csquares(x, resolution = 1, csquares, ..., use_centroids = TRUE)

# S3 method for class 'sfc'
as_csquares(x, resolution = 1, csquares, ..., use_centroids = TRUE)

# S3 method for class 'stars'
as_csquares(x, resolution = 1, csquares, ...)
```

## Arguments

- x:

  An object to be coerced to a `csquares` object. `x` can be a vector of
  `character` strings representing c-squares code. It can also be a
  `numeric` `matrix` with two columns containing the x and y
  coordinates. `x` can also be a simple features object
  ([`sf`](https://r-spatial.github.io/sf/reference/st_as_sf.html)) or a
  spatial arrays object
  ([`stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html)).

- resolution:

  Resolution (in WGS84 degrees) to be used for creating c-squares codes.
  As per c-square specifications, the resolution should be 10 or less,
  yet greater than 0. It should be a tenfold of 1 or 5. Valid
  resolutions are therefore: 10, 5, 1, 0.5, 0.1, etc.

- csquares:

  If `x` is not a vector of `character` strings (but for instance a
  `data.frame`), the `csquares` argument should specify the name of the
  element of `x` containing the c-square codes as `character` strings.

- ...:

  Currently ignored

- validate:

  A `logical` value indicating whether the created object needs to be
  validated. Defaults to `TRUE`. Validation can be time-consuming so set
  to `FALSE` to save computing time.

- use_centroids:

  In case `x` is a simple features object and `use_centroids` is `TRUE`,
  the centroid of each geometry is used for deriving c-squares. If it is
  `FALSE` all coordinates in the geometry are used.

## Value

Returns a `csquares` object that contains c-squares codes.

## Author

Pepijn de Vries

## Examples

``` r
as_csquares(cbind(x = 5.2399066, y = 52.7155812), resolution = 1)
#>          x        y      csquares
#> x 5.239907 52.71558 N50 E  0 (1°)
pts <-
  expand.grid(
    x = seq(-0.1, 0.1, by = 0.01),
    y = seq(-0.1, 0.1, by = 0.01)
  ) |>
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
as_csquares(pts, 0.05)
#> Simple feature collection with 441 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -0.1 ymin: -0.1 xmax: 0.1 ymax: 0.1
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>              geometry         csquares
#> 1   POINT (-0.1 -0.1) S 0 W  0 (0.05°)
#> 2  POINT (-0.09 -0.1) S 0 W  0 (0.05°)
#> 3  POINT (-0.08 -0.1) S 0 W  0 (0.05°)
#> 4  POINT (-0.07 -0.1) S 0 W  0 (0.05°)
#> 5  POINT (-0.06 -0.1) S 0 W  0 (0.05°)
#> 6  POINT (-0.05 -0.1) S 0 W  0 (0.05°)
#> 7  POINT (-0.04 -0.1) S 0 W  0 (0.05°)
#> 8  POINT (-0.03 -0.1) S 0 W  0 (0.05°)
#> 9  POINT (-0.02 -0.1) S 0 W  0 (0.05°)
#> 10 POINT (-0.01 -0.1) S 0 W  0 (0.05°)
orca_csq <- as_csquares(orca, csquares = "csquares")
```

# Get ICES geometries

Functions to convert ICES rectangles

## Usage

``` r
ices_centroids(ices_rect)

ices_rectangles(ices_rect)

ices_to_csquares(ices_rect)

ices_from_csquares(csquares)
```

## Arguments

- ices_rect:

  A `character` vector containing valid ICES rectangle codes

- csquares:

  A `csquares` object, or an object that can be coerced with
  [`as_csquares()`](https://pepijn-devries.github.io/csquares/reference/as_csquares.md).

## Value

In case of `ices_centroids` a
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
is returned, with `POINT` geometries representing the centroids of the
ICES rectangles.

In case of `ices_rectangles` a
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
is returned, with `POLYGON` geometries representing the outline of the
ICES rectangles.

In case of `ices_to_csquares` a `csquares` object inheriting from
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) is
returned, the csquares code should represent the ICES rectangles.

In case of `ices_from_csquares` a `character` vector is returned with
ICES rectangle codes that correspond with the csquares. The method is
fast yet crude: it only checks in which ICES rectangles the centroids of
the csquares are located. It does not check if the resolution matches.
`NA` values are returned when csquares are situated outside the area
covered by ICES rectangles.

## See also

Other other-encodings:
[`ices_columns()`](https://pepijn-devries.github.io/csquares/reference/ices_columns.md)

## Author

Pepijn de Vries

## Examples

``` r
ices_rects <-
  c("31F21", "31F22", "31F23", "31F24", "31F25", "31F26", "31F27", "31F28", "31F29",
    "32F2", "33F2", "34F2", "35F2",
    "31F3", "32F3", "33F3", "34F3", "35F3",
    "31F4", "32F4", "33F4", "34F4", "35F4")
ices_centroids(ices_rects)
#> Simple feature collection with 23 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2.166667 ymin: 51.08333 xmax: 4.5 ymax: 53.25
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>     ICES subrect                  geometry
#> 1  31F21    TRUE POINT (2.166667 51.41667)
#> 2  31F22    TRUE    POINT (2.166667 51.25)
#> 3  31F23    TRUE POINT (2.166667 51.08333)
#> 4  31F24    TRUE      POINT (2.5 51.41667)
#> 5  31F25    TRUE         POINT (2.5 51.25)
#> 6  31F26    TRUE      POINT (2.5 51.08333)
#> 7  31F27    TRUE POINT (2.833333 51.41667)
#> 8  31F28    TRUE    POINT (2.833333 51.25)
#> 9  31F29    TRUE POINT (2.833333 51.08333)
#> 10  32F2   FALSE         POINT (2.5 51.75)
ices_rectangles(ices_rects)
#> Simple feature collection with 23 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 51 xmax: 5 ymax: 53.5
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>     ICES subrect                       geometry
#> 1  31F21    TRUE POLYGON ((2 51.33333, 2 51....
#> 2  31F22    TRUE POLYGON ((2 51.16667, 2 51....
#> 3  31F23    TRUE POLYGON ((2 51, 2 51.16667,...
#> 4  31F24    TRUE POLYGON ((2.333333 51.33333...
#> 5  31F25    TRUE POLYGON ((2.333333 51.16667...
#> 6  31F26    TRUE POLYGON ((2.333333 51, 2.33...
#> 7  31F27    TRUE POLYGON ((2.666667 51.33333...
#> 8  31F28    TRUE POLYGON ((2.666667 51.16667...
#> 9  31F29    TRUE POLYGON ((2.666667 51, 2.66...
#> 10  32F2   FALSE POLYGON ((2 51.5, 2 52, 3 5...
ices_csq <- ices_to_csquares(ices_rects)
#> Warning: Input contains ICES subrectangles which are incompatible with csquares.
ices_from_csquares(ices_csq)
#>  [1] NA     NA     NA     NA     NA     NA     NA     NA     NA     "32F2"
#> [11] "33F2" "34F2" "35F2" "31F3" "32F3" "33F3" "34F3" "35F3" "31F4" "32F4"
#> [21] "33F4" "34F4" "35F4"
```

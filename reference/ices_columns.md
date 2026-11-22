# Valid ICES rectangle columns

Get all valid column codes of ICES rectangles. Note that ICES
subrectangles are not compatible with csquares. For more details see
[`vignette("ices")`](https://pepijn-devries.github.io/csquares/articles/ices.md).

## Usage

``` r
ices_columns()
```

## Value

A `character` vector with all allowed codes for the columns in ICES
rectangles.

## See also

Other other-encodings:
[`ices_centroids()`](https://pepijn-devries.github.io/csquares/reference/ices_geom.md)

## Examples

``` r
ices_columns()
#>   [1] "A0" "A1" "A2" "A3" "B0" "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9" "C0"
#>  [16] "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9" "D0" "D1" "D2" "D3" "D4" "D5"
#>  [31] "D6" "D7" "D8" "D9" "E0" "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "F0"
#>  [46] "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "G0" "G1" "G2" "G3" "G4" "G5"
#>  [61] "G6" "G7" "G8" "G9" "H0" "H1" "H2" "H3" "H4" "H5" "H6" "H7" "H8" "H9" "J0"
#>  [76] "J1" "J2" "J3" "J4" "J5" "J6" "J7" "J8" "J9" "K0" "K1" "K2" "K3" "K4" "K5"
#>  [91] "K6" "K7" "K8" "K9" "L0" "L1" "L2" "L3" "L4" "L5" "L6" "L7" "L8" "L9" "M0"
#> [106] "M1" "M2" "M3" "M4" "M5" "M6" "M7" "M8"
```

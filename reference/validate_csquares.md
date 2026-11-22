# Test if a csquares object is valid

Tests if a `csquares` object is correctly specified and can be
translated into valid coordinates

## Usage

``` r
validate_csquares(x)
```

## Arguments

- x:

  An object of class `csquares` to be evaluated.

## Value

Returns a `logical` value indicating whether the `csquares` object is
valid or not.

## See also

Other csquares-class:
[`as_csquares()`](https://pepijn-devries.github.io/csquares/reference/as_csquares.md),
[`new_csquares()`](https://pepijn-devries.github.io/csquares/reference/new_csquares.md),
[`st_as_stars.csquares()`](https://pepijn-devries.github.io/csquares/reference/st_as_stars.csquares.md)

## Author

Pepijn de Vries

## Examples

``` r
validate_csquares(
  as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1")
)
#> [1] TRUE
```

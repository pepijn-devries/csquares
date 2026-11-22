# Drop c-square information from object

Drops c-square data from an object, but keeps the parent class of the
object intact. You cannot deselect the csquare column from a `csquares`
object as this will render the object invalid. Use `drop_csquares`
instead.

## Usage

``` r
drop_csquares(x, ...)
```

## Arguments

- x:

  An object of class `csquares` from which the c-square information
  needs to be dropped.

- ...:

  ignored

## Value

Returns a copy of `x` inheriting its parent classes but with out
csquares info.

## Author

Pepijn de Vries

## Examples

``` r
csq <- as_csquares("1000")
drop_csquares(csq)
#> <character[1]>
#> [1] 1000

csq <-
  data.frame(csquares = "1000", foo = "bar") |>
  as_csquares(csquares = "csquares")

drop_csquares(csq)
#>   foo
#> 1 bar
```

# Expand c-squares with wildcards to all matching c-squares

The asterisk (\*) can be used as a wildcard, for a compact notation of
csquares. `expand_wildcards` will replace all wild cards with valid
combinations of values and expands the compact notation to an explicit
notation without wildcards. Check out `vignette("wildcards")` for more
details.

## Usage

``` r
expand_wildcards(x, csquares, ...)
```

## Arguments

- x:

  A `character` string containing csquares codes with wildcards
  (asterisk character); or a `data.frame` that contains a column with
  csquares codes with wildcards

- csquares:

  When `x` is `data.frame` this argument should specify the column name
  that contains the csquares codes with wildcards.

- ...:

  ignored

## Value

Returns a `csquares` object with full notation

## Author

Pepijn de Vries

## Examples

``` r
expand_wildcards("1000:*")
#>  csquares [1:1]   4 squares
expand_wildcards("1000:***")
#>  csquares [1:1] 100 squares
expand_wildcards("1000:1**")
#>  csquares [1:1]  25 squares
expand_wildcards("1000:***:*")
#>  csquares [1:1] 400 squares
expand_wildcards(c("1000:*", "1000:***", "1000:1**", "1000:***:*"))
#>  csquares [1:4]   4 squares, 100 squares,  25 squares, 400 squares

expand_wildcards(data.frame(csq = "1000:*", foo = "bar"), csquares = "csq")
#>           csq foo
#> 1   4 squares bar
```

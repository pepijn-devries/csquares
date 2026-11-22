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

  ignored.

## Value

Returns a spatiotemporal array
([stars](https://r-spatial.github.io/stars/reference/st_as_stars.html))
object based on `x`.

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

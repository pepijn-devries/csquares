# Basic csquares methods

Basic S3 methods for handling `csquares` objects

## Usage

``` r
# S3 method for class 'csquares'
format(x, ...)

# S3 method for class 'csquares'
print(x, short = TRUE, ...)

# S3 method for class 'csquares'
as.character(x, ...)

# S3 method for class 'csquares'
summary(object, ...)

# S3 method for class 'csquares'
as.data.frame(x, ...)

# S3 method for class 'csquares'
c(...)

# S3 method for class 'csquares'
rbind(..., deparse.level = 1)

# S3 method for class 'csquares'
cbind(..., deparse.level = 1)

# S3 method for class 'csquares'
x[i, j, ..., drop = FALSE]

# S3 method for class 'csquares'
x[[i]]

# S3 method for class 'csquares'
x$name

# S3 method for class 'csquares'
x[i, j] <- value

# S3 method for class 'csquares'
x[[i]] <- value

# S3 method for class 'csquares'
x$i <- value

# S3 method for class 'csquares'
merge(x, y, ...)

# S3 method for class 'csquares'
names(x) <- value
```

## Arguments

- x, object:

  A `csquares` object to be handled by the s3 methods

- ...:

  Passed on to generic methods

- short:

  `logical` option to print `csquares` `vctrs_vec`. If `TRUE` it will
  only print one line, if `FALSE` it will print up to
  `options("max.print")` records.

- deparse.level:

  integer controlling the construction of labels in the case of
  non-matrix-like arguments (for the default method):  
  `deparse.level = 0` constructs no labels;  
  the default `deparse.level = 1` typically and `deparse.level = 2`
  always construct labels from the argument names, see the ‘Value’
  section below.

- i, j, name:

  Indices/name for selecting subsets of `x`

- drop:

  `logical` value indicating if unused dimensions should be dropped

- value:

  Replacement values for a subset. a `csquares` object or a `character`
  string that can be coerced to a `csquares` object

- y:

  A `data.frame` to be merged with `x`

## Value

Returns (a subsetted / formatted / modified version of) x

## Examples

``` r
orca_csq <- as_csquares(orca, csquares = "csquares") |> head(10)

format(orca_csq)
#>         csquares orcinus_orca
#> 1  S60 E 30 (5°)        FALSE
#> 2  S60 W 40 (5°)        FALSE
#> 3  N10 W 90 (5°)         TRUE
#> 4  N 0 W 40 (5°)        FALSE
#> 5  S10 W 70 (5°)         TRUE
#> 6  N 0 W100 (5°)         TRUE
#> 7  N40 E 30 (5°)        FALSE
#> 8  N 0 W100 (5°)         TRUE
#> 9  S 0 W 10 (5°)        FALSE
#> 10 S40 E130 (5°)         TRUE
print(orca_csq)
#>         csquares orcinus_orca
#> 1  S60 E 30 (5°)        FALSE
#> 2  S60 W 40 (5°)        FALSE
#> 3  N10 W 90 (5°)         TRUE
#> 4  N 0 W 40 (5°)        FALSE
#> 5  S10 W 70 (5°)         TRUE
#> 6  N 0 W100 (5°)         TRUE
#> 7  N40 E 30 (5°)        FALSE
#> 8  N 0 W100 (5°)         TRUE
#> 9  S 0 W 10 (5°)        FALSE
#> 10 S40 E130 (5°)         TRUE
as.character(orca_csq)
#> [1] "c(\"3603:3\", \"5604:4\", \"7109:4\", \"7004:3\", \"5107:4\", \"7010:3\", \"1403:4\", \"7010:4\", \"5001:1\", \"3413:4\")"
#> [2] "c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)"                                                       
summary(orca_csq)
#>       csquares  orcinus_orca   
#>  Length   :10   Mode :logical  
#>  N.unique :10   FALSE:5        
#>  N.blank  : 0   TRUE :5        
#>  Min.nchar: 6                  
#>  Max.nchar: 6                  
as.data.frame(orca_csq)
#>         csquares orcinus_orca
#> 1  S60 E 30 (5°)        FALSE
#> 2  S60 W 40 (5°)        FALSE
#> 3  N10 W 90 (5°)         TRUE
#> 4  N 0 W 40 (5°)        FALSE
#> 5  S10 W 70 (5°)         TRUE
#> 6  N 0 W100 (5°)         TRUE
#> 7  N40 E 30 (5°)        FALSE
#> 8  N 0 W100 (5°)         TRUE
#> 9  S 0 W 10 (5°)        FALSE
#> 10 S40 E130 (5°)         TRUE
## Concatenating and subsetting
c(orca_csq$csquares[1L:3L], orca_csq$csquares[4L:5L])
#>  csquares [1:5] S60 E 30 (5°), S60 W 40 (5°), N10 W 90 (5°), N 0 W 40 (5°),...
orca_csq$csquares[[1L]]
#>  csquares [1:1] S60 E 30 (5°)
```

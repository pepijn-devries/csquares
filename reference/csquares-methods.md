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

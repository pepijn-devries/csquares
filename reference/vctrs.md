# vctrs methods for csquares objects

Implementations to support csquare vctrs operations. There is no need to
call these functions directly.

## Usage

``` r
vec_cast.csquares(x, to, ...)

# S3 method for class 'csquares'
vec_cast.csquares(x, to, ...)

# S3 method for class 'character'
vec_cast.csquares(x, to, ...)

# Default S3 method
vec_cast.csquares(x, to, ...)

vec_ptype2.csquares(x, y, ...)

# S3 method for class 'character'
vec_ptype2.csquares(x, y, ...)

# S3 method for class 'csquares'
vec_ptype2.csquares(x, y, ...)

# Default S3 method
vec_ptype2.csquares(x, y, ..., x_arg = "x", y_arg = "y")
```

## Arguments

- x, y:

  Vector types.

- to:

  Types to cast to. If NULL, `x` will be returned as is.

- ...:

  Ignored.

- x_arg, y_arg:

  Argument names for `x` and `y`.

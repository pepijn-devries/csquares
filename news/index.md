# Changelog

## csquares v0.1.1

- Added reference to Rees (2015)
- Added
  [`vignette("boundary-cases")`](https://pepijn-devries.github.io/csquares/articles/boundary-cases.md)
- Replaced deprecated tidyverse function calls
- Fix for [issue](https://github.com/pepijn-devries/csquares/issues/38)
  [\#38](https://github.com/pepijn-devries/csquares/issues/38)
- Redesigned logo
- Organised reference page
- Cleaned code to pass `pkgcheck::pkgcheck()`
- Reduced dependency footprint
- Corrected some typos

## csquares v0.1.0

CRAN release: 2024-08-29

- Added code coverage workflow and badge
- Improved test coverage
- Reduce file size of vignettes
- Several minor fixes

## csquares v0.0.7

CRAN release: 2024-07-25

- Added functions to convert ICES rectangles
- Added tidyverse join operations
- Added vignettes:
  - [`vignette("tidy")`](https://pepijn-devries.github.io/csquares/articles/tidy.md)
  - [`vignette("csquares-objects")`](https://pepijn-devries.github.io/csquares/articles/csquares-objects.md)
  - [`vignette("ices")`](https://pepijn-devries.github.io/csquares/articles/ices.md)
- Methods that (potentially) rename columns should update the value for
  the `csquares_col` attribute. This is now fixed for:
  `names.csquares()` and
  [`cbind.csquares()`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
- Implemented `vctrs` cast mechanisms
- Better methods for showing and printing `csquares`

## csquares v0.0.6

CRAN release: 2024-07-19

- Improved support for tidyverse and base generics
  - Csquares properties are now preserved when calling tidyverse and
    base methods
- Implementation of
  [`summarise.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  is changed to better match tidyverse generic
  - The previous implementation of
    [`summarise.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
    (\<= v0.0.5) is migrated to
    [`resample_csquares()`](https://pepijn-devries.github.io/csquares/reference/resample_csquares.md)
- Several minor corrections

## csquares v0.0.5

CRAN release: 2024-07-05

- Added function to expand wildcards
- Added function to match wildcards
- Added vignette about wildcards
- Added multiple methods for base and tidyverse S3 generics
- Added self tests
- Several minor corrections

## csquares v0.0.3

CRAN release: 2024-06-24

- Initial release
- Features to encode and decode c-squares

csquares v0.0.7
-------------

  * Added experimental functions to convert ICES rectangles
  * Added tidyverse join operations
  * Added vignettes:
    * `vignette("tidy")`
    * `vignette("csquares-objects")`
    * `vignette("ices")`
  * Methods that (potentially) rename columns should update the value for the `csquares_col` attribute.
    This is now fixed for: `names.csquares()` and `cbind.csquares()`
  * Implemented `vctrs` cast mechanisms
  * Better methods for showing and printing `csquares`

csquares v0.0.6
-------------

  * Improved support for tidyverse and base generics
    * Csquares properties are now preserved when calling tidyverse and base methods
  * Implementation of `summarise.csquares()` is changed to better match
    tidyverse generic
    * The previous implementation of `summarise.csquares()` (<= v0.0.5)
      is migrated to `resample_csquares()`
  * Several minor corrections

csquares v0.0.5
-------------

  * Added function to expand wildcards
  * Added function to match wildcards
  * Added vignette about wildcards
  * Added multiple methods for base and tidyverse S3 generics
  * Added self tests
  * Several minor corrections

csquares v0.0.3
-------------

  * Initial release
  * Features to encode and decode c-squares
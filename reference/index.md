# Package index

## Creating `csquares` Class Objects

Functions for creating and maintaining `csquares` class objects

- [`as_csquares()`](https://pepijn-devries.github.io/csquares/reference/as_csquares.md)
  : Convert lon-lat coordinates into c-square codes
- [`new_csquares()`](https://pepijn-devries.github.io/csquares/reference/new_csquares.md)
  : Create a c-squares raster from a bounding box
- [`st_as_stars.csquares()`](https://pepijn-devries.github.io/csquares/reference/st_as_stars.csquares.md)
  : Coerce csqaures object into a stars object
- [`validate_csquares()`](https://pepijn-devries.github.io/csquares/reference/validate_csquares.md)
  : Test if a csquares object is valid

## Base Methods

Base methods for handling `csquares` class objects

- [`format(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`print(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`as.character(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`summary(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`as.data.frame(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`c(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`rbind(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`cbind(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `[`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `[[`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `$`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `[<-`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `[[<-`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `$<-`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`merge(`*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  [`` `names<-`( ``*`<csquares>`*`)`](https://pepijn-devries.github.io/csquares/reference/csquares-methods.md)
  : Basic csquares methods

## Advanced Handlers

More advanced handlers for `csquares` class objects

- [`drop_csquares()`](https://pepijn-devries.github.io/csquares/reference/drop_csquares.md)
  : Drop c-square information from object
- [`expand_wildcards()`](https://pepijn-devries.github.io/csquares/reference/expand_wildcards.md)
  : Expand c-squares with wildcards to all matching c-squares
- [`in_csquares()`](https://pepijn-devries.github.io/csquares/reference/in_csquares.md)
  : Match c-squares against other c-squares (with wildcards)
- [`resample_csquares()`](https://pepijn-devries.github.io/csquares/reference/resample_csquares.md)
  : Resample csquares to a different resolution

## Other Grid Encodings

Functions for handling grid encodings, other than C-Squares

- [`ices_columns()`](https://pepijn-devries.github.io/csquares/reference/ices_columns.md)
  : Valid ICES rectangle columns
- [`ices_centroids()`](https://pepijn-devries.github.io/csquares/reference/ices_geom.md)
  [`ices_rectangles()`](https://pepijn-devries.github.io/csquares/reference/ices_geom.md)
  [`ices_to_csquares()`](https://pepijn-devries.github.io/csquares/reference/ices_geom.md)
  [`ices_from_csquares()`](https://pepijn-devries.github.io/csquares/reference/ices_geom.md)
  : Get ICES geometries

## Implementation of Spatial Methods

Spatial methods enherited from the `sf` package, but adapted to handle
C-Squares objects

- [`st_as_sf.csquares()`](https://pepijn-devries.github.io/csquares/reference/st_as_sf.md)
  [`st_as_sfc.csquares()`](https://pepijn-devries.github.io/csquares/reference/st_as_sf.md)
  : Create a simple features object from c-squares

## Implementation of Tidy Operators

Tidy operators from the `dplyr` and `tidyr` package, but adapted to
handle C-Squares objects

- [`inner_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  [`left_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  [`right_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  [`full_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  [`semi_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  [`anti_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  [`st_join.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyjoins.md)
  :

  Join `csquares` objects using tidyverse conventions

- [`filter.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`select.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`as_tibble.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`arrange.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`group_by.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`ungroup.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`rowwise.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`mutate.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`rename.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`rename_with.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`slice.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`distinct.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`summarise.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`pivot_longer.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`pivot_wider.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`group_split.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`nest.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`unite.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`unnest.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`unnest.csquares_nested()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  [`drop_na.csquares()`](https://pepijn-devries.github.io/csquares/reference/tidyverse.md)
  : Tidyverse methods for csquares objects (drop the 'csquares'-suffix)

## Implementation of vctrs Operators

Operators from the `vctrs` package, but adapted to handle C-Squares
objects

- [`vec_cast.csquares()`](https://pepijn-devries.github.io/csquares/reference/vctrs.md)
  [`vec_ptype2.csquares()`](https://pepijn-devries.github.io/csquares/reference/vctrs.md)
  : vctrs methods for csquares objects

## Example Data

Data used in examples and demonstrations

- [`orca`](https://pepijn-devries.github.io/csquares/reference/orca.md)
  : Killer whale realm

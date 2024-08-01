library(sf)
library(dplyr)

nc     <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) |>
  st_transform(4326)
nc_csq <- new_csquares(nc, resolution = 1)
nc_sf1 <- nc_csq |> st_as_sf()
nc_sf2 <- nc_csq$csquares |>
  c() |>
  as_csquares() |>
  st_as_sf() |>
  st_centroid() |>
  suppressWarnings()

fine_grid <- st_bbox(c(xmin = -6, xmax = 6, ymin = -6, ymax = 6), crs = 4326) |>
  new_csquares(resolution = 0.05)

test_that("new_squares produces c-square codes that translate to coordinates in the grid", {
  expect_true({
    st_within(nc_sf2, nc_sf1) |>
      lapply(\(x) length(x) > 0) |>
      unlist() |>
      all()
  })
})

test_that("Rounded bboxes of input and output are identical", {
  nc_bbox <- st_bbox(nc)
  nc_bbox[grepl("min", names(nc_bbox))] <-
    floor(nc_bbox[grepl("min", names(nc_bbox))])
  nc_bbox[grepl("max", names(nc_bbox))] <-
    ceiling(nc_bbox[grepl("max", names(nc_bbox))])
  
  nc_csq_bbox <- st_bbox(nc_csq)
  nc_csq_bbox[grepl("min", names(nc_csq_bbox))] <-
    floor(nc_csq_bbox[grepl("min", names(nc_csq_bbox))])
  nc_csq_bbox[grepl("max", names(nc_csq_bbox))] <-
    ceiling(nc_csq_bbox[grepl("max", names(nc_csq_bbox))])
  
  expect_identical(nc_csq_bbox, nc_bbox)
})

test_that("C-squares coordinates from new_csquares form a regular grid", {
  expect_true({
    crds <- nc_sf2 |> st_coordinates()
    test <-
      (
        apply(crds, 2, \(x) {
          x |> signif(6) |> unique() |> sort() |> diff() |> sd()
        }, simplify = TRUE) < 1e-5
      ) |>
      all()
    test
  })
})

test_that("Fine grids near N0, E0 don't produce duplicated csquares", {
  expect_false({
    fine_grid |> pull("csquares") |> c() |> duplicated() |> any()
  })
})

test_that("Warning when crs is missing", {
  expect_warning({
    nc2 <- nc
    sf::st_crs(nc2) <- NA
    new_csquares(nc2, 10)
  })
})
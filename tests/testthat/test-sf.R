library(sf)
library(dplyr)

orca_csq <-
  orca_sf <-
  orca |>
  as_csquares(csquares = "csquares")

orca_sf <- {
  orca_csq |>
    st_as_sf()
}

orca_back <- orca_sf |> drop_csquares() |>
  as_csquares(resolution = 5) |> suppressWarnings()

test_that("Number of rows of input and output matches", {
  expect_equal({
    nrow(orca_sf)
  }, nrow(orca))
})

test_that("Created simple features object is valid", {
  expect_true({
    all(st_is_valid(orca_sf))
  })
})

test_that("Translating csquares to coordinates and back yield the same csquares", {
  expect_identical(orca_sf$csquares, orca_back$csquares)
})

test_that("Csquares are used from csquares object", {
  expect_s3_class(st_as_sfc(orca_csq[1:10,]), "sfc")
})

test_that("Warning when geometry is replaced", {
  expect_warning(st_as_sf(orca_sf[1:10,], "csquares", use_geometry = FALSE))
})

test_that("Warning when csquares column is not specified", {
  oc <- orca_csq
  attributes(oc)$csquares_col <-NULL
  expect_warning(st_as_sf(oc[1:10,]))
})

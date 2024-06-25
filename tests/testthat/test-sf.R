library(sf)
library(dplyr)

orca_sf <- {
  orca_sf <-
    orca |>
    as_csquares(csquares = "csquares") |>
    st_as_sf()
}

orca_back <- as_csquares(orca_sf |> select(!"csquares"), resolution = 5) |> suppressWarnings()

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
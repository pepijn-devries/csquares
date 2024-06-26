library(sf) |> suppressMessages()
library(dplyr) |> suppressMessages()

crds <-
  cbind(x = runif(100, -180, 180), y = runif(100, -90, 90))

crds_sf <- crds |> as_csquares(10) |> st_as_sf()

crds_sf2 <- crds |> apply(1, st_point, simplify = F) |> tibble() |>
  rename("geom" = 1) |> st_as_sf(crs = 4326)

codes <-
  c("7500:110:3|7500:110:1|1500:110:3|1500:110:1",
    "1715:248:478",
    "3405:371:1",
    "5010:374")

csq    <- as_csquares(codes)
csq_sf <- st_as_sf(csq)

test_that("Random global coordinates produce valid and overlapping csquares", {
  expect_true({
    cur_s2 <- sf_use_s2()
    sf_use_s2(FALSE) |> suppressMessages()
    test <- st_within(crds_sf2, crds_sf) |>
      lapply(\(x) length(x) > 0) |>
      unlist() |>
      all() |>
      suppressMessages()
    sf_use_s2(cur_s2) |> suppressMessages()
    test
  })
})

test_that("Length of input codes and output csquares are equal", {
  expect_equal({length(codes)}, {length(csq)})
})

test_that("Both input codes and output squares produce the same number of elements", {
  expect_identical({
    csq_sf$geom |> lapply(length) |> unlist()
  }, {
    strsplit(codes, "[|]") |> lapply(length) |> unlist()
  })
})

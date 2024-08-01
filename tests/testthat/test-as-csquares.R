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

test_that("A stars can be converted into a csquares object", {
  expect_true({
    orca_stars <-
      new_csquares(
        st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = 4326),
        resolution = 5)
    
    orca_stars[["orcinus_orca"]] <- NA
    orca_stars[["orcinus_orca"]][match(orca$csquares, orca_stars$csquares)] <- orca$orcinus_orca
    orca_stars$csquares_copy <- orca_stars$csquares
    orca_stars <- drop_csquares(orca_stars)
    orca_stars <- as_csquares(orca_stars)
    
    inherits(orca_stars, "csquares") && "csquares_col" %in% names(attributes(orca_stars))
  })
})

test_that("Error when coercing unsupported type to csquares", {
  expect_error({
    as_csquares(raw(0))
  })
})

test_that("Error when coercing invalid string to csquares", {
  expect_error({
    as_csquares("2000")
  })
})

test_that("Error when coercing numeric with wrong dimensions to csquares", {
  expect_error({
    as_csquares(1L)
  })
})

test_that("Expect specific number of items when converting string with wildcard", {
  expect_equal(as_csquares("*000") |> length(), 1L)
})

# test_that("Warn when csquares column is specified when coercing stars", {
#   new_csquares(sf::st_bbox(c(xmin = 0, xmax = 1, ymin = 50, ymax = 51), crs = 4326)) |> as_csquares()#TODO
# })
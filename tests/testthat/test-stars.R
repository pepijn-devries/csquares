{
  library(stars, quietly = TRUE)
  library(dplyr, quietly = TRUE)
}  |>
  suppressMessages() |> suppressWarnings()
orca_sub <-
  as_csquares(orca |>
                filter(csquares %in% c("7817:4", "7817:3", "7817:2", "7817:1")),
              csquares = "csquares")
orca_sf <- orca_sub |> sf::st_as_sf()
str1 <- st_as_stars(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
str2 <- st_as_stars(orca_sub)

test_that("`st_as_stars` produces stars objects from csquares", {
  expect_s3_class(str1, "stars")
})

test_that("`st_as_stars` produces stars objects from csquares", {
  expect_s3_class(str2, "stars")
})

test_that("`st_as_stars` cannot be created from `sf` objects", {
  expect_error({
    st_as_stars(orca_sf)
  })
})

test_that("Cannot have more that one square per row if there is more than 1 row", {
  expect_error({
    st_as_stars(as_csquares(c("7500:110:3", "7500:110:1",
                              "1500:110:3", "1500:110:1|1500:110:4")))
    })
})

test_that("Cannot use variable resolutions in squares", {
  expect_error({
    st_as_stars(as_csquares(c("7500:110:3","7500:110")))
  })
})

test_that("`stars` object is returned as is", {
  expect_identical(st_as_stars(str1), str1)
})

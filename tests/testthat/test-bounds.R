test_that("Points on csquare boundaries are handled conform Rees 2015", {
  expect_identical({
    # Test for https://github.com/pepijn-devries/csquares/issues/38
    pts <-
      data.frame(
        x = c(-180, -180, 0, 180, 180, 0.5, 0.5, -0.5, -0.5),
        y = c(90,    -90, 0, 90,  -90, 0.5, -0.5, 0.5, -0.5)
      ) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326)
    as_csquares(pts, 0.5)$csquares |> as.character()
  },
  c("7817:499:4", "5817:499:4", "1000:100:1", "1817:499:4", "3817:499:4",
    "1000:100:4", "3000:100:4", "7000:100:4", "5000:100:4"))
})
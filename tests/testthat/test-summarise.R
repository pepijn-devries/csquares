library(stars) |>
  suppressWarnings() |>
  suppressMessages()
library(dplyr) |>
  suppressMessages()

g <- {
  grd <-
    sf::st_bbox(c(xmin = 4.0, xmax = 6.5, ymin = 52.5, ymax = 53), crs = 4326) |>
    new_csquares(resolution = 0.1)
  grd[["random"]] <- grd |> dim() |> prod() |> rnorm()
  grd
}

g_sum <- {
  summarise(grd, random_sum = sum(.data$random, na.rm = TRUE), .by = "csquares")
}

test_that("All squares are summarised", {
  expect_true({
    all(
      (g_sum$csquares |> substr(1, 10) |> c() |> unique()) %in%
        (g$csquares |> substr(1, 10) |> c() |> unique())
    )
  })
})

test_that("Source and summarised data have the same sum", {
  expect_equal(sum(g$random, na.rm = TRUE), sum(g_sum$random_sum, na.rm = TRUE),
               tolerance = 1e-14)
})

library(stars) |>
  suppressWarnings() |>
  suppressMessages()
library(dplyr) |>
  suppressMessages()

g <-
    sf::st_bbox(c(xmin = 4.0, xmax = 6.5, ymin = 52.5, ymax = 53), crs = 4326) |>
    new_csquares(resolution = 0.1) |>
    ## 'exp' to generate positive values only
    ## this to avoid the tests to fail due to cancellation error
    mutate(random = .data$csquares |> length() |> rnorm() |> exp())

g_sum <-
  resample_csquares(g, method = "down", random_sum = sum(.data$random, na.rm = TRUE))

test_that("All squares are summarised", {
  expect_true({
    all(
      (g_sum$csquares |> unclass() |> substr(1, 10) |> c() |> unique()) %in%
        (g$csquares |> unclass() |> substr(1, 10) |> c() |> unique())
    )
  })
})

test_that("Source and summarised data have the same sum", {
  sum_source <- sum(g$random, na.rm = TRUE)
  sum_csq    <- sum(g_sum$random_sum, na.rm = TRUE)
  expect_equal(sum_source, sum_csq)
})
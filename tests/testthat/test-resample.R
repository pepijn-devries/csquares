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

test_that("Warning when resolution is specified for method other than 'target'", {
  expect_warning({
    resample_csquares(g_sum, method = "down",
                      resolution = 10,
                      random_sum = sum(.data$random_sum, na.rm = TRUE))
  })
})

test_that("Warning when resolution is specified for method other than 'target'", {
  expect_warning({
    resample_csquares(g_sum, method = "down",
                      resolution = 10,
                      random_sum = sum(.data$random_sum, na.rm = TRUE))
  })
})

test_that("Resampling from stars and sf produces same csquares", {
  expect_identical({
    resample_csquares(g_sum |> sf::st_as_sf(), method = "down",
                      random_sum = sum(.data$random_sum, na.rm = TRUE)) |>
      pull("csquares") |>
      unclass()  |>
      unique() |>
      sort()
  }, {
    resample_csquares(g_sum, method = "down",
                      random_sum = sum(.data$random_sum, na.rm = TRUE)) |>
      sf::st_as_sf() |>
      pull("csquares") |>
      unclass() |>
      unique() |>
      sort()
  })
})

test_that("Error when resolution is messing for method 'target'", {
  expect_error({resample_csquares(
    g_sum, method = "target",
    random_sum = sum(.data$random_sum, na.rm = TRUE))})
})

test_that("Method 'target' works as expected", {
  expect_identical({
    resample_csquares(c("1000|3000:4"), method = "target", resolution = 10) |> unclass()
  }, "1000|3000")
})

test_that("Method 'min' works as expected", {
  expect_identical({
    resample_csquares(c("1000|3000:4"), method = "min") |> unclass()
  }, "1000|3000")
})

test_that("Method 'max' works as expected", {
  expect_identical({
    resample_csquares(c("1000|3000:4"), method = "max") |> unclass()
  }, "1000:1|1000:2|1000:3|1000:4|3000:4")
})

test_that("Method 'up' works as expected", {
  expect_identical({
    resample_csquares(c("1000|3000:4"), method = "up") |> unclass()
  }, "1000:1|1000:2|1000:3|1000:4|3000:455|3000:456|3000:457|3000:458|3000:459|3000:465|3000:466|3000:467|3000:468|3000:469|3000:475|3000:476|3000:477|3000:478|3000:479|3000:485|3000:486|3000:487|3000:488|3000:489|3000:495|3000:496|3000:497|3000:498|3000:499")
})

test_that("Method 'down' works as expected", {
  expect_identical({
    resample_csquares(c("1000|3000:4"), method = "down") |> unclass()
  }, "1000|3000")
})

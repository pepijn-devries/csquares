spec_tester <- function(file, resolution) {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if(!interactive(), "This test can only be performed in interactive R sessions")
  skip_if(
    utils::winDialog("yesno", "Skip spec testing (slow)?") == "YES",
    "Skipping at user's request"
  )
  specs <-
    read.table(file, skip = 2, header = TRUE, sep = ",") |>
    dplyr::mutate(c.square.code = as.character(.data$c.square.code))
  
  test <-
    new_csquares(
      sf::st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = 4326),
      resolution = resolution
    ) |>
    dplyr::as_tibble() |>
    dplyr::left_join(specs, by = c(csquares = "c.square.code")) |>
    dplyr::mutate(
      comparison = abs(.data$x - .data$centre.longitude) +
        abs(.data$y - .data$centre.latitude) < 1e-14
    ) |>
    dplyr::pull(comparison) |>
    all()
  
  test
}

test_that(
  "10 degree rasters are conform csquare specs", {
    expect_true({
      spec_tester("https://www.cmar.csiro.au/csquares/c-squares_list_ten_deg.txt",
                  10)
    })
  })

test_that(
  "5 degree rasters are conform csquare specs", {
    expect_true({
      spec_tester("https://www.cmar.csiro.au/csquares/c-squares_list_five_deg.txt",
                  5)
    })
  })

test_that(
  "1 degree rasters are conform csquare specs", {
    expect_true({
      spec_tester("https://www.cmar.csiro.au/csquares/c-squares_list_one_deg.txt",
                  1)
    })
  })

test_that(
  "0.5 degree rasters are conform csquare specs", {
    expect_true({
      spec_tester("https://www.cmar.csiro.au/csquares/c-squares_list_point5_deg.txt",
                  0.5)
    })
  })
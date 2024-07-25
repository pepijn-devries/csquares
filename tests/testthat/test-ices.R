test_rects <- c("31F2", "32F2", "33F2", "34F2", "35F2",
                "31F3", "32F3", "33F3", "34F3", "35F3",
                "31F4", "32F4", "33F4", "34F4", "35F4")

test_that(
  "Calculated ICES rects are identical to specs",
  {
    expect_true({
      skip_on_cran()
      skip_on_ci()
      skip_if_offline()
      skip_if(!interactive(), "This test can only be performed in interactive R sessions")
      skip_if(
        utils::winDialog("yesno", "Skip ices testing (slow)?") == "YES",
        "Skipping at user's request"
      )
      fl <- tempfile(fileext = ".zip")
      td <- tempdir()
      utils::download.file("https://gis.ices.dk/shapefiles/ICES_rectangles.zip", fl,
                           quiet = TRUE)
      utils::unzip(fl, exdir = td)
      fs <- list.files(td, "\\.shp$", full.names = TRUE)
      ices <- sf::st_read(fs, quiet = TRUE)
      unlink(fl)
      fs <- list.files(td, gsub("\\.shp$", "*.?", basename(fs)), full.names = TRUE)
      unlink(fs)
      ices_test  <- ices |> dplyr::filter(ICESNAME %in% test_rects)
      ices_calc  <- ices_rectangles(test_rects)
      ices_calc2 <- ices_to_csquares(test_rects) |> sf::st_as_sf()
      ices_join <-
        ices_test |>
        dplyr::select("ICESNAME", test_geom = "geometry") |>
        dplyr::left_join(
          ices_calc |> dplyr::select(ICESNAME = "ICES", calc1_geom = "geometry") |>
            as.data.frame(),
          by = "ICESNAME"
        ) |>
        dplyr::left_join(
          ices_calc2 |> drop_csquares() |>
            dplyr::select(ICESNAME = "ICES", calc2_geom = "geom") |>
            as.data.frame(),
          by = "ICESNAME"
        ) |>
        dplyr::mutate(
          check = .data$test_geom == .data$calc1_geom
        )
      
      check <-
        lapply(seq_len(nrow(ices_join)), \(i) {
          sf::st_difference(ices_join$test_geom[[i]],
                            ices_join$calc1_geom[[i]]) |> sf::st_area() <
            testthat_tolerance() &
            sf::st_difference(ices_join$test_geom[[i]],
                              ices_join$calc2_geom[[i]]) |> sf::st_area() <
            testthat_tolerance()
        }) |> unlist() |> all()
      check
    })
  }
)

test_that(
  "Translating ICES rectangles to csquares and back to ICES rects result in the original set",
  {
    expect_true({
      csq <- ices_to_csquares(test_rects)
      ices <- ices_from_csquares(csq)
      all(ices == test_rects)
    })
  })
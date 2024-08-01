nc       <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
my_stars <- new_csquares(nc)

test_that(
  "`.all_of_class` works correctly", {
    expect_true({
      csquares:::.all_of_class("A", "B", "C", "D", "E", "F", my_class = "character")
    })
  })

test_that(
  "Error when `.no_stars` is called on a stars object", {
    expect_error({csquares:::.no_stars(my_stars, "test")})
  })

test_that(
  "Error when `.no_stars_or_char` is called on a stars object", {
    expect_error({csquares:::.no_stars_or_char(my_stars, "test")})
  })

test_that(
  "Error when using resolution beyond csquares domain", {
    expect_error({csquares:::.check_resolution(100)})
  })

test_that(
  "Error when prepping stars/data.frame but got something else", {
    expect_error({csquares:::.s3_df_stars_prep("1000", "test")})
  })

test_that(
  "Error when csquares column doesn't exist", {
    df <- data.frame()
    class(df) <- union("csquares", class(df))
    attributes(df)$csquares_col <- "dummy"
    expect_error({csquares:::.s3_df_stars_prep(df, "test")})
  })

test_that(
  "NULL geometry for invalid csquares",
  {
    expect_true({
      csq <- csquares:::.csquares_to_coords("2000")
      is.null(csq$geom[[1]])
    })
  })

{
  library(sf, quietly = TRUE)
  library(dplyr, quietly = TRUE)
} |> suppressWarnings() |> suppressMessages()

orca_sub <-
  as_csquares(orca |>
                filter(csquares %in% c("7817:4", "7817:3", "7817:2", "7817:1")),
              csquares = "csquares")

orca_sf <- orca_sub |> as_csquares(csquares = "csquares") |> st_as_sf()
orca_grid <- new_csquares(orca_sf, 5)

right_table <- data.frame(csquares = c("1000:1", "1004:1"), foobar = TRUE)

test_that("Joins throw no errors", {
  expect_no_error({
    orca_join <- left_join (orca_sf, right_table, by = "csquares")
    orca_join <- left_join (orca_sf, right_table, by = c(orcinus_orca = "foobar"))
    orca_join <- right_join (orca_sf, right_table, by = "csquares")
    orca_join <- right_join (orca_sf, right_table, by = c(orcinus_orca = "foobar"))
    orca_join <- inner_join (orca_sf, right_table, by = "csquares")
    orca_join <- inner_join (orca_sf, right_table, by = c(orcinus_orca = "foobar"))
    orca_join <- anti_join (orca_sf, right_table, by = "csquares")
    orca_join <- anti_join (orca_sf, right_table, by = c(orcinus_orca = "foobar"))
    orca_join <- semi_join (orca_sf, right_table, by = "csquares")
    orca_join <- semi_join (orca_sf, right_table, by = c(orcinus_orca = "foobar"))
    orca_join <- full_join (orca_sf, right_table, by = "csquares")
    orca_join <- full_join (orca_sf, right_table, by = c(orcinus_orca = "foobar"))
    orca_grid <- left_join (orca_grid, orca, by = "csquares")
    orca_join <- sf::st_join(orca_sf, orca_sf)
  })
})

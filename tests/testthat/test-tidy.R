library(dplyr, quietly = TRUE) |> suppressMessages()
library(tidyr, quietly = TRUE) |> suppressMessages()

orca_csq <- as_csquares(orca[1L:10L,], csquares = "csquares") |>
  mutate(quadrant = substr(csquares |> as.character(), 1L, 1L))
orca_sf <- orca_csq |> sf::st_as_sf()
pc <- c("orcinus_orca", "foobar")
orca_piv <-
  orca_csq |>
  mutate(foobar = TRUE) |>
  pivot_longer(pc)

orca_nest1 <-
  orca_csq |>
  nest(.by = "orcinus_orca")

orca_nest2 <-
  orca_csq |>
  nest(.by = "csquares")

test_that("`filter` returns correct number of rows", {
  expect_equal({
    orca_csq |> filter(orcinus_orca) |> nrow()
  }, 5L)
})

test_that("`select` returns correct number of columns", {
  expect_equal({
    orca_csq |> select(csquares) |> ncol()
  }, 1L)
})

test_that("`select` returns correct number of columns", {
  expect_equal({
    orca_csq |> select(csquares) |> ncol()
  }, 1L)
})

test_that("`as_tibble` on csqaures character returns correct number of rows", {
  expect_equal({
    orca_csq$csquares |> as_tibble() |> nrow()
  }, 10L)
})

test_that("`arrange` sorts csquares correctly", {
  expect_identical({
    orca_csq |> arrange(orcinus_orca) |> pull("orcinus_orca")
  }, c(rep(FALSE, 5L), rep(TRUE, 5L)))
})

test_that("No error when using `rowwise`", {
  expect_no_error({
    orca_csq |> rowwise() |> mutate(test = paste(csquares, orcinus_orca))
  })
})

test_that("Renaming csquares column is handled correctly", {
  expect_equal({
    oc <-
      orca_csq |>
      rename(csq = "csquares")
    attributes(oc)$csquares_col
  }, "csq")
})

test_that("`rename_with` works as expected", {
  expect_equal({
    oc <-
      orca_csq |>
      rename_with(~paste0(., "_test"), any_of("csquares"))
    attributes(oc)$csquares_col
  }, "csquares_test")
})

test_that("`slice` works as expected", {
  expect_identical(slice(orca_csq, 1L:2L), orca_csq[1L:2L,])
})

test_that("`distinct` works as expected", {
  expect_equal(orca_csq[c(1L, 1L, 1L, 2L, 2L),] |> distinct() |> nrow(), 2L)
})

test_that("`summarise` works on csquares sf", {
  expect_identical(
    orca_sf |> group_by(orcinus_orca) |> summarise(n = n()) |> pull("n"),
    c(5L, 5L)
  )
})

test_that("Error when pivotting csquares column", {
  expect_error({
    orca_csq |>
      pivot_longer("csquares")
  })
})

test_that("`pivot_longer` works as expected", {
  expect_true({
    inherits(orca_piv, "csquares") &&
      nrow(orca_piv) == 20L && all(orca_piv$name %in% pc)
  })
})

test_that("`pivot_wider` works as expected", {
  expect_true({
    op <-
      pivot_wider(orca_piv, names_from = "name", values_from = "value")
    inherits(op, "csquares") &&
      nrow(op) == 10L && all(op$foobar)
  })
})

test_that("`pivot_wider` works as expected", {
  expect_true({
    op <-
      pivot_wider(orca_sf, names_from = "orcinus_orca", values_from = "csquares",
                  values_fn = list)
    nrow(op) == 10L && ncol(op) == 5L && inherits(op, "csquares")  && inherits(op, "sf")
  })
})

test_that("`group_split` works as expected", {
  expect_true({
    gs <- group_split(orca_csq, orcinus_orca)
    length(gs) == 2L && nrow(gs[[1]]) == 5L &&
      nrow(gs[[2]]) == 5L && inherits(gs[[1]], "csquares")
  })
})

test_that("`nest` works as expected", {
  expect_true({
    nrow(orca_nest1) == 2L && inherits(orca_nest1, "csquares_nested") &&
      nrow(orca_nest2) == 10L &&  inherits(orca_nest2, "csquares")
  })
})

test_that("`unnest` works as expected", {
  expect_identical({
    unnest(orca_nest1, cols = "data") |>
      select(any_of(names(orca_csq))) |>
      arrange(csquares)
  }, {
    orca_csq  |>
      arrange(csquares) |>
      as_tibble()
  })
})

test_that("`unnest` works as expected", {
  expect_identical({
    unnest(orca_nest2, cols = "data") |>
      select(any_of(names(orca_csq))) |>
      arrange(csquares)
  }, {
    orca_csq  |>
      arrange(csquares) |>
      as_tibble()
  })
})

test_that("`unite` works as expected", {
  expect_true({
    un <- unite(orca_sf, "quad_realm", any_of(c("quadrant", "orcinus_orca")))
    inherits(un, c("sf", "csquaress"))
  })
})

test_that("`drop_na` works as expected", {
  expect_identical({
    orca_na <- orca_csq
    orca_na$csquares[[3L]] <- NA
    orca_na <- drop_na(orca_na)
    rownames(orca_na) <- (1L:10L)[-3L]
    orca_na
  },
  orca_csq[-3L,])
})

test_that("`ungroup` works as expected", {
  expect_true({
    og <- orca_csq |> group_by(orcinus_orca)
    ug <- og |> ungroup()
    is_grouped_df(og) && !is_grouped_df(ug) && inherits(ug, "csquares")
  })
})
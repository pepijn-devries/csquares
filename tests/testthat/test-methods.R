orca_csq <- orca[1:10,] |> as_csquares(csquares = "csquares")

test_that("`format` formats csquares correctly", {
  expect_identical(format.csquares(as_csquares("1000")),
                   "N 0 E  0 (10°)")
})

test_that("`format` formats csquares correctly", {
  expect_identical(format.csquares(as_csquares(NA_character_)),
                   "empty")
})

test_that("`format` formats csquares correctly", {
  expect_identical(format.csquares(as_csquares("1000|3000")),
                   "  2 squares")
})

test_that("Print long works without", {
  expect_no_error({
    print.csquares(as_csquares("1000"), short = TRUE) |>
      capture.output()
    print.csquares(as_csquares("1000"), short = FALSE) |>
      capture.output()
  })
})

test_that("Nextmethod works on as.character", {
  expect_no_error({as.character(orca_csq)})
})

test_that("Nextmethod works on format data.frame", {
  expect_no_error({format(orca_csq)})
})

test_that("Nextmethod works on print vctrs", {
  expect_no_error({print(orca_csq$csquares) |> capture.output()})
})

test_that("Nextmethod works on print data.frame", {
  expect_no_error({print(orca_csq) |> capture.output()})
})

test_that("No error when calling as.data.frame on data.frame", {
  expect_no_error({as.data.frame(orca_csq)})
})

test_that("No error when calling summary", {
  expect_no_error({summary(orca_csq); summary(as_csquares("1000"))})
})

test_that("bind rows gives correct number of rows", {
  expect_identical(rbind.csquares(orca_csq, orca_csq) |> nrow(), 20L)
})

test_that("bind rows gives correct number of rows", {
  expect_identical(rbind.csquares(orca_csq, NULL) |> nrow(), 10L)
})

test_that("bind cols gives correct number of rows", {
  expect_identical(cbind.csquares(orca_csq, foo = "bar") |> ncol(), 3L)
})

test_that("Error when bind cols with NULL", {
  expect_error({cbind.csquares(orca_csq, NULL)})
})

test_that("merge gives correct number of rows", {
  expect_identical(
    nrow(
      merge(orca_csq, data.frame(orcinus_orca = TRUE, foo = "bar"),
            "orcinus_orca")
    ), 5L)
})

test_that("concat works ok", {
  expect_identical({c(orca_csq$csquares[[1]], orca_csq$csquares[[2]])},
                   orca_csq$csquares[1:2])
})

test_that("concat works ok with mixed types", {
  expect_identical({
    c(orca_csq$csquares[[1]], as.character(orca_csq$csquares[[2]]))
  }, orca_csq$csquares[1:2])
})

test_that("concat of data.frames is the the same as rbind", {
  expect_identical(
    c(orca_csq, orca_csq),
    rbind(orca_csq, orca_csq))
})

test_that("Error when x is of wrong type", {
  expect_error({in_csquares(1, "100*")})
})

test_that("Error when y is invalid", {
  expect_error({in_csquares("1000", "200*")})
})

test_that("Error when x is of wrong type", {
  expect_false({
    orca[1:10,] |> as_csquares(csquares = "csquares") |>
      in_csquares("1000:*") |> all()
  })
})

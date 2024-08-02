library(vctrs, quietly = TRUE) |>
  suppressMessages()
csq <- as_csquares("1000")

test_that("vec_ptype2 on csquares char returns correct class", {
  expect_s3_class(vec_ptype2.csquares(csq, ""), "csquares")
})

test_that("vec_ptype2 on csquares char specific returns correct class", {
  expect_s3_class(csquares:::vec_ptype2.csquares.character(csq, ""), "csquares")
})

test_that("vec_ptype2 on csquares returns correct class", {
  expect_s3_class(csquares:::vec_ptype2.csquares.default(csq, csq), "csquares")
})

test_that("vctrs vec is correctly cast to character", {
  expect_identical(vec_cast.csquares(csq, ""), as.character(csq))
})

test_that("vctrs vec is correctly cast to csquares", {
  expect_identical(vec_cast.csquares(csq, as_csquares(as.character(NA))), csq)
})

test_that("No errors when calling s3 specific implementations", {
  csquares:::vec_cast.csquares.character("1000", as_csquares(as.character(NA)))
  csquares:::vec_cast.csquares.csquares(as_csquares("1000"), as_csquares(as.character(NA)))
  csquares:::vec_cast.csquares.default(as_csquares("1000"), as_csquares(as.character(NA)))
})

test_that("Error when csquares is not a vctrs", {
  expect_error({
    as_csquares(orca[1:10,], csquares = "csquares") |>
      csquares:::vec_cast.csquares.character("")
  })
})

test_that("Error when csquares is not a vctrs", {
  expect_error({
    as_csquares(orca[1:10,], csquares = "csquares") |>
      csquares:::vec_cast.csquares.default(1L)
  })
})

test_that("Error when csquares is not a vctrs", {
  expect_error({
    as_csquares(orca[1:10,], csquares = "csquares") |>
      csquares:::vec_cast.csquares.csquares(1L)
  })
})

test_that("Error when not of type character", {
  expect_error({
    csquares:::.vec_char_only(1L)
  })
})

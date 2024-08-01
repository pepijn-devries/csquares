orca_csq <- as_csquares(orca[1:10,], csquares = "csquares")

test_that(
  "Error when validating non-csquares object",{
    expect_error({validate_csquares("1000")})
  })

test_that(
  "csquares data.frame from first 10 orca records is valid", {
    expect_true({validate_csquares(orca_csq)})
  })
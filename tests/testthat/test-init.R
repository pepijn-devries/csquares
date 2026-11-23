test_that(
  "Init runs without error", {
    expect_no_error({
      .onLoad("csquares", "csquares")
    })
  })

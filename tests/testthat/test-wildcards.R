test_that("Applying similar wildcards produce the same number of hits", {
  expect_true({
    test <- table(in_csquares(orca$csquares, "1***:*"))["TRUE"]
    check <- test == table(in_csquares(orca$csquares, "1***", strict = FALSE))["TRUE"]
    check <- check &&
      test == table(in_csquares(orca$csquares, "1***:*", strict = TRUE))["TRUE"]
    check
  })
})

test_that("Strict comparison with non-matching wildcard returns no hits", {
  expect_true({
    sum(in_csquares(orca$csquares, "1***", strict = TRUE)) == 0
  })
})

test_that("Expand wildcards produces expected number of cells for 1 quadrant", {
  expect_equal({
    expand_wildcards("1***") |> strsplit("[|]") |> unlist() |> length()
  }, 18*9)
})

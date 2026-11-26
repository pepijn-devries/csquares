test_that("Source code should not have things on TODO list", {
  skip_on_ci()
  skip_on_cran()
  expect_false({
    skip_if(length(unclass(packageVersion("csquares"))[[1]]) > 3,
            "Skipping during development")
    r_path <- normalizePath(test_path()) |> dirname() |> dirname()
    files_to_check <- list.files(
      r_path,
      pattern = ".[r|R]$|NEWS|DESCRIPTION|README\\.md",
      recursive = TRUE, full.names = TRUE)
    files_to_check <- files_to_check[!endsWith(files_to_check, "test-dev.R")]
    any(
      unlist(
        lapply(files_to_check, function(file) {
          content <- suppressWarnings(readLines(file))
          result  <- grepl("TODO", content, fixed = TRUE) &
            !grepl("grepl\\(\"TODO\"", content, fixed = TRUE) &
            !grepl("on TODO list", content, fixed = TRUE)
          if (any(result)) {
            warning(sprintf("File `%s` has items on TODO list at lines `%s`",
                            file, paste(which(result), collapse = "`, `")))
          }
          any(result)
        })
      )
    )
  })
})

test_that("Last NEWS entry version matches installed version", {
  skip_on_cran()
  expect_true({
    news_heading <- readLines(system.file("NEWS.md", package = "csquares"), 1L)
    new_vsn <- regmatches(news_heading,
                          regexpr("[0-9]+(\\.[0-9]+)+", news_heading))
    utils::packageVersion("csquares") == new_vsn
  })
})
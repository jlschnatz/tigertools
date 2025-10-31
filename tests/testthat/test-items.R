test_that("Formatting of items is fine!", {
  temp_dir <- tempdir()
  unzip("testdata/mock.zip", exdir = temp_dir)
  setwd(file.path(temp_dir, "items"))
  files <- list.files(pattern = "\\.md$", full.names = TRUE)
  sapply(files, function(x) {
    expect_true(suppressMessages(validate_item(x)))
  })
})

test_that("Formatting of items is fine!", {
  temp_dir <- tempdir()
  unzip("testdata/mock.zip", exdir = temp_dir)
  setwd(temp_dir)
  files <- list.files("items/", pattern = "\\.md$", full.names = TRUE)
  sapply(files, function(x) {
    expect_true(suppressMessages(validate_item(x)))
  })
})

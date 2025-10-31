test_that("Error when required files are missing", {
  temp_dir <- tempdir()
  setwd(temp_dir)
  dir.create(file.path(temp_dir, "items"))
  expect_error(suppressMessages(create(open = FALSE)))
  on.exit({
    unlink(file.path(temp_dir, "items"), recursive = TRUE)
  })
})


test_that("Creation of new item works", {
  temp_dir <- tempdir()
  setwd(temp_dir)
  file.create(file.path(temp_dir, "test.Rproj"))
  dir.create(file.path(temp_dir, "items"))
  file.create(file.path(temp_dir, "items", "tiger_item_001.md"))
  suppressMessages(create(open = FALSE))
  new_files <- dir(file.path(temp_dir, "items"), pattern = "tiger_item_\\d+\\.md")
  expect_true(length(new_files) == 2)
  expect_true("tiger_item_002.md" %in% new_files)
  on.exit({
    file.remove("test.Rproj")
    unlink(file.path(temp_dir, "items"), recursive = TRUE)
  })
})

test_that("Creation of new item with R-file works", {
  temp_dir <- tempdir()
  setwd(temp_dir)
  file.create(file.path(temp_dir, "test.Rproj"))
  dir.create(file.path(temp_dir, "items"))
  file.create(file.path(temp_dir, "items", "tiger_item_001.md"))
  suppressMessages(create(open = FALSE, r_file = "new_script.R"))
  new_files <- dir(file.path(temp_dir, "items"), pattern = "tiger_item_\\d+\\.md")
  expect_true(length(new_files) == 2)
  expect_true("tiger_item_002.md" %in% new_files)
  expect_true(file.exists(file.path(temp_dir, "data-raw", "new_script.R")))
  on.exit({
    file.remove("test.Rproj")
    unlink(file.path(temp_dir, "items"), recursive = TRUE)
    unlink(file.path(temp_dir, "data-raw"), recursive = TRUE)
  })
})
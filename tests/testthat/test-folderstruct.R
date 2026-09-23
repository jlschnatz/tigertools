test_that("check_folderstruct() passes in a project folder with items/", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  expect_true(dir.exists("items/www"))
  expect_no_error(check_folderstruct())
})

test_that("check_folderstruct() errors without an .Rproj file or items/ folder", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  file.remove("mock.Rproj")
  expect_error(check_folderstruct(), "not located in the correct folder")
  file.create("mock.Rproj")
  unlink("items", recursive = TRUE)
  expect_error(check_folderstruct(), "items")
})

test_that("all mock items (pre-numeric MC files) still validate", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  files <- list.files("items", pattern = "\\.md$", full.names = TRUE)
  expect_gt(length(files), 0)
  for (f in files) {
    expect_true(suppressMessages(validate_item(f)), label = f)
  }
})

test_that("Correct folder structure.", {
  root <- create_mock_structure()
  setwd(root)
  expect_true(file.exists(".Rproj"))
  file.remove(".Rproj")
  expect_true(dir.exists("items"))
  expect_true(dir.exists("items/www"))
  expect_error(check_rproj())
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
})



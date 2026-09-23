local_project <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(root, .local_envir = env)
  file.create("test.Rproj")
  dir.create("items")
  root
}

test_that("Error when required files are missing", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  dir.create("items")
  expect_error(suppressMessages(create(open = FALSE)))
})

test_that("Creation of new item works", {
  local_project()
  file.create(file.path("items", "tiger_item_001.md"))
  suppressMessages(create(open = FALSE))
  new_files <- dir("items", pattern = "tiger_item_\\d+\\.md")
  expect_length(new_files, 2)
  expect_true("tiger_item_002.md" %in% new_files)
  lines <- readLines("items/tiger_item_002.md", encoding = "UTF-8")
  expect_equal(lines[grep("^# id_item", lines) + 1], "2")
  expect_equal(lines[grep("^# answer_mode", lines) + 2], "mc")
})

test_that("Creation of new item with R-file works", {
  local_project()
  file.create(file.path("items", "tiger_item_001.md"))
  suppressMessages(create(open = FALSE, r_file = "new_script.R"))
  new_files <- dir("items", pattern = "tiger_item_\\d+\\.md")
  expect_length(new_files, 2)
  expect_true("tiger_item_002.md" %in% new_files)
  expect_true(file.exists(file.path("data-raw", "new_script.R")))
})

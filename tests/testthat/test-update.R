read_item_db <- function(path = "db_item.sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "item_db")
}

test_that("update_db() rebuilds CSV + DB from all item files, keeping unmanaged DB columns", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)

  # Simulate the downstream IRT step having added a column to the DB
  con <- DBI::dbConnect(RSQLite::SQLite(), "db_item.sqlite")
  DBI::dbExecute(con, "ALTER TABLE item_db ADD COLUMN irt_diff REAL")
  DBI::dbExecute(con, "UPDATE item_db SET irt_diff = 0.25")
  ids_before <- as.integer(DBI::dbGetQuery(con, "SELECT id_item FROM item_db")$id_item)
  DBI::dbDisconnect(con)

  f <- write_item_md(num_item_fields(id_item = "997"), path = "items/tiger_item_997.md")
  suppressMessages(update_db(f))

  md_ids <- as.integer(sub("tiger_item_(\\d+)\\.md", "\\1", list.files("items", "^tiger_item_\\d{3}\\.md$")))
  csv <- .read_item_csv("data_item_tiger.csv")
  expect_setequal(csv$id_item, md_ids)
  expect_false(is.unsorted(csv$id_item))
  expect_equal(csv$answer_correct[csv$id_item == 997L], "1;2")

  db <- read_item_db()
  expect_true(all(md_ids %in% db$id_item))
  expect_true(all(ids_before %in% db$id_item)) # nothing dropped
  # rows that existed before keep their IRT value; newly inserted ones have none
  expect_true(all(db$irt_diff[db$id_item %in% ids_before] == 0.25))
  expect_true(all(is.na(db$irt_diff[!db$id_item %in% ids_before])))
  expect_false(997L %in% ids_before)
  expect_equal(db$answer_mode[db$id_item == 997L], "num")
})

test_that("update_db() writes nothing when the given item is invalid", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  csv_before <- readLines("data_item_tiger.csv")

  f <- write_item_md(num_item_fields(id_item = "996", answer_correct = "7"), path = "items/tiger_item_996.md")
  expect_error(suppressMessages(update_db(f)), "requirements are not met")
  expect_identical(readLines("data_item_tiger.csv"), csv_before)
})

test_that("push_all() pushes every item file; without overwrite it stops at the first existing item", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  write_item_md(num_item_fields(id_item = "995"), path = "items/tiger_item_995.md")
  md_ids <- as.integer(sub("tiger_item_(\\d+)\\.md", "\\1", list.files("items", "\\.md$")))

  expect_error(suppressMessages(push_all()), "already exists")

  suppressMessages(push_all(overwrite = TRUE))
  csv <- .read_item_csv("data_item_tiger.csv")
  expect_true(all(md_ids %in% csv$id_item))
  expect_false(anyDuplicated(csv$id_item) > 0)
  expect_true(995L %in% read_item_db()$id_item)
})

test_that("parse_csv_to_md() writes item files that parse back to the same item", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  dir.create("items")
  src <- write_item_md(num_item_fields(id_item = "7"))
  item <- .as_item_schema(parse_md_to_csv(src))

  suppressMessages(parse_csv_to_md(item))
  expect_true(file.exists("items/tiger_item_007.md"))
  back <- .as_item_schema(parse_md_to_csv("items/tiger_item_007.md"))
  expect_equal(back, item, ignore_attr = TRUE)
  expect_true(suppressMessages(validate_item("items/tiger_item_007.md")))
})

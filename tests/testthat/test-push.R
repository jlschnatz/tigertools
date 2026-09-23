test_that(".upsert_items keeps columns and rows it doesn't manage, adds missing schema columns", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  # An "old" item_db: pre-numeric schema subset plus downstream IRT columns
  DBI::dbWriteTable(con, "item_db", data.frame(
    id_item = c(1L, 2L), learning_area = c("Regression", "Regression"),
    answer_correct = c("1", "2"), irt_diff = c(0.5, -0.3), irt_discr = c(1.1, 0.9)
  ))
  DBI::dbDisconnect(con)

  new <- .as_item_schema(data.frame(
    id_item = c("2", "3"), learning_area = c("Deskriptivstatistik", "Wahrscheinlichkeit"),
    answer_correct = c("1;2", "1"), answer_mode = c("num", "num"),
    answeroption_01 = c("6,67", "5"), lower_answeroption_01 = c("6.66", NA),
    upper_answeroption_01 = c("6.67", NA)
  ))
  suppressMessages(.upsert_items(new, db))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(con))
  out <- DBI::dbGetQuery(con, "SELECT * FROM item_db ORDER BY id_item")
  expect_equal(out$id_item, 1:3)
  # untouched row and downstream columns survive
  expect_equal(out$learning_area[1], "Regression")
  expect_equal(out$irt_diff, c(0.5, -0.3, NA))
  expect_equal(out$irt_discr, c(1.1, 0.9, NA))
  # updated / inserted rows carry the new values and columns
  expect_equal(out$learning_area[2:3], c("Deskriptivstatistik", "Wahrscheinlichkeit"))
  expect_equal(out$answer_correct, c("1", "1;2", "1"))
  expect_equal(out$answeroption_01[2], "6.67")
  expect_equal(out$lower_answeroption_01[2], 6.66)
  expect_true(all(c(.bound_cols(), "answer_mode") %in% names(out)))
  types <- DBI::dbGetQuery(con, "SELECT name, type FROM pragma_table_info('item_db')")
  expect_equal(types$type[types$name == "lower_answeroption_01"], "REAL")
})

test_that(".upsert_items creates item_db with schema types when missing", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  suppressMessages(.upsert_items(.as_item_schema(data.frame(id_item = "7", answer_correct = "1;3")), db))
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(con))
  types <- DBI::dbGetQuery(con, "SELECT name, type FROM pragma_table_info('item_db')")
  expect_equal(types$type[types$name == "id_item"], "INTEGER")
  expect_equal(types$type[types$name == "answer_correct"], "TEXT")
  expect_equal(DBI::dbGetQuery(con, "SELECT answer_correct FROM item_db")$answer_correct, "1;3")
})

test_that("push() adds a numeric item to an existing CSV + DB, keeping answer_correct as text", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  n_before <- nrow(.read_item_csv("data_item_tiger.csv"))

  f <- write_item_md(num_item_fields(id_item = "999"), path = "items/tiger_item_999.md")
  suppressMessages(push(f))

  csv <- .read_item_csv("data_item_tiger.csv")
  expect_equal(nrow(csv), n_before + 1L)
  expect_equal(csv$answer_correct[csv$id_item == 999L], "1;2")
  expect_equal(csv$answer_mode[csv$id_item == 999L], "num")
  expect_true(all(csv$answer_mode[csv$id_item != 999L] == "mc"))

  con <- DBI::dbConnect(RSQLite::SQLite(), "db_item.sqlite")
  withr::defer(DBI::dbDisconnect(con))
  row <- DBI::dbGetQuery(con, "SELECT answer_correct, lower_answeroption_04 FROM item_db WHERE id_item = 999")
  expect_equal(row$answer_correct, "1;2")
  expect_equal(row$lower_answeroption_04, 2.58)

  # pushing again without overwrite refuses, with overwrite replaces
  expect_error(suppressMessages(push(f)), "already exists")
  suppressMessages(push(f, overwrite = TRUE, confirm = FALSE))
  expect_equal(nrow(.read_item_csv("data_item_tiger.csv")), n_before + 1L)
})

test_that("push_recent() pushes only files newer than the database", {
  root <- create_mockstruct()
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_dir(root)
  Sys.setFileTime("db_item.sqlite", Sys.time() - 60)
  Sys.setFileTime(list.files("items", "\\.md$", full.names = TRUE), Sys.time() - 120)
  write_item_md(num_item_fields(id_item = "998"), path = "items/tiger_item_998.md")

  suppressMessages(push_recent())
  expect_true(998L %in% .read_item_csv("data_item_tiger.csv")$id_item)
})

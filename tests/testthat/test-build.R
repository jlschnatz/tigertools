# build_release(): pool from scratch + difficulties + images

# A fresh item project: items/ with the given items (named list of field
# lists, names = ids), items/www/ with one image, difficulty CSVs. Returns
# its path; the test's working directory is set to it.
local_build_project <- function(items, calibrated, provisional, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "items", "www"), recursive = TRUE)
  writeBin(as.raw(1:10), file.path(root, "items", "www", "plot.png"))
  writeBin(as.raw(1:10), file.path(root, "items", "www", "unused.png"))
  for (id in names(items)) {
    write_item_md(items[[id]], path = file.path(root, "items", sprintf("tiger_item_%03d.md", as.integer(id))))
  }
  dir.create(file.path(root, "difficulty"))
  utils::write.csv(calibrated, file.path(root, "difficulty", "calibrated.csv"), row.names = FALSE)
  utils::write.csv(provisional, file.path(root, "difficulty", "provisional.csv"), row.names = FALSE)
  withr::local_dir(root, .local_envir = env)
  root
}

read_pool <- function(file = "release/db_item.sqlite", table = "item_db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", table))
}

three_items <- function() {
  list(
    "1" = mc_item_fields(id_item = "1", stimulus_image = "www/plot.png", type_stimulus = "image"),
    "2" = mc_item_fields(id_item = "2"),
    "3" = num_item_fields(id_item = "3")
  )
}

test_that("build_release merges calibrated and provisional difficulties, calibrated wins", {
  local_build_project(
    three_items(),
    # the calibration's format
    calibrated = data.frame(id_item = c(1, 2), difficulty_median = c(0.5, -1.2)),
    # the LLM estimates' format; 2 is in both, 99 isn't in the pool
    provisional = data.frame(item_id = c(2, 3, 99), solution_probability = 0.5, difficulty_logit = c(0.1, 0.3, 2))
  )
  suppressMessages(build_release())
  pool <- read_pool()
  expect_equal(pool$id_item, 1:3)
  expect_equal(pool$irt_diff, c(0.5, -1.2, 0.3))
  expect_equal(pool$irt_provisional, c(0L, 0L, 1L))
  expect_equal(pool$answer_correct, c("1", "1", "1;2"))
  expect_equal(pool$answer_mode, c("mc", "mc", "num"))
  expect_equal(pool$lower_answeroption_01[3], 6.66)

  types <- read_pool(table = "pragma_table_info('item_db')")
  expect_equal(types$type[match(c("id_item", "irt_diff", "irt_provisional", "answer_correct", "upper_answeroption_01"), types$name)],
               c("INTEGER", "REAL", "INTEGER", "TEXT", "REAL"))
})

test_that("build_release copies only the images items use and replaces img_item/", {
  local_build_project(
    three_items(),
    calibrated = data.frame(id_item = 1:3, difficulty_median = 0),
    provisional = data.frame(item_id = integer(0), difficulty_logit = numeric(0))
  )
  dir.create("release/img_item", recursive = TRUE)
  file.create("release/img_item/stale.png")
  suppressMessages(build_release())
  expect_equal(list.files("release/img_item"), "plot.png")
  expect_equal(list.files("release"), c("db_item.sqlite", "img_item")) # no temp leftovers
})

test_that("build_release is reproducible and records its inputs", {
  local_build_project(
    three_items(),
    calibrated = data.frame(id_item = 1:2, difficulty_median = 0),
    provisional = data.frame(item_id = 3, difficulty_logit = 1)
  )
  suppressMessages(build_release("a"))
  suppressMessages(build_release("b"))
  expect_equal(unname(tools::md5sum("a/db_item.sqlite")), unname(tools::md5sum("b/db_item.sqlite")))

  info <- read_pool("a/db_item.sqlite", "build_info")
  expect_equal(info$input, c("items", "calibrated", "provisional", "tigertools"))
  expect_equal(info$n_items, c(3, 2, 1, NA))
  expect_match(info$file[4], "^\\d+\\.\\d+\\.\\d+")

  # changing an item changes the items hash
  write_item_md(mc_item_fields(id_item = "2", stimulus_text = "Anders."), path = "items/tiger_item_002.md")
  suppressMessages(build_release("c"))
  expect_false(identical(info$md5[1], read_pool("c/db_item.sqlite", "build_info")$md5[1]))
})

test_that("a deleted item file is gone from the pool (built from scratch)", {
  local_build_project(
    three_items(),
    calibrated = data.frame(id_item = 1:3, difficulty_median = 0),
    provisional = data.frame(item_id = integer(0), difficulty_logit = numeric(0))
  )
  suppressMessages(build_release())
  file.remove("items/tiger_item_002.md")
  suppressMessages(build_release())
  expect_equal(read_pool()$id_item, c(1L, 3L))
})

test_that("build_release stops on items without a difficulty unless allow_missing", {
  local_build_project(
    three_items(),
    calibrated = data.frame(id_item = 1, difficulty_median = 0),
    provisional = data.frame(item_id = 2, difficulty_logit = 0)
  )
  expect_error(suppressMessages(build_release()), "without a difficulty")
  expect_false(file.exists("release/db_item.sqlite"))

  suppressMessages(build_release(allow_missing = TRUE))
  pool <- read_pool()
  expect_true(is.na(pool$irt_diff[3]))
  expect_true(is.na(pool$irt_provisional[3]))
})

test_that("build_release lists every invalid item file and writes nothing", {
  items <- three_items()
  items[["2"]] <- mc_item_fields(id_item = "2", learning_area = "Astrophysik")
  items[["3"]] <- num_item_fields(id_item = "3", answer_correct = "7")
  local_build_project(
    items,
    calibrated = data.frame(id_item = 1:3, difficulty_median = 0),
    provisional = data.frame(item_id = integer(0), difficulty_logit = numeric(0))
  )
  err <- expect_error(suppressMessages(build_release()), "2 item files failed validation")
  expect_match(conditionMessage(err), "tiger_item_002.md")
  expect_match(conditionMessage(err), "tiger_item_003.md")
  expect_false(file.exists("release/db_item.sqlite"))
})

test_that("build_release rejects duplicate ids, bad difficulty files and the app's DB folder", {
  items <- three_items()
  local_build_project(
    items,
    calibrated = data.frame(id_item = 1:3, difficulty_median = 0),
    provisional = data.frame(item_id = integer(0), difficulty_logit = numeric(0))
  )
  write_item_md(mc_item_fields(id_item = "2"), path = "items/tiger_item_002_copy.md")
  expect_error(suppressMessages(build_release()), "same .*id_item")
  file.remove("items/tiger_item_002_copy.md")

  utils::write.csv(data.frame(id = 1, b = 0), "difficulty/bad.csv", row.names = FALSE)
  expect_error(suppressMessages(build_release(calibrated = "difficulty/bad.csv")), "needs an id column")
  utils::write.csv(data.frame(id_item = c(1, 1), irt_diff = 0), "difficulty/dup.csv", row.names = FALSE)
  expect_error(suppressMessages(build_release(calibrated = "difficulty/dup.csv")), "more than once")

  dir.create("appdb")
  file.create("appdb/db_user.sqlite")
  expect_error(suppressMessages(build_release("appdb")), "app's DB folder")

  # provisional = NULL: calibrated only
  suppressMessages(build_release(provisional = NULL))
  expect_equal(read_pool()$irt_provisional, c(0L, 0L, 0L))
})

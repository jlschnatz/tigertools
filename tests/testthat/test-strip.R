# dev/strip_mc_skip.R is a one-off script, not part of the package; these tests
# only run from the source tree (dev/ is not in the built package).

skip_fb <- "Alles klar! Du hast die Aufgabe übersprungen."

load_strip_mc_skip <- function() {
  path <- testthat::test_path("..", "..", "dev", "strip_mc_skip.R")
  testthat::skip_if_not(file.exists(path), "dev/strip_mc_skip.R not available")
  env <- new.env()
  assign(".strip_mc_skip_sourced_only", TRUE, envir = env)
  sys.source(path, envir = env)
  env$strip_mc_skip
}

# An item folder with one file per case, named by case
local_skip_items <- function(cases, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "items"))
  for (nm in names(cases)) {
    write_item_md(cases[[nm]], path = file.path(root, "items", paste0(nm, ".md")))
  }
  withr::local_dir(root, .local_envir = env)
}

test_that("strip_mc_skip empties the skip option and its feedback, nothing else", {
  strip_mc_skip <- load_strip_mc_skip()
  local_skip_items(list(
    text = mc_item_fields(answeroption_06 = "Frage überspringen.", if_answeroption_06 = skip_fb),
    image = mc_item_fields(answeroption_04 = "www/skip.png", if_answeroption_04 = skip_fb),
    clean = mc_item_fields(),
    num = num_item_fields()
  ))
  before <- lapply(list.files("items", full.names = TRUE), readLines)
  names(before) <- list.files("items")

  expect_length(suppressMessages(strip_mc_skip()), 2L) # dry run
  expect_identical(lapply(list.files("items", full.names = TRUE), readLines), unname(before))

  changed <- suppressMessages(strip_mc_skip(apply = TRUE))
  expect_setequal(basename(changed), c("text.md", "image.md"))
  backup <- list.files(".", "^items_backup_")
  expect_length(backup, 1L)

  for (f in c("text.md", "image.md")) {
    path <- file.path("items", f)
    expect_true(suppressMessages(validate_item(path)), label = f)
    expect_identical(readLines(file.path(backup, f)), before[[f]])
    x <- .as_item_schema(parse_md_to_csv(path))
    old <- .as_item_schema(parse_md_to_csv(file.path(backup, f)))
    cleared <- if (f == "text.md") "06" else "04"
    expect_true(is.na(x[[paste0("answeroption_", cleared)]]))
    expect_true(is.na(x[[paste0("if_answeroption_", cleared)]]))
    same <- setdiff(names(x), paste0(c("answeroption_", "if_answeroption_"), cleared))
    expect_equal(x[, same], old[, same])
    # Only the two section bodies changed
    expect_length(setdiff(before[[f]], readLines(path)), 2L)
  }
  expect_identical(readLines("items/clean.md"), before[["clean.md"]])
  expect_identical(readLines("items/num.md"), before[["num.md"]])

  expect_length(suppressMessages(strip_mc_skip(apply = TRUE)), 0L) # idempotent
  expect_length(list.files(".", "^items_backup_"), 1L)
})

test_that("strip_mc_skip keeps CRLF line endings and the final newline", {
  strip_mc_skip <- load_strip_mc_skip()
  local_skip_items(list(crlf = mc_item_fields(answeroption_06 = "Frage überspringen.", if_answeroption_06 = skip_fb)))
  f <- "items/crlf.md"
  lf <- readBin(f, "raw", file.size(f))
  writeBin(charToRaw(gsub("\n", "\r\n", rawToChar(lf), fixed = TRUE)), f)
  suppressMessages(strip_mc_skip(apply = TRUE))
  out <- rawToChar(readBin(f, "raw", file.size(f)))
  expect_false(grepl("[^\r]\n", out))
  expect_match(out, "\r\n$")
  expect_no_match(out, "berspr")
  expect_true(suppressMessages(validate_item(f)))
})

test_that("strip_mc_skip leaves unclear cases untouched", {
  strip_mc_skip <- load_strip_mc_skip()
  local_skip_items(list(
    not_last = mc_item_fields(answeroption_02 = "Frage überspringen."),
    own_feedback = mc_item_fields(answeroption_04 = "Frage überspringen.", if_answeroption_04 = "Anderes Feedback"),
    correct = mc_item_fields(answeroption_04 = "Frage überspringen.", if_answeroption_04 = skip_fb, answer_correct = "4")
  ))
  before <- lapply(list.files("items", full.names = TRUE), readLines)
  msgs <- capture.output(changed <- strip_mc_skip(apply = TRUE), type = "message")
  expect_length(changed, 0L)
  expect_identical(lapply(list.files("items", full.names = TRUE), readLines), before)
  expect_length(list.files(".", "^items_backup_"), 0L)
  expect_true(any(grepl("not the last option", msgs)))
  expect_true(any(grepl("not the skip feedback", msgs)))
  expect_true(any(grepl("is answer_correct", msgs)))
})

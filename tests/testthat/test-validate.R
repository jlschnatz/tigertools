# Shared and multiple-choice checks (numeric rules are in test-numeric.R)

test_that("id_item must be a positive integer", {
  expect_invalid(mc_item_fields(id_item = "abc"))
  expect_invalid(mc_item_fields(id_item = "0"))
})

test_that("categorical fields must match a category, with a suggestion for typos", {
  f <- write_item_md(mc_item_fields(learning_area = "Regresion"))
  msgs <- character(0)
  expect_error(withCallingHandlers(
    validate_item(f),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  ))
  expect_true(any(grepl("Did you mean", msgs) & grepl("Regression", msgs)))
  expect_invalid(mc_item_fields(learning_area = "Astrophysik"))
  expect_invalid(mc_item_fields(bloom_taxonomy = ""))
  expect_invalid(mc_item_fields(type_stimulus = "video"))
})

test_that("MC skip option and skip feedback must match the template", {
  expect_invalid(mc_item_fields(answeroption_06 = "Weiter"))
  expect_invalid(mc_item_fields(type_answer = "image")) # needs www/skip.png
  expect_invalid(mc_item_fields(type_answer = "audio"))
  expect_invalid(mc_item_fields(if_answeroption_06 = "Übersprungen."))
})

test_that("MC answer_correct must point to an existing option", {
  expect_invalid(mc_item_fields(answer_correct = "4")) # only 3 options
  expect_invalid(mc_item_fields(answer_correct = "zwei"))
})

test_that("MC needs one feedback block per answer option", {
  expect_invalid(mc_item_fields(if_answeroption_03 = "NA"))
  expect_invalid(mc_item_fields(if_answeroption_04 = "Feedback ohne Option"))
})

test_that("stimulus_image must be an existing www/ image inside items/", {
  withr::local_dir(withr::local_tempdir())
  dir.create("items/www", recursive = TRUE)
  file.create("items/www/plot.png")
  expect_valid(mc_item_fields(stimulus_image = "www/plot.png", type_stimulus = "image"))
  expect_invalid(mc_item_fields(stimulus_image = "www/missing.png"))
  expect_invalid(mc_item_fields(stimulus_image = "plot.png"))
  expect_invalid(mc_item_fields(stimulus_image = "www/plot.pdf"))
})

test_that("the file must start with the # id_item header", {
  f <- write_item_md(mc_item_fields())
  writeLines(c("Notiz vor dem Header", readLines(f)), f)
  expect_error(suppressMessages(validate_item(f)), "header of the file")

  g <- write_item_md(mc_item_fields()[-1]) # no id_item section
  expect_error(suppressMessages(validate_item(g)), "no .*# id_item")
})

# ── Storage helpers ───────────────────────────────────────────────────────────

test_that(".read_item_csv() returns an empty, schema-aligned frame for a missing file", {
  df <- .read_item_csv(tempfile(fileext = ".csv"))
  expect_equal(nrow(df), 0)
  expect_true(all(.item_cols() %in% names(df)))
})

test_that(".add_and_sort() aligns differing columns and sorts by id", {
  old <- data.frame(id_item = c(3L, 1L), extra = c("a", "b"))
  new <- data.frame(id_item = 2L, answer_mode = "num")
  out <- .add_and_sort(old, new)
  expect_equal(out$id_item, 1:3)
  expect_equal(out$extra, c("b", NA, "a"))
  expect_equal(out$answer_mode, c(NA, "num", NA))
})

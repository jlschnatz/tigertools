# Numeric item rules — see "Numeric item rules" in shinytigeR's CLAUDE.md.

test_that("a valid numeric item passes", {
  expect_valid(num_item_fields())
})

test_that("decimal commas are accepted and normalized to dots", {
  f <- write_item_md(num_item_fields(
    answeroption_01 = "6,67", lower_answeroption_01 = "6,66", upper_answeroption_01 = "6,67"
  ))
  expect_true(suppressMessages(validate_item(f)))
  x <- .as_item_schema(parse_md_to_csv(f))
  expect_equal(x$answeroption_01, "6.67")
  expect_equal(x$lower_answeroption_01, 6.66)
  expect_equal(x$upper_answeroption_01, 6.67)
})

test_that("rule 1: options without bounds are fine (exact match)", {
  expect_valid(num_item_fields(
    lower_answeroption_01 = "", upper_answeroption_01 = "",
    lower_answeroption_04 = "", upper_answeroption_04 = "",
    lower_answeroption_05 = "", upper_answeroption_05 = ""
  ))
})

test_that("rule 2: the value must lie inside its own range, lower <= upper", {
  expect_invalid(num_item_fields(lower_answeroption_01 = "6.60", upper_answeroption_01 = "6.65"))
  expect_invalid(num_item_fields(lower_answeroption_01 = "6.68", upper_answeroption_01 = "6.66"))
})

test_that("rule 3: exactly one bound set is invalid", {
  expect_invalid(num_item_fields(upper_answeroption_01 = ""))
  expect_invalid(num_item_fields(lower_answeroption_02 = "4.9"))
})

test_that("rule 4: overlapping ranges (incl. touching and exact duplicates) are invalid", {
  # 05 [2.23, 2.24] -> [2.23, 2.58] touches 04's [2.58, 2.59]
  expect_invalid(num_item_fields(upper_answeroption_05 = "2.58"))
  # option 03 exactly equal to exact-only option 02
  expect_invalid(num_item_fields(answeroption_03 = "5"))
  # exact value falling inside another option's range
  expect_invalid(num_item_fields(answeroption_03 = "6.665"))
})

test_that("rule 5: answer_correct lists existing options, ';'-separated, no duplicates", {
  expect_valid(num_item_fields(answer_correct = "1"))
  expect_valid(num_item_fields(answer_correct = " 1 ; 2 "))
  expect_invalid(num_item_fields(answer_correct = "1;6")) # option 06 is empty
  expect_invalid(num_item_fields(answer_correct = "1,2"))
  expect_invalid(num_item_fields(answer_correct = "1;1"))
  expect_invalid(num_item_fields(answer_correct = ""))
})

test_that("values and bounds must be numbers", {
  expect_invalid(num_item_fields(answeroption_03 = "zwanzig"))
  expect_invalid(num_item_fields(lower_answeroption_01 = "6.6a"))
})

test_that("each option needs feedback, and bounds/feedback need an option", {
  expect_invalid(num_item_fields(if_answeroption_03 = ""))
  expect_invalid(num_item_fields(if_answeroption_06 = "Feedback ohne Option"))
  expect_invalid(num_item_fields(lower_answeroption_06 = "1", upper_answeroption_06 = "2"))
})

test_that("gaps between options are allowed", {
  expect_valid(num_item_fields(
    answeroption_03 = "", if_answeroption_03 = "",
    answeroption_06 = "100", if_answeroption_06 = "Weit daneben."
  ))
})

test_that("misspelled sections are reported", {
  fields <- num_item_fields()
  names(fields)[names(fields) == "lower_answeroption_01"] <- "lower_answer_01"
  expect_error(suppressMessages(validate_item(write_item_md(fields))))
})

test_that("an invalid answer_mode is reported", {
  expect_invalid(num_item_fields(answer_mode = "numeric"))
})

# ── Multiple-choice items ─────────────────────────────────────────────────────

test_that("an MC item without an answer_mode section defaults to mc and passes", {
  f <- write_item_md(mc_item_fields())
  expect_true(suppressMessages(validate_item(f)))
  expect_equal(.as_item_schema(parse_md_to_csv(f))$answer_mode, "mc")
})

test_that("MC items take exactly one correct index and no bounds", {
  expect_valid(mc_item_fields(answer_mode = "mc"))
  expect_invalid(mc_item_fields(answer_correct = "1;2"))
  expect_invalid(mc_item_fields(lower_answeroption_01 = "1", upper_answeroption_01 = "3"))
})

# ── Templates ─────────────────────────────────────────────────────────────────

test_that("create() writes the numeric template with all option sections", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  file.create("test.Rproj")
  dir.create("items")
  suppressMessages(create(open = FALSE, answer_mode = "num"))
  lines <- readLines("items/tiger_item_001.md", encoding = "UTF-8")
  expect_equal(lines[grep("^# id_item", lines) + 1], "1")
  headings <- sub("^# ", "", grep("^# ", lines, value = TRUE))
  expect_setequal(setdiff(.item_cols(), headings), "type_answer")
  expect_equal(lines[grep("^# answer_mode", lines) + 2], "num")
})

test_that("create() rejects an unknown answer_mode", {
  expect_error(create(open = FALSE, answer_mode = "numeric"))
})

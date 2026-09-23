# Writes an item markdown file from a named list of fields (one "# name"
# section per element; "" = empty section). Returns the file path.
write_item_md <- function(fields, path = tempfile("tiger_item_", fileext = ".md")) {
  lines <- unlist(lapply(names(fields), function(nm) c(paste0("# ", nm), as.character(fields[[nm]]), "")))
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

# A valid numeric item (the variance example from the numeric item rules):
# two correct options (n - 1 and n), ranges on options 01/04/05, exact-only
# options 02/03. Pass named arguments to override fields; NULL removes one.
num_item_fields <- function(...) {
  base <- list(
    id_item = "900",
    learning_area = "Deskriptivstatistik",
    type_item = "content",
    bloom_taxonomy = "application",
    theo_diff = "medium",
    answer_mode = "num",
    stimulus_text = "Berechne die Varianz von 2, 4, 6, 8. Runde auf zwei Nachkommastellen.",
    stimulus_image = "",
    type_stimulus = "text",
    answeroption_01 = "6.67", lower_answeroption_01 = "6.66", upper_answeroption_01 = "6.67",
    if_answeroption_01 = "Richtig, Stichprobenvarianz (n - 1).",
    answeroption_02 = "5", lower_answeroption_02 = "", upper_answeroption_02 = "",
    if_answeroption_02 = "Richtig, Populationsvarianz (n).",
    answeroption_03 = "20", lower_answeroption_03 = "", upper_answeroption_03 = "",
    if_answeroption_03 = "Das ist die Quadratsumme.",
    answeroption_04 = "2.58", lower_answeroption_04 = "2.58", upper_answeroption_04 = "2.59",
    if_answeroption_04 = "Das ist die Standardabweichung (n - 1).",
    answeroption_05 = "2.24", lower_answeroption_05 = "2.23", upper_answeroption_05 = "2.24",
    if_answeroption_05 = "Das ist die Standardabweichung (n).",
    answeroption_06 = "", lower_answeroption_06 = "", upper_answeroption_06 = "",
    if_answeroption_06 = "",
    answer_correct = "1;2"
  )
  utils::modifyList(base, list(...))
}

# A valid multiple-choice item, written the pre-numeric way (no answer_mode
# section), like the existing item files.
mc_item_fields <- function(...) {
  base <- list(
    id_item = "901",
    learning_area = "Regression",
    type_item = "content",
    bloom_taxonomy = "knowledge",
    theo_diff = "easy",
    stimulus_text = "Was ist die Steigung von y = 2x + 3?",
    stimulus_image = "NA",
    answeroption_01 = "2", answeroption_02 = "3", answeroption_03 = "5",
    answeroption_04 = "NA", answeroption_05 = "NA",
    answeroption_06 = "Frage überspringen.",
    answer_correct = "1",
    type_stimulus = "text",
    type_answer = "text",
    if_answeroption_01 = "Richtig.", if_answeroption_02 = "Achsenabschnitt.",
    if_answeroption_03 = "Summe.", if_answeroption_04 = "NA", if_answeroption_05 = "NA",
    if_answeroption_06 = "Alles klar! Du hast die Aufgabe übersprungen."
  )
  utils::modifyList(base, list(...))
}

expect_valid <- function(fields) {
  expect_true(suppressMessages(validate_item(write_item_md(fields))))
}

expect_invalid <- function(fields) {
  expect_error(suppressMessages(validate_item(write_item_md(fields))), "requirements are not met|Unknown")
}

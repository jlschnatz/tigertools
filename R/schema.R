# Single source of truth for the item_db column set, in the same order as the
# app's db_item.sqlite (shinytigeR, see its CLAUDE.md "Databases" section):
# per-option range bounds sit right after the feedback columns, answer_mode
# comes last. Columns the app adds downstream (irt_*, ia_*) are not tigertools'
# concern and are preserved on write, see .upsert_items() in push.R.

#' @noRd
.bound_cols <- function() {
  c(sprintf("lower_answeroption_%02d", 1:6), sprintf("upper_answeroption_%02d", 1:6))
}

#' @noRd
.item_cols <- function() {
  c(
    "id_item", "learning_area", "type_item", "bloom_taxonomy", "theo_diff",
    "stimulus_text", "stimulus_image",
    sprintf("answeroption_%02d", 1:6),
    "answer_correct", "type_stimulus", "type_answer",
    sprintf("if_answeroption_%02d", 1:6),
    .bound_cols(),
    "answer_mode"
  )
}

# SQLite column types for a freshly created item_db (and for columns added to
# an existing one). Everything not listed is TEXT.
#' @noRd
.item_field_types <- function(cols) {
  types <- stats::setNames(rep("TEXT", length(cols)), cols)
  types[cols == "id_item"] <- "INTEGER"
  types[cols %in% .bound_cols()] <- "REAL"
  types
}

# Categories allowed for the categorical fields. learning_area must match
# LEARNING_AREA_LEVELS in shinytigeR's R/constants.R.
#' @noRd
.valid_categories <- function() {
  list(
    learning_area = c(
      "Deskriptivstatistik", "Wahrscheinlichkeit", "Grundlagen der Inferenzstatistik",
      "Gruppenvergleiche", "Poweranalyse", paste0("Zusammenhangsma", "\u00df", "e"), "Regression"
    ),
    type_item = c("content", "coding"),
    bloom_taxonomy = c("knowledge", "comprehension", "application"),
    theo_diff = c("easy", "medium", "hard"),
    type_stimulus = c("text", "image"),
    answer_mode = c("mc", "num")
  )
}

# "6,67" -> "6.67" (German decimal comma), trimmed; NA stays NA.
#' @noRd
.normalize_decimal <- function(x) {
  x <- trimws(as.character(x))
  gsub(",", ".", x, fixed = TRUE)
}

# Aligns a parsed item data frame (from parse_md_to_csv() or the CSV) to the
# full item schema: missing columns are added as NA, empty strings become NA,
# a missing answer_mode defaults to "mc" (item files written before numeric
# items existed have no answer_mode section), numeric-item values and bounds
# get their decimal comma normalized, and columns get their schema types.
# Columns not in the schema are kept, after the schema columns.
#' @noRd
.as_item_schema <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  cols <- .item_cols()
  for (col in setdiff(cols, names(df))) df[[col]] <- rep(NA_character_, nrow(df))
  for (col in names(df)) {
    v <- as.character(df[[col]])
    v[!is.na(v) & (trimws(v) == "" | v == "NA")] <- NA
    df[[col]] <- v
  }
  df$answer_mode[is.na(df$answer_mode)] <- "mc"
  df$answer_mode <- trimws(df$answer_mode)

  is_num <- df$answer_mode == "num"
  for (col in sprintf("answeroption_%02d", 1:6)) {
    df[[col]][is_num] <- .normalize_decimal(df[[col]][is_num])
  }
  for (col in .bound_cols()) {
    df[[col]] <- suppressWarnings(as.numeric(.normalize_decimal(df[[col]])))
  }
  df$id_item <- suppressWarnings(as.integer(df$id_item))
  df[, c(cols, setdiff(names(df), cols)), drop = FALSE]
}

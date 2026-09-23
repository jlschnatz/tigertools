#' @title Push a single item to the database
#' @description Validates one item (from markdown) and writes it to the CSV and
#'   SQLite database. If the item already exists, it can optionally be
#'   overwritten.
#' @details The SQLite table is updated in place, never recreated: the item's
#'   row is updated (or inserted, if new), and columns tigertools doesn't manage
#'   itself (e.g. `irt_*` parameters added by a later calibration step) are kept
#'   untouched. Columns of the current item schema that the table is still
#'   missing (e.g. the numeric-item range bounds) are added.
#' @param file Path to the markdown file
#' @param overwrite Logical, whether to overwrite an existing item (default: FALSE)
#' @param confirm Logical, whether to prompt user interactively (default: TRUE)
#' @export
push <- function(file, overwrite = FALSE, confirm = TRUE) {
  # Ensure correct working directory and file validity
  check_folderstruct()
  if (!file.exists(file)) cli::cli_abort("The file {.file {file}} does not exist.")
  validate_item(file)

  # Load existing data and new item
  df_existing <- .read_item_csv("./data_item_tiger.csv")
  df_new <- .as_item_schema(parse_md_to_csv(file))
  id_new <- df_new$id_item

  if (id_new %in% df_existing$id_item) {
    if (!overwrite) {
      cli::cli_abort("Item with id {.val {id_new}} already exists. Use {.code overwrite = TRUE} to replace it.")
    }

    cli::cli_alert_info("Item with id {.val {id_new}} already exists.")

    # Prompt for overwrite only if confirm = TRUE
    if (confirm && !isTRUE(utils::askYesNo("Do you want to overwrite the existing item?", default = FALSE))) {
      cli::cli_abort("Item not overwritten. Rerun with confirmation.")
    }

    cli::cli_alert_info("Overwriting existing item with id {.val {id_new}}.")
    df_existing <- .remove_item_by_id(df_existing, id_new)
  } else {
    cli::cli_alert_info("Adding new item with id {.val {id_new}} to the database.")
  }

  # Add new item and write to CSV + DB
  df_updated <- .add_and_sort(df_existing, df_new)
  .write_to_storage(df_updated)

  cli::cli_alert_success("Item with id {.val {id_new}} processed successfully.")
}


#' @keywords internal
.remove_item_by_id <- function(df, id) df[df$id_item != id, , drop = FALSE]

#' @keywords internal
.add_and_sort <- function(df_existing, df_new) {
  # Columns only one side has (e.g. extra columns in an edited CSV) become NA
  for (col in setdiff(names(df_existing), names(df_new))) df_new[[col]] <- NA
  for (col in setdiff(names(df_new), names(df_existing))) df_existing[[col]] <- rep(NA, nrow(df_existing))
  df_combined <- rbind(df_existing, df_new[, names(df_existing), drop = FALSE])
  df_combined[order(df_combined$id_item), , drop = FALSE]
}

# Reads the item CSV as all-character (no type guessing, which would e.g. turn
# answer_correct "1;2" into NA), then aligns it to the current schema - so a
# CSV written before numeric items existed gains the new columns as NA.
#' @keywords internal
.read_item_csv <- function(csv_file) {
  if (!file.exists(csv_file)) {
    return(.as_item_schema(data.frame(id_item = character(0))))
  }
  df <- readr::read_csv(csv_file, col_types = readr::cols(.default = readr::col_character()))
  .as_item_schema(df)
}

#' @keywords internal
.write_to_storage <- function(df, csv_file = "./data_item_tiger.csv", sqlite_file = "./db_item.sqlite") {
  readr::write_csv(df, csv_file)
  cli::cli_alert_info("CSV updated: {.file {normalizePath(csv_file)}}")
  .upsert_items(df, sqlite_file)
  cli::cli_alert_info("SQLite DB updated: {.file {normalizePath(sqlite_file)}}")
}

# Writes `df` into item_db without dropping anything: rows with an existing
# id_item are UPDATEd (only the columns in `df`), new ids are INSERTed, schema
# columns the table lacks are ADDed first, and all other columns and rows are
# left as they are. Creates the table (with schema types) if it doesn't exist.
#' @keywords internal
.upsert_items <- function(df, sqlite_file) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!DBI::dbExistsTable(con, "item_db")) {
    DBI::dbWriteTable(con, "item_db", df, field.types = .item_field_types(names(df)))
    return(invisible(nrow(df)))
  }

  missing <- setdiff(names(df), DBI::dbListFields(con, "item_db"))
  types <- .item_field_types(missing)
  for (col in missing) {
    DBI::dbExecute(con, sprintf("ALTER TABLE item_db ADD COLUMN `%s` %s", col, types[[col]]))
  }
  if (length(missing) > 0L) {
    cli::cli_alert_info("Added new column(s) {.field {missing}} to {.file {sqlite_file}}.")
  }

  DBI::dbWithTransaction(con, {
    existing_ids <- as.character(DBI::dbGetQuery(con, "SELECT id_item FROM item_db")$id_item)
    is_update <- as.character(df$id_item) %in% existing_ids
    set_cols <- setdiff(names(df), "id_item")
    if (any(is_update)) {
      upd <- df[is_update, , drop = FALSE]
      sql <- sprintf(
        "UPDATE item_db SET %s WHERE id_item = ?",
        paste(sprintf("`%s` = ?", set_cols), collapse = ", ")
      )
      DBI::dbExecute(con, sql, params = unname(as.list(upd[, c(set_cols, "id_item")])))
    }
    if (any(!is_update)) {
      DBI::dbAppendTable(con, "item_db", df[!is_update, , drop = FALSE])
    }
  })
  invisible(nrow(df))
}


#' @title Push all items in a directory
#' @description Pushes all markdown files in a directory to the database.
#' If overwrite = TRUE, all existing items will be overwritten without confirmation.
#' @param path Path to directory with markdown files (default: "./items")
#' @param overwrite Logical, whether to overwrite existing items (default: FALSE)
#' @export
push_all <- function(path = "./items", overwrite = FALSE) {
  check_folderstruct()
  files <- list.files(path, pattern = "\\.md$", full.names = TRUE)

  if (overwrite && interactive()) {
    if (!isTRUE(utils::askYesNo("You are about to overwrite existing items. Continue?", default = FALSE))) {
      cli::cli_abort("Aborted by user.")
    }
  }

  for (file in files) {
    push(file, overwrite = overwrite, confirm = FALSE)
    cli::cli_rule()
  }

  cli::cli_alert_success("All items pushed.")
}


#' @title Push Recent Items to the Database
#' @description Pushes Markdown files modified more recently than the database
#'   file. Existing items among them are overwritten without confirmation (a
#'   newer file is taken as an intended edit).
#' @param path Path to directory with markdown files (default: "./items")
#' @return Invisibly returns `NULL`.
#' @seealso \code{\link{push}} to push a single file.
#' @export
push_recent <- function(path = "./items") {
  check_folderstruct()
  md_files <- list.files(path, pattern = "\\.md$", full.names = TRUE)
  mtime_sqlite <- file.mtime("db_item.sqlite")
  recent_files <- if (is.na(mtime_sqlite)) md_files else md_files[file.mtime(md_files) > mtime_sqlite]
  if (length(recent_files) == 0) {
    cli::cli_alert_info("No recent items to push.")
    return(invisible())
  }
  cli::cli_alert_info("Pushing {length(recent_files)} recent item{?s} to the database...")
  for (file in recent_files) {
    push(file, overwrite = TRUE, confirm = FALSE)
    cli::cli_rule()
  }
  cli::cli_alert_success("Recent items pushed successfully.")
  invisible()
}

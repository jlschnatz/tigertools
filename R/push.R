#' @title Push a single item to the database
#' @description Pushes one item (from markdown) to the CSV and SQLite database. 
#' If the item already exists, it can optionally be overwritten.
#' @param file Path to the markdown file
#' @param overwrite Logical, whether to overwrite an existing item (default: FALSE)
#' @param confirm Logical, whether to prompt user interactively (default: TRUE)
push <- function(file, overwrite = FALSE, confirm = TRUE) {
  # Ensure correct working directory and file validity
  check_folderstruct()
  if (!file.exists(file)) cli::cli_abort("The file {.file {file}} does not exist.")
  validate_item(file)

  # Load existing data and new item
  df_existing <- readr::read_csv("./data_item_tiger.csv", col_types = readr::cols())
  id_existing <- df_existing$id_item
  df_new <- parse_md_to_csv(file)
  id_new <- df_new$id_item

  # Check if item exists
  item_exists <- id_new %in% id_existing

  if (item_exists) {
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
.remove_item_by_id <- function(df, id) df[df$id_item != id, ]

#' @keywords internal
.add_and_sort <- function(df_existing, df_new) {
  df_combined <- rbind(df_existing, df_new)
  df_combined[order(df_combined$id_item), ]
}

#' @keywords internal
.write_to_storage <- function(df) {
  readr::write_csv(df, "./data_item_tiger.csv")
  cli::cli_alert_info("CSV updated: {.file {normalizePath('./data_item_tiger.csv')}}")

  con <- DBI::dbConnect(RSQLite::SQLite(), "./db_item.sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "item_db", df, overwrite = TRUE)
  cli::cli_alert_info("SQLite DB updated: {.file {normalizePath('./db_item.sqlite')}}")
}


#' @title Push all items in a directory
#' @description Pushes all markdown files in a directory to the database.
#' If overwrite = TRUE, all existing items will be overwritten without confirmation.
#' @param path Path to directory with markdown files (default: "./items")
#' @param overwrite Logical, whether to overwrite existing items (default: FALSE)
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
#' @description Pushes Markdown files modified more recently than the database file.
#' Only files newer than the database are pushed without overwrite confirmation.
#' @return Invisibly returns `NULL`.
#' @seealso \code{\link{push}} to push a single file.
#' @export
push_recent <- function() {
  md_files <- list.files(path, pattern = "\\.md$", full.names = TRUE)
  ctime_md_files <- file.info(md_files)$ctime
  ctime_sqlite <- file.info("db_item.sqlite")$ctime
  recent_files <- md_files[ctime_md_files > ctime_sqlite]
  if (length(recent_files) == 0) {
    cli::cli_alert_info("No recent items to push.")
    return(invisible())
  }
  cli::cli_alert_info("Pushing recent items to the database...")
  for (file in recent_files) {
    push(file, overwrite = FALSE, confirm = FALSE)
    cli::cli_rule()
  }
  cli::cli_alert_success("Recent items pushed successfully.")
  invisible()
}
#' @title Update Database
#' @description Validates one (newly created) item, then rebuilds the CSV from
#'   all item files and writes them to the SQLite database.
#' @details Like [push()], the SQLite table is updated in place (rows updated or
#'   inserted by `id_item`), never recreated, so columns tigertools doesn't
#'   manage itself (e.g. `irt_*`) are kept.
#' @param md_file path to filename of the newly created item
#' @param item_folder folder name, where all items are located (defualts to 'items')
#' @param csv_file the csv-file to which the database should be written
#' @param sqlite_file the sqlite-file to which the database should be written
#' @return Writes the csv-file and sqlite-file.
#' @export
update_db <- function(md_file, item_folder = "items", csv_file = "data_item_tiger.csv", sqlite_file = "db_item.sqlite") {
  validate_item(md_file)
  files <- list.files(item_folder, full.names = TRUE, pattern = "tiger_item_\\d{3}\\.md$")
  df_item <- .as_item_schema(do.call(rbind, lapply(files, function(f) .as_item_schema(parse_md_to_csv(f)))))
  .write_to_storage(df_item[order(df_item$id_item), , drop = FALSE], csv_file = csv_file, sqlite_file = sqlite_file)
  cli::cli_alert_success("Finished!")
}

#' @title Remove non-markdown files from the item folder
#' @param item_folder folder name, where all items are located
#' @noRd
clean <- function(item_folder) {
  files <- list.files(item_folder, full.names = TRUE)
  files <- files[!grepl("\\.md$", files)]
  file.remove(files)
  cli::cli_alert_success("Removed non .md-files from {.file {normalizePath(item_folder)}}")
}

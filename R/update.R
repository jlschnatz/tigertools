#' @title Update Database
#' @description Update the sqlite-database and the csv-file containing all items
#' @param md_file path to filename of the newly created item
#' @param item_folder folder name, where all items are located (defualts to 'items')
#' @param csv_file the csv-file to which the database should be written
#' @param sqlite_file the sqlite-file to which the database should be written
#' @return Writes the csv-file and sqlite-file.
#' @export 
update_db <- function(md_file, item_folder = "items", csv_file = "data_item_tiger.csv", sqlite_file = "db_item.sqlite") {
    validate_item(md_file)
    df_item <- do.call(rbind, lapply(dir(file.path(getwd(), item_folder), full.names = TRUE), parse_md_to_csv))
    readr::write_csv(df_item, csv_file)
    cli::cli_alert_info("Neue CSV-Datei beschrieben, siehe {.file {normalizePath(csv_file)}}")
    con <- DBI::dbConnect(drv = RSQLite::SQLite(), sqlite_file)
    DBI::dbWriteTable(con, "item_db", df_item, overwrite = TRUE)
    DBI::dbDisconnect(con)
    cli::cli_alert_info("Data was written to database, see {.file {normalizePath(sqlite_file)}}")
    cli::cli_alert_success("Fertig!")
}

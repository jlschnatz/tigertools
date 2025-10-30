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
    files <- list.files(item_folder, full.names = TRUE, pattern = "tiger_item_\\d{3}\\.md$")
    df_item <- do.call(rbind, lapply(files, parse_md_to_csv))
    readr::write_csv(df_item, csv_file)
    cli::cli_alert_info("CSV file written, see {.file {normalizePath(csv_file)}}")
    con <- DBI::dbConnect(drv = RSQLite::SQLite(), sqlite_file)
    DBI::dbWriteTable(con, "item_db", df_item, overwrite = TRUE)
    DBI::dbDisconnect(con)
    cli::cli_alert_info("SQLite database written, see {.file {normalizePath(sqlite_file)}}")
    cli::cli_alert_success("Finished!")
}


add_modified <- function(item_folder = "items", csv_file = "data_item_tiger.csv", sqlite_file = "db_item.sqlite") {
    md_files <- list.files(item_folder, full.names = TRUE, pattern = "tiger_item_\\d{3}\\.md$")
    mtime_md <- file.mtime(md_files)
    mtime_sqlite <- file.mtime(file.path(getwd(), sqlite_file))
    id_change <- which(mtime_md > mtime_sqlite) 
    if(length(id_change) == 0) {
        cli::cli_alert_info("No new items found.")
        return()
    }
    cli::cli_alert_info("Found {.val {length(id_change)}} new or modified items.")
    lapply(md_files[id_change], function(x) {
        cli::cli_alert_info("Adding item {.file {normalizePath(x)}}")
        add_item(x, item_folder, csv_file, sqlite_file)
    })
}


add_item <- function(md_file, item_folder = "items", csv_file = "data_item_tiger.csv", sqlite_file = "db_item.sqlite") {

    # check if the file is a valid item
    validate_item(md_file)

    # check if the item is already in the database
    con <- DBI::dbConnect(drv = RSQLite::SQLite(), sqlite_file)
    df_item <- DBI::dbReadTable(con, "item_db")
    df_new_item <- parse_md_to_csv(md_file) 
    is_existing <- df_new_item$id_item %in% df_item$id_item
    if(is_existing) {
        # if the item is already in the database, ask if the user wants to overwrite it
        cli::cli_alert_danger("Item already exist in the database.")
        overwrite <- ask_yes_no("Do you want to overwrite the existing item?") 
        if(!overwrite) {
            # if not, do not add the item to the database
            cli::cli_alert_info("Item not added to the database.")
            return()
        } else {
            # if yes, overwrite the existing item
            df_item[which(df_item$id_item == df_new_item$id_item), ] <- df_new_item
            DBI::dbWriteTable(con, "item_db", df_item, overwrite = TRUE)
            cli::cli_alert_info("SQLite database written, see {.file {normalizePath(sqlite_file)}}")
            readr::write_csv(df_item, csv_file)
            cli::cli_alert_info("CSV file written, see {.file {normalizePath(csv_file)}}")
        }
    } else {
        # if the item is not in the database, add it to the database
        df_item <- rbind(df_item, df_new_item)
        DBI::dbWriteTable(con, "item_db", df_item, overwrite = TRUE)
        cli::cli_alert_info("SQLite database written, see {.file {normalizePath(sqlite_file)}}")
        readr::write_csv(df_item, csv_file)
        cli::cli_alert_info("CSV file written, see {.file {normalizePath(csv_file)}}")
    }

    DBI::dbDisconnect(con)
    cli::cli_alert_success("Finished!")
}

ask_yes_no <- function(prompt_text) {
  while (TRUE) {
    cli::cli_text("{.bold {prompt_text}} ({.emph yes}/{.emph no})")
    answer <- tolower(trimws(readline(">> ")))
    if (answer %in% c("yes", "y")) {
      return(TRUE)
    }
    if (answer %in% c("no", "n")) {
      return(FALSE)
    }
    cli::cli_alert_danger("Invalid input. Please enter 'yes' or 'no'.")
    cli::cli_alert_info("Please try again.")
  }
}


clean <- function(item_folder) {
    # remove all files that are non .md files
    files <- list.files(item_folder, full.names = TRUE)
    files <- files[!grepl("\\.md$", files)]
    file.remove(files)
    cli::cli_alert_success("Removed non .md-files from {.file {normalizePath(item_folder)}}")
}

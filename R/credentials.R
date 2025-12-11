#' @title Passwort Generation
#' @param n Number of passworts to generate
#' @param len The length of the password
#' @return A character vector
generate_pw <- function(n, len) {
  stopifnot(is.numeric(n), length(n) == 1, is.numeric(len), length(len) == 1)
  chr <- c(letters, LETTERS, 0:9)
  pw <- sapply(seq_len(n), function(n) {
    paste0(sample(chr, len, replace = TRUE), collapse = "")
  })
  return(pw)
}

#' @noRd
.generate_users <- function(n, len) {
  cli::cli_alert_info("Generating usernames and passwords.")
  user_name <- gsub(" ", "-", tolower(noah::pseudonymize(seq_len(n))))
  password <- generate_pw(n, len)
  cli::cli_alert_info("Encrypting passwords.")
  password_hashed <- sapply(cli::cli_progress_along(password), function(i) {
    sodium::password_store(password[i])
  })
  credentials_unmasked <- tibble::tibble(
    user_name,
    password,
    permissions = "standard"
  )
  credentials_masked <- tibble::tibble(
    user_name,
    password_hashed,
    permissions = "standard"
  )
  return(list(masked = credentials_masked, unmasked = credentials_unmasked))
}


#' @title Generate User Credential Data
#' @param n Numer of users to generate
#' @param len Length of the password
#' @export
generate_users <- function(n, len) {
  if (!dir.exists("csv") | !dir.exists("sqlite") | !dir.exists("unmasked")) {
    msg <- "Your current working directory is not set correctly. Please ensure that you open the {.emph user_credentials/} folder via the RStudio-project file."
    cli::cli_abort(msg)
  }
  x <- .generate_users(n, len)
  write_masked(x)
  write_unmasked(x)
}

#' @noRd
write_unmasked <- function(x) {
  if (!dir.exists("unmasked")) {
    dir.create("unmasked")
  }
  filename_csv <- sprintf(
    "unmasked/data_user_credentials_unmasked_%s.csv",
    format(Sys.Date(), "%d%m%y")
  )
  utils::write.csv(
    x$unmasked[, c("user_name", "password")],
    filename_csv,
    row.names = FALSE
  )
  cli::cli_alert_success("Unmasked data written to {.file {filename_csv}}.")
  cli::cli_alert_warning("Please remember to delete the data after printing!")
}

#' @noRd
write_masked <- function(x) {
  cur_data <- unique(do.call(
    rbind,
    lapply(dir("csv", full.names = TRUE), utils::read.csv)
  ))
  new_data <- rbind(cur_data, x$masked)
  if (any(duplicated(new_data$user_name))) {
    cli::cli_abort(
      "Data contains duplicated usernames. Please re-run the function!"
    )
  }
  filename_csv <- sprintf(
    "csv/data_user_credentials_%s.csv",
    format(Sys.Date(), "%d%m%y")
  )
  utils::write.csv(new_data, filename_csv, row.names = FALSE)
  cli::cli_alert_success(
    "Masked data written to csv-file {.file {filename_csv}}."
  )
  filename_sqlite <- sprintf(
    "sqlite/db_credentials_%s.sqlite",
    format(Sys.Date(), "%d%m%y")
  )
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = filename_sqlite)
  DBI::dbWriteTable(
    con,
    "credentials_db",
    new_data,
    row.names = FALSE,
    overwrite = TRUE
  )
  cli::cli_alert_success("Data written to database {.file {filename_sqlite}}.")
  DBI::dbDisconnect(con)
  f <- file.path(here::here(), "db_credentials.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = f)
  DBI::dbWriteTable(
    con,
    "credentials_db",
    new_data,
    row.names = FALSE,
    overwrite = TRUE
  )
  cli::cli_alert_success(
    "Copyable version of the database written to {.file {f}}."
  )
  cli::cli_alert_info(
    "You can use this file to directly for the server database"
  )
  DBI::dbDisconnect(con)
}

#' @title Check Folder Structure
#' @description Checks if the current working directory is the correct folder structure for the project.
#' @details The current working directory should contain the `.Rproj` file and the `items` folder.
#' @return Throws an error if the folder structure is not correct.
check_folderstruct <- function() {
    if (!any(grepl("\\.Rproj$", dir(getwd()), ignore.case = TRUE))) {
        msg <- c(
            "Youre current working directory is not located in the correct folder", 
            "It should be the folder where the .Rproj-file is located.", 
            "Please open the RStudio-project using the R-Project file."
        )
        cli::cli_abort(paste0(msg, collapse = " "))
    }

    if( !dir.exists("items")) {
        msg <- "The folder {.field items} does not exist in the current working directory. Please ensure you are in the correct project folder."
        cli::cli_abort(msg)
    }
}

#' @title Create a new Item from the Markdown Template
#' @param open Whether to open the newly created file. Logical vector.
#' @param r_file Filename of an R-file (defaults to NULL).
#' @param answer_mode `"mc"` (default) for a multiple-choice item, or `"num"`
#'   for a numeric item (students type a number; each answer option has an
#'   accepted range). The two use different templates.
#' @return Creates a new file based on the Markdown-Template with prefilled `id_item`.
#' @export
create <- function(open = TRUE, r_file = NULL, answer_mode = c("mc", "num")) {
    answer_mode <- match.arg(answer_mode)
    item_folder <- "items" # Default folder for items
    # Current wd must be where .Rproj file is located and where the items folder is located
    check_folderstruct()

    item_folder_path <- file.path(getwd(), item_folder)
    # Determine the next item number
    existing_files <- dir(item_folder_path, pattern = "tiger_item_\\d+\\.md", full.names = FALSE)
    if (length(existing_files) == 0) {
        max_digit_exist <- 0
    } else {
        max_digit_exist <- max(as.numeric(sub("tiger_item_(\\d*)\\.md", "\\1", existing_files)), na.rm = TRUE)
    }
   
    # Create the new markdown file from template
    md_filename <- sprintf("%s/tiger_item_%03d.md", item_folder_path, max_digit_exist + 1)
    template <- system.file("templates", sprintf("item_%s.md", answer_mode), package = "tigertools")
    item_txt <- readLines(template, warn = FALSE, encoding = "UTF-8")
    item_txt <- append(item_txt, as.character(max_digit_exist + 1), after = grep("^# id_item", item_txt)[1])
    writeLines(enc2utf8(item_txt), md_filename, useBytes = TRUE)
    
    # Handle optional R file creation
    if (!is.null(r_file)) {
        if (!grepl("^(r|R)$", tools::file_ext(r_file))) {
            cli::cli_abort("File in argument {.field r_file} must be a valid R-file.")
        }
        data_path <- file.path(getwd(), "data-raw")
        if (!dir.exists(data_path)) dir.create(data_path)
        r_filename <- file.path(data_path, r_file)
        file.create(r_filename)
        if (open) {
            utils::browseURL(md_filename, browser = getOption("browser"))
            utils::browseURL(r_filename, browser = getOption("browser"))
        }
        cli::cli_alert_success("New R file created at {.file {r_filename}}")
    }
    
    if (open) {
        utils::browseURL(md_filename, browser = getOption("browser"))
    }
    cli::cli_alert_success("New markdown file created at {.file {md_filename}}")
}





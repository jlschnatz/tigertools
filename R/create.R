#' @title Create a new Item from the Markdown Template
#' @param item_folder The folder where the new item should be created. Defaults to "items".
#' @param open Whether to open the newly created file. Logical vector.
#' @param r_file Filename of an R-file (defaults to NULL).
#' @return Creates a new file based on the Markdown-Template with prefilled `id_item`.
#' @export 
create_item <- function(item_folder = "items", open = TRUE, r_file = NULL) {
    # Current wd must be where .Rproj file is located
    if(!any(grepl("\\.Rproj$", dir(getwd()), ignore.case = TRUE))) {
        msg <- c(
            "Youre current working directory is not located in the correct folder", 
            "It should be the folder where the .Rproj-file is located.", 
            "Please open the RStudio-project using the R-Project file."
        )
        cli::cli_abort(paste0(msg, collapse = " "))
    }
    # Ensure the folder exists
    item_folder_path <- file.path(getwd(), item_folder)
    if (!dir.exists(item_folder_path)) {
        msg <- "Specified directory in argument {.field item_folder} does not exist. Please create this folder."
        cli::cli_abort(msg)
    }
    
    # Determine the next item number
    existing_files <- dir(item_folder_path, pattern = "tiger_item_\\d+\\.md", full.names = FALSE)
    if (length(existing_files) == 0) {
        max_digit_exist <- 0
    } else {
        max_digit_exist <- max(as.numeric(sub("tiger_item_(\\d*)\\.md", "\\1", existing_files)), na.rm = TRUE)
    }
   
    ## Create the new markdown file
    md_filename <- sprintf("%s/tiger_item_%03d.md", item_folder_path, max_digit_exist + 1)
    writeLines(skeleton, md_filename)
    item_txt <- readLines(md_filename, warn = FALSE)
    item_txt_append <- append(item_txt, c(max_digit_exist + 1, ""), grep("learning_area", item_txt) - 1)
    writeLines(item_txt_append, md_filename)
    
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


tools::showNonASCIIfile("R/validate.R")



#' @title List Files in Directory (without subdirectories)
#' @description List all files in a directory, excluding subdirectories
#' @param path path to the directory
#' @return A character vector of file paths
list_files <- function(path) {
    files <- list.files(path, full.names = TRUE)
    files <- files[!file.info(files)$isdir]
    return(files)
}
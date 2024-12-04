#' @title Remove Markdown Comments in String via Regular Expressions
#' @param x A character string.
#' @return A character string without markdown comments.
rm_md_comments <- function(x) {
    rgx <- "<![-]{2,3}(.|\\s|\\r)*?[-]{2,3}>"
    gsub(pattern = rgx, replacement = "", x)
}

#' @title Parse Markdown to DataFrame
#' @param filename The input filename of the Markdown file.
#' @return A data frame containing parsed sections in tidy format.
parse_md_to_csv <- function(filename) {
    # Parse the markdown file
    parsed_txt <- parsermd::parse_rmd(filename)
    # Extract unique sections
    sections <- unique(stats::na.omit(unlist(parsermd::rmd_node_sections(parsed_txt))))
    # Convert parsed text to a data frame
    df_parsed <- as.data.frame(parsed_txt)   
    # Filter rows where type is "rmd_markdown"
    df_parsed <- df_parsed[df_parsed$type == "rmd_markdown", ]  
    # Combine the 'ast' field (list of character vectors) into a single string
    df_parsed$ast <- sapply(df_parsed$ast, function(x) paste(x, collapse = "\n"))
    # Select only the relevant columns: 'sec_h1' (heading) and 'ast' (text)
    df_parsed <- df_parsed[, c("sec_h1", "ast")]
    colnames(df_parsed) <- c("heading", "text") 
    # Create a data frame for all sections
    sections_df <- data.frame(heading = sections, stringsAsFactors = FALSE) 
    # Merge parsed content with the full list of sections
    df_parsed <- merge(sections_df, df_parsed, by = "heading", all.x = TRUE, sort = FALSE) 
    # Sort rows by the order of sections
    df_parsed <- df_parsed[match(sections, df_parsed$heading), ]
    # Clean up the text by removing markdown comments and trimming whitespace
    df_parsed$text <- rm_md_comments(trimws(df_parsed$text))  
    # Convert to wide format with headings as column names
    result <- parsermd::as_tibble(stats::setNames(as.list(df_parsed$text), df_parsed$heading))
    result[, which(result == "NA")] <- NA
    return(result)
}

#' @title Parse Tidy DataFrame Back to Markdown Without For Loop
#' @param x A data frame containing parsed sections in tidy format.
#' @param item_folder folder name, where all items are located (defualts to 'items')
#' @return Writes the reconstructed Markdown to the specified file.
parse_csv_to_md <- function(x, item_folder = "items") {
    filename <- sprintf(file.path(item_folder, "tiger_item_%03s.md"), x$id_item)
    output <- vector(mode = "list", length = nrow(x))
    for(i in seq_len(nrow(x))) {
        output[[i]] <- unlist(mapply(function(section, content) {
            # Add the header
            header <- paste0("# ", section)
            # Combine header, content, and a blank line
            c(header, content, "")
        }, names(x[i, ]), as.list(x[i, ]), SIMPLIFY = FALSE))
        writeLines(output[[i]], filename[i])
    }
    cli::cli_alert_success("Successfully transferred data frame to .md files, see: {.file {file.path(getwd(), item_folder)}}")
}
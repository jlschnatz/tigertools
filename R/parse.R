#' @title Remove Markdown Comments in String via Regular Expressions
#' @param x A character string.
#' @return A character string without markdown comments.
rm_md_comments <- function(x) {
    rgx <- "<![-]{2,3}(.|\\s|\\r)*?[-]{2,3}>"
    gsub(pattern = rgx, replacement = "", x)
}

#' @title Parse Markdown to DataFrame
#' @description Splits an item file into its `# field` sections: each section's
#'   value is everything between its heading and the next one, with markdown
#'   comments removed and surrounding whitespace trimmed. Fenced code blocks
#'   (```` ``` ```` or `~~~`) are kept verbatim as part of the section,
#'   including indentation inside them, and `#` lines inside them (R comments)
#'   are not treated as headings.
#' @param filename The input filename of the Markdown file.
#' @return A one-row tibble with one character column per section, in file
#'   order; a section that is empty or `NA` is `NA`. If a heading occurs
#'   twice, the first one wins (validate_item() reports duplicates).
parse_md_to_csv <- function(filename) {
    lines <- sub("\r$", "", readLines(filename, warn = FALSE, encoding = "UTF-8"))
    is_fence <- grepl("^\\s*(```|~~~)", lines)
    in_code <- (cumsum(is_fence) %% 2 == 1) & !is_fence
    # Outside code, lines are trimmed (so indentation can't accidentally turn
    # text into an indented code block); inside code, indentation is content
    lines[!in_code] <- trimws(lines[!in_code])
    is_heading <- !in_code & !is_fence & grepl("^#\\s+\\S", lines)

    starts <- which(is_heading)
    ends <- c(starts[-1] - 1L, length(lines))
    headings <- trimws(sub("^#\\s+", "", lines[starts]))
    texts <- mapply(function(s, e) {
        body <- if (e > s) lines[(s + 1L):e] else character(0)
        trimws(rm_md_comments(paste(body, collapse = "\n")))
    }, starts, ends, USE.NAMES = FALSE)

    keep <- !duplicated(headings)
    texts <- as.list(texts[keep])
    texts <- lapply(texts, function(t) if (!nzchar(t) || t == "NA") NA_character_ else t)
    tibble::as_tibble(stats::setNames(texts, headings[keep]))
}

#' @title Parse Tidy DataFrame Back to Markdown Without For Loop
#' @param x A data frame containing parsed sections in tidy format.
#' @param item_folder folder name, where all items are located (defualts to 'items')
#' @return Writes the reconstructed Markdown to the specified file.
parse_csv_to_md <- function(x, item_folder = "items") {
    filename <- sprintf(file.path(item_folder, "tiger_item_%03d.md"), as.integer(x$id_item))
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
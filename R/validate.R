#' @noRd
is_empty <- function(x, first.only = TRUE, all.na.empty = TRUE) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      # characters may also be of length 0
      if (length(x) == 0) return(TRUE)
      # else, check all elements of x
      zero_len <- nchar(x) == 0
      # return result for multiple elements of character vector
      if (first.only) {
        zero_len <- .is_true(zero_len[1])
        if (length(x) > 0) x <- x[1]
      } else {
        return(unname(zero_len))
      }
      # we have a non-character vector here. check for length
    } else if (is.list(x)) {
      x <- purrr::compact(x)
      zero_len <- length(x) == 0
    } else {
      zero_len <- length(x) == 0
    }
  }

  any(is.null(x) || zero_len || (all.na.empty && all(is.na(x))))
}

.is_true <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

#' @noRd 
validate_id_item <- function(x) {
    x <- suppressWarnings(as.integer(x))
    if (!is.integer(x) || x <= 0 || is.na(x)) {
        cli::cli_alert_danger("The value for the variable {.field id_item} must be a positive integer.")
        FALSE
    } else {
        cli::cli_alert_success("Input for variable {.field id_item correct.}")
        TRUE
    }
}

#' @noRd 
is_valid_img_path <- function(path) {
    path <- file.path("items", path)
    img_regex <- "^items/www/.*\\.(jpg|jpeg|png|gif|bmp|tiff|webp)$"
    if(length(path) > 1) {
        cli::cli_alert_danger("Muliple image paths in a single field detected. Only one image path is allowed.")
        return(FALSE)
    }

    if(!grepl(img_regex, path, ignore.case = TRUE)) {
        cli::cli_alert_danger("Invalid naming scheme for image path. The path must start with {.emph www/} and end with a valid image file extension (e.g., .jpg, .png, .bmp, .tiff).")
        return(FALSE)
    }

    if(!file.exists(path)) {
        cli::cli_alert_danger("The image file does not exist. Please check the path.")
        return(FALSE)
    }

    cli::cli_alert_success("The image path is valid.")
    return(TRUE)
}


#' @noRd 
validate_stimulus_image <- function(x) {
    if(!is.na(x$stimulus_image) && x$stimulus_image != "") {
        is_valid <- is_valid_img_path(x$stimulus_image)
        if(!is_valid) {
            cli::cli_alert_danger("Input for variable {.field stimulus_image} must start with {.emph www/} and end with a valid image file extension (e.g., .jpg, .png, .bmp, .tiff). For instance: {.str www/image.png}.")
            return(FALSE)
        } else {
            cli::cli_alert_success("Input for variable {.field stimulus_image} correct.")
            return(TRUE)
        }
    }
}

validate_answeroption <- function(x) {
    # if type_anwer = "image"
    if(x$type_answer == "image") {
        ansopt_nms <- paste0("answeroption_0", seq_len(5))
        lapply(x[, ansopt_nms],)


    }
}

#' @noRd 
validate_answeroption <- function(x) {
    check <- dplyr::case_when(
        x$type_answer == "text" & x$answeroption_06 == "Frage \u00FCberspringen." ~ "correct",
        x$type_answer == "text" & x$answeroption_06 != "Frage \u00FCberspringen." ~ "text_false",
        x$type_answer == "image" & x$answeroption_06 != "www/skip.png" ~ "image_false",
        x$type_answer == "image" & x$answeroption_06 == "www/skip.png" ~ "correct",
    )
    if(check == "correct") {
        cli::cli_alert_success("Input for variable {.field answeroption_06} correct.")
        return(TRUE)
    } else if(check == "text_false") {
        cli::cli_alert_danger("Input for variable {.field answeroption_06} incorrect. Should have the following value based on variable {.field type_answer} = {.str {x$type_answer}}: {.str Frage \u00FCberspringen.}")
        return(FALSE)
    } else if(check == "image_false") {
        cli::cli_alert_danger("Input for variable {.field answeroption_06} incorrect. Based on variable {.field type_answer} = {.str {x$type_answer}}, it should have the following value: {.str www/skip.png}")
        return(FALSE)
    }
}

#' @noRd 
validate_answer_correct <- function(x) {
    answer_correct <- suppressWarnings(as.numeric(x$answer_correct))
    n_answer <- sum(!is.na(unlist(x[, paste0("answeroption_0", 1:5)])))
    error_case <- is.na(answer_correct) | !rlang::is_integerish(answer_correct) | answer_correct < 1 | answer_correct > n_answer
    if(error_case) {
        cli::cli_alert_danger("Input for variable {.field answer_correct} incorrect. Must be index (integer) of correct answer.")
        return(FALSE)
    } else {
        cli::cli_alert_success("Input for variable {.field answer_correct} corresponds to formalities.")
        return(TRUE)
    }
}

#' @noRd 
validate_if_answeroption <- function(x) {
    if(x$if_answeroption_06 != "Alles klar! Du hast die Aufgabe \u00FCbersprungen.") {
        cli::cli_alert_danger("Input for variable {.field if_answeroption_06} must remain unchanged, please select {.str Alles klar! Du hast die Aufgabe uebersprungen.}")
        return(FALSE)
    } else {
        cli::cli_alert_success("Input for variable {.field if_answeroptions_06} corresponds to formalities.")
    }

    n_answer <- sum(sapply( unlist(x[, paste0("answeroption_0", 1:5)]), function(x) !is_empty(x)))
    n_if_answer <- sum(sapply( unlist(x[, paste0("if_answeroption_0", 1:5)]), function(x) !is_empty(x)))
    if(n_answer != n_if_answer) {
        cli::cli_alert_danger("{n_answer} fields for {.field answer_option} have been filled out, but {n_if_answer} fields for {.field if_answeroption}. These must be identical!")
        return(FALSE)
    } else {
        cli::cli_alert_success("The number of answer options corresponds to the number of feedback blocks.")
        return(TRUE)
    }
}

#' @noRd 
validate_str <- function(x, var, valid, dist_len) {
    if(!x %in% valid) {
        str_dist <- stringdist::stringdist(valid, x, method = "lv")
        if(min(str_dist) < dist_len) {
            nearest_match <- which.min(str_dist)
            nm_valid <- valid[nearest_match] # nolint
            cli::cli_alert_danger(c("Invalid input for variable {.field {var}}: {.arg {x}}.", "Must be one of these categories: {.arg {valid}}.", "Did you mean {.arg {nm_valid}}?"))
            return(FALSE)
        } else {
            cli::cli_alert_danger("Invalid input for variable {.field {var}}: {.arg {x}}. Must be one of these categories: {.arg {valid}}")
            return(FALSE)
        }
    } else {
        cli::cli_alert_success("Input for variable {.field {var}} correct.")
        return(TRUE)
    }
}


#' @noRd
validate_file_header <- function(filename) {
    # read lines until the first header (# id_item)
    lines <- readLines(filename)
    header_line <- grep("^# id_item", lines)
    empty <- all(lines[seq_len(header_line) - 1] == "") 
    if(!empty) cli::cli_abort("The header of the file seems to contain some text. Please remove all lines before the {.field # id_item} header.")
}




#' @title Validate a newly created item for correct adherence to formatting
#' @param filename The filename of the md-file of the item
validate_item <- function(filename) {
    valid_names <- list(
        learning_area = c('Deskriptivstatistik', 'Grundlagen der Inferenzstatistik', 'Wahrscheinlichkeit', paste0("Zusammenhangsma", "\u00df", "e"), 'Regression', 'Poweranalyse', 'Gruppenvergleiche'),
        type_item = c('content', 'coding'),
        bloom_taxonomy = c('knowledge', 'comprehension', 'application'),
        theo_diff = c('easy', 'medium', 'hard'),
        type_stimulus = c('text', 'image'),
        type_answer = c('text', 'image')
    )
    validate_file_header(filename)
    x <- parse_md_to_csv(filename)
    v1 <- validate_id_item(x$id_item)
    v2 <- vector(mode = "list", length = length(names(valid_names)))
    names(v2) <- names(valid_names)
    for(nm in names(valid_names)) {
        v2[[nm]] <- validate_str(x = unlist(x[, nm]), var = nm, valid = valid_names[[nm]], dist_len = 5)
    }
    v3 <- validate_stimulus_image(x)
    v4 <- validate_answeroption(x)
    v5 <- validate_answer_correct(x)
    v6 <- validate_if_answeroption(x)
    if(any(!unname(unlist(list(v1, v2, v3, v4, v5, v6))))) cli::cli_abort("Some requirements are not met, please check the messages")
    return(TRUE)
}

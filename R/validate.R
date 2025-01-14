#' @noRd 
validate_id_item <- function(x) {
    x <- suppressWarnings(as.integer(x))
    if(!is.integer(x) | x <= 0 | is.na(x)) {
        cli::cli_alert_danger("The value for the variable {.field id_item} must be a positive integer.")
        return(FALSE)
    } else {
        cli::cli_alert_success("Input for variable {.field id_item correct.}")
        return(TRUE)
    }
}

#' @noRd 
is_img_path <- function(path) {
  img_regex <- "\\.(jpg|jpeg|png|gif|bmp|tiff|webp)$"
  grepl(img_regex, path, ignore.case = TRUE)
}

#' @noRd 
validate_stimulus_image <- function(x) {
    if(!is.na(x$stimulus_image)) {
        if(!is_img_path(x$stimulus_image)) {
            cli::cli_alert_danger("Input for variable {.field stimulus_image} is not a path with image.")
            return(FALSE)
        } else {
            cli::cli_alert_success("Input for variable {.field stimulus_image} correct.")
            return(TRUE)
        }
    }
}

#' @noRd 
validate_answeroption <- function(x) {
    type_answer <- x$type_answer
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

    n_answer <- sum(!is.na(unlist(x[, paste0("answeroption_0", 1:5)])))
    n_if_answer <- sum(!is.na(unlist(x[, paste0("if_answeroption_0", 1:5)])))
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
            cli::cli_alert_danger(c("Invalid input for variable {.field {var}}: {.arg {x}}.", "Must be one of these categories: {.arg {valid}}.", "Did you mean {.arg {valid[nearest_match ]}}?"))
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
}
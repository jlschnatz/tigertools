#' @noRd
.pass <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_success(msg, .envir = .envir)
  TRUE
}

#' @noRd
.fail <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_danger(msg, .envir = .envir)
  FALSE
}

#' @noRd
validate_id_item <- function(x) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(x) || x <= 0) {
    .fail("The value for the variable {.field id_item} must be a positive integer.")
  } else {
    .pass("Input for variable {.field id_item} correct.")
  }
}

#' @noRd
is_valid_img_path <- function(path) {
  path <- file.path("items", path)
  img_regex <- "^items/www/.*\\.(jpg|jpeg|png|gif|bmp|tiff|webp)$"
  if (length(path) > 1) {
    return(.fail("Muliple image paths in a single field detected. Only one image path is allowed."))
  }
  if (!grepl(img_regex, path, ignore.case = TRUE)) {
    return(.fail("Invalid naming scheme for image path. The path must start with {.emph www/} and end with a valid image file extension (e.g., .jpg, .png, .bmp, .tiff)."))
  }
  if (!file.exists(path)) {
    return(.fail("The image file does not exist. Please check the path."))
  }
  .pass("The image path is valid.")
}

#' @noRd
validate_stimulus_image <- function(x) {
  if (is.na(x$stimulus_image)) {
    return(TRUE)
  }
  if (!is_valid_img_path(x$stimulus_image)) {
    .fail("Input for variable {.field stimulus_image} must start with {.emph www/} and end with a valid image file extension (e.g., .jpg, .png, .bmp, .tiff). For instance: {.str www/image.png}.")
  } else {
    .pass("Input for variable {.field stimulus_image} correct.")
  }
}

#' @noRd
validate_str <- function(x, var, valid, dist_len = 5) {
  if (length(x) != 1L || is.na(x)) {
    return(.fail("Missing input for variable {.field {var}}. Must be one of these categories: {.arg {valid}}."))
  }
  if (x %in% valid) {
    return(.pass("Input for variable {.field {var}} correct."))
  }
  str_dist <- stringdist::stringdist(valid, x, method = "lv")
  if (min(str_dist) < dist_len) {
    nm_valid <- valid[which.min(str_dist)] # nolint
    .fail(c("Invalid input for variable {.field {var}}: {.arg {x}}.", "Must be one of these categories: {.arg {valid}}.", "Did you mean {.arg {nm_valid}}?"))
  } else {
    .fail("Invalid input for variable {.field {var}}: {.arg {x}}. Must be one of these categories: {.arg {valid}}")
  }
}

#' @noRd
validate_file_header <- function(filename) {
  # read lines until the first header (# id_item)
  lines <- readLines(filename, warn = FALSE)
  header_line <- grep("^# id_item", lines)
  if (length(header_line) == 0L) {
    cli::cli_abort("The file has no {.field # id_item} header. Please use the template from {.fn create}.")
  }
  empty <- all(trimws(lines[seq_len(header_line[1] - 1)]) == "")
  if (!empty) cli::cli_abort("The header of the file seems to contain some text. Please remove all lines before the {.field # id_item} header.")
}

# Every "# heading" must be a known item field - catches typos such as
# "# answeroption_1" or "# lower_answer_01", which would otherwise just be
# silently ignored (the field would read as empty).
#' @noRd
validate_sections <- function(filename) {
  lines <- readLines(filename, warn = FALSE)
  # "# comment" lines inside fenced code blocks (R code in a stimulus or
  # feedback) are not headings
  in_code <- cumsum(grepl("^\\s*(```|~~~)", lines)) %% 2 == 1
  headings <- trimws(sub("^#\\s+", "", grep("^#\\s+\\S", lines[!in_code], value = TRUE)))
  unknown <- setdiff(headings, .item_cols())
  dup <- unique(headings[duplicated(headings)])
  ok <- TRUE
  if (length(unknown) > 0L) {
    ok <- .fail("Unknown section(s): {.field {unknown}}. Check the spelling against the template.")
  }
  if (length(dup) > 0L) {
    ok <- .fail("Duplicated section(s): {.field {dup}}.")
  }
  if (ok) .pass("All sections are known item fields.") else FALSE
}

# -- Multiple-choice items ----------------------------------------------------

#' @noRd
validate_answeroption <- function(x) {
  skip <- c(text = "Frage \u00FCberspringen.", image = "www/skip.png")
  if (!x$type_answer %in% names(skip)) {
    return(FALSE) # type_answer itself is reported by validate_str()
  }
  if (identical(x$answeroption_06, skip[[x$type_answer]])) {
    .pass("Input for variable {.field answeroption_06} correct.")
  } else {
    expected <- skip[[x$type_answer]] # nolint
    .fail("Input for variable {.field answeroption_06} incorrect. Based on variable {.field type_answer} = {.str {x$type_answer}}, it should have the following value: {.str {expected}}")
  }
}

#' @noRd
validate_answer_correct <- function(x) {
  answer_correct <- suppressWarnings(as.numeric(x$answer_correct))
  n_answer <- sum(!is.na(unlist(x[, paste0("answeroption_0", 1:5)])))
  error_case <- is.na(answer_correct) || !rlang::is_integerish(answer_correct) || answer_correct < 1 || answer_correct > n_answer
  if (error_case) {
    .fail("Input for variable {.field answer_correct} incorrect. Must be the index (a single integer) of the correct answer option (1-{n_answer}).")
  } else {
    .pass("Input for variable {.field answer_correct} corresponds to formalities.")
  }
}

#' @noRd
validate_if_answeroption <- function(x) {
  ok <- TRUE
  if (!identical(x$if_answeroption_06, "Alles klar! Du hast die Aufgabe \u00FCbersprungen.")) {
    ok <- .fail("Input for variable {.field if_answeroption_06} must remain unchanged, please select {.str Alles klar! Du hast die Aufgabe \u00FCbersprungen.}")
  } else {
    .pass("Input for variable {.field if_answeroption_06} corresponds to formalities.")
  }
  n_answer <- sum(!is.na(unlist(x[, paste0("answeroption_0", 1:5)])))
  n_if_answer <- sum(!is.na(unlist(x[, paste0("if_answeroption_0", 1:5)])))
  if (n_answer != n_if_answer) {
    .fail("{n_answer} fields for {.field answer_option} have been filled out, but {n_if_answer} fields for {.field if_answeroption}. These must be identical!")
  } else {
    .pass("The number of answer options corresponds to the number of feedback blocks.") && ok
  }
}

#' @noRd
validate_mc_no_bounds <- function(x) {
  set <- .bound_cols()[!is.na(unlist(x[, .bound_cols()]))]
  if (length(set) > 0L) {
    .fail("Range bounds are only allowed for numeric items ({.field answer_mode} = {.str num}), but {.field {set}} is set.")
  } else {
    TRUE
  }
}

# -- Numeric items (see "Numeric item rules" in shinytigeR's CLAUDE.md) -------

# `raw` is the unaligned parse (all character), used to tell an unparseable
# value ("6.6a") apart from an empty one - after .as_item_schema() both are NA.
#' @noRd
validate_numeric_options <- function(x, raw) {
  ok <- TRUE
  n_opts <- 0L
  for (i in 1:6) {
    opt <- sprintf("answeroption_%02d", i)
    lo <- sprintf("lower_answeroption_%02d", i)
    up <- sprintf("upper_answeroption_%02d", i)
    fb <- sprintf("if_answeroption_%02d", i)
    raw_val <- .raw_field(raw, opt)
    raw_lo <- .raw_field(raw, lo)
    raw_up <- .raw_field(raw, up)
    has_val <- !is.na(raw_val)
    has_fb <- !is.na(x[[fb]])

    if (!has_val) {
      if (!is.na(raw_lo) || !is.na(raw_up) || has_fb) {
        ok <- .fail("{.field {opt}} is empty, but its bounds or feedback ({.field {lo}}/{.field {up}}/{.field {fb}}) are filled out.")
      }
      next
    }
    n_opts <- n_opts + 1L
    val <- suppressWarnings(as.numeric(.normalize_decimal(raw_val)))
    if (!is.finite(val)) {
      ok <- .fail("{.field {opt}} must be a number (e.g. {.str 6.67} or {.str 6,67}), got {.str {raw_val}}.")
      next
    }
    for (b in c(lo, up)) {
      rb <- .raw_field(raw, b)
      if (!is.na(rb) && !is.finite(suppressWarnings(as.numeric(.normalize_decimal(rb))))) {
        ok <- .fail("{.field {b}} must be a number or empty, got {.str {rb}}.")
      }
    }
    l <- x[[lo]]
    u <- x[[up]]
    if (xor(is.na(raw_lo), is.na(raw_up))) {
      ok <- .fail("{.field {opt}}: set both {.field {lo}} and {.field {up}}, or neither (= exact match only).")
    } else if (!is.na(l) && !is.na(u)) {
      if (l > u) {
        ok <- .fail("{.field {opt}}: lower bound {l} is greater than upper bound {u}.")
      } else if (val < l || val > u) {
        ok <- .fail("{.field {opt}}: value {val} lies outside its own range [{l}, {u}].")
      }
    }
    if (!has_fb) {
      ok <- .fail("{.field {opt}} has no feedback in {.field {fb}}.")
    }
  }
  if (n_opts == 0L) {
    ok <- .fail("A numeric item needs at least one answer option.")
  }
  if (ok) .pass("Numeric answer options, bounds and feedback are consistent.") else FALSE
}

#' @noRd
.raw_field <- function(raw, col) {
  v <- if (col %in% names(raw)) as.character(raw[[col]][1]) else NA_character_
  if (is.na(v) || trimws(v) %in% c("", "NA")) NA_character_ else trimws(v)
}

# Each option's accepted interval; an exact-match option is the point [v, v].
# Inclusive bounds, so touching intervals (2.59 in both) count as overlap.
#' @noRd
validate_numeric_overlap <- function(x) {
  iv <- do.call(rbind, lapply(1:6, function(i) {
    v <- suppressWarnings(as.numeric(x[[sprintf("answeroption_%02d", i)]]))
    if (is.na(v)) return(NULL)
    l <- x[[sprintf("lower_answeroption_%02d", i)]]
    u <- x[[sprintf("upper_answeroption_%02d", i)]]
    if (is.na(l) || is.na(u)) {
      l <- v
      u <- v
    }
    data.frame(idx = i, lo = l, hi = u)
  }))
  if (is.null(iv) || nrow(iv) < 2L) {
    return(TRUE)
  }
  iv <- iv[order(iv$lo, iv$hi), ]
  clashes <- character(0)
  for (k in seq_len(nrow(iv) - 1L)) {
    for (m in (k + 1L):nrow(iv)) {
      if (iv$lo[m] <= iv$hi[k]) {
        clashes <- c(clashes, sprintf("%02d and %02d", iv$idx[k], iv$idx[m]))
      }
    }
  }
  if (length(clashes) > 0L) {
    .fail("The accepted ranges of answer options {clashes} overlap. A typed value must match at most one option.")
  } else {
    .pass("The accepted ranges of the answer options do not overlap.")
  }
}

#' @noRd
validate_answer_correct_num <- function(x) {
  ac <- trimws(as.character(x$answer_correct))
  present <- which(!is.na(unlist(x[, sprintf("answeroption_%02d", 1:6)])))
  if (is.na(ac) || !grepl("^\\d+(\\s*;\\s*\\d+)*$", ac)) {
    return(.fail("Input for variable {.field answer_correct} incorrect. Must be one option number (e.g. {.str 1}) or several separated by {.str ;} (e.g. {.str 1;2})."))
  }
  idx <- as.integer(trimws(strsplit(ac, ";", fixed = TRUE)[[1]]))
  if (anyDuplicated(idx)) {
    return(.fail("{.field answer_correct} lists an option more than once: {.str {ac}}."))
  }
  bad <- setdiff(idx, present)
  if (length(bad) > 0L) {
    return(.fail("{.field answer_correct} refers to empty answer option(s): {bad}."))
  }
  .pass("Input for variable {.field answer_correct} corresponds to formalities ({length(idx)} correct option{?s}).")
}

#' @title Validate a newly created item for correct adherence to formatting
#' @description Checks an item's markdown file against the item rules: the
#'   shared fields for all items, plus either the multiple-choice rules or the
#'   numeric-item rules depending on its `answer_mode`. Every check prints a
#'   message; if any check fails, the function aborts at the end.
#' @param filename The filename of the md-file of the item
#' @return `TRUE` (invisibly aborts with an error if a requirement is not met)
validate_item <- function(filename) {
  validate_file_header(filename)
  checks <- list(validate_sections(filename))
  raw <- parse_md_to_csv(filename)
  x <- .as_item_schema(raw)[1, , drop = FALSE]

  checks <- c(checks, validate_id_item(x$id_item))
  valid <- .valid_categories()
  for (nm in names(valid)) {
    checks <- c(checks, validate_str(x[[nm]], var = nm, valid = valid[[nm]]))
  }
  checks <- c(checks, validate_stimulus_image(x))

  if (identical(x$answer_mode, "num")) {
    checks <- c(
      checks,
      validate_numeric_options(x, raw),
      validate_numeric_overlap(x),
      validate_answer_correct_num(x)
    )
  } else if (identical(x$answer_mode, "mc")) {
    checks <- c(
      checks,
      validate_str(x$type_answer, var = "type_answer", valid = c("text", "image")),
      validate_answeroption(x),
      validate_answer_correct(x),
      validate_if_answeroption(x),
      validate_mc_no_bounds(x)
    )
  }
  if (!all(unlist(checks))) cli::cli_abort("Some requirements are not met, please check the messages")
  TRUE
}

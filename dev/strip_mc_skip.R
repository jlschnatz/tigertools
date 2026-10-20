# One-off cleanup: removes the old skip option from MC item files.
#
# MC items used to end in a skip option ("Frage überspringen." for text
# answers, "www/skip.png" for image answers) with the fixed feedback "Alles
# klar! Du hast die Aufgabe übersprungen.". The app now has its own skip
# button, and validate_item() rejects the old option. This script empties both
# sections (the option and its feedback) in every item file that still has
# them, so the files match the app's migrated db_item.sqlite again
# (shinytigeR's dev/migrate_drop_mc_skip.R does the same for the database).
#
# A file is only changed if the skip value is its LAST non-empty option, its
# feedback is the fixed skip text (or empty) and it isn't answer_correct.
# Anything else is reported and left untouched. Only the two section bodies are
# replaced (by an empty line, including any template comments in them); all
# other lines, the line endings and the final newline stay as they are.
#
# Usage, from the item database folder (the one containing items/):
#
#   Rscript <path-to-tigertools>/dev/strip_mc_skip.R                       # dry run: only reports
#   Rscript <path-to-tigertools>/dev/strip_mc_skip.R --apply               # backs up items/, then rewrites
#   Rscript <path-to-tigertools>/dev/strip_mc_skip.R items_psybsc7 --apply # another item folder
#
# With --apply, the folder is first copied to <folder>_backup_<timestamp>/.
# Afterwards, push the cleaned files with push_all(overwrite = TRUE).
#
# Not part of the package (dev/ is in .Rbuildignore); tested in
# tests/testthat/test-strip.R. Delete both once all item files are cleaned.

strip_mc_skip <- function(item_folder = "items", apply = FALSE) {
  if (!dir.exists(item_folder)) {
    stop("No item folder found at ", item_folder, call. = FALSE)
  }
  files <- list.files(item_folder, pattern = "\\.md$", full.names = TRUE)
  todo <- list()
  manual <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    sec <- md_sections(lines)
    res <- find_mc_skip(sec)
    if (is.character(res)) {
      manual <- c(manual, sprintf("%s: %s", basename(f), res))
    } else if (!is.null(res)) {
      todo[[f]] <- list(lines = lines, sec = sec, pos = res)
    }
  }

  message(sprintf("%d item file(s) in %s, %d with a skip option to remove.", length(files), item_folder, length(todo)))
  if (length(manual) > 0L) {
    message("Check by hand (left untouched):\n", paste0("  ", manual, collapse = "\n"))
  }
  if (!apply || length(todo) == 0L) {
    if (!apply && length(todo) > 0L) {
      message("Dry run, nothing changed. Run again with --apply to back up the folder and rewrite the files.")
    }
    return(invisible(names(todo)))
  }

  backup <- sprintf("%s_backup_%s", sub("/+$", "", item_folder), format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir.create(backup)
  ok <- file.copy(
    list.files(item_folder, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    backup, recursive = TRUE, copy.date = TRUE
  )
  if (!all(ok)) stop("Backup to ", backup, " failed, nothing changed.", call. = FALSE)
  message("Backed up ", item_folder, " to ", backup, ".")

  for (f in names(todo)) {
    t <- todo[[f]]
    # Keep the file's line endings (some are CRLF) and final newline
    raw <- readBin(f, "raw", file.size(f))
    eol <- if (any(raw == as.raw(13L))) "\r\n" else "\n"
    last_eol <- length(raw) > 0L && raw[length(raw)] == as.raw(10L)
    text <- paste0(paste(enc2utf8(strip_sections(t$lines, t$sec, t$pos)), collapse = eol), if (last_eol) eol)
    writeBin(charToRaw(text), f)
  }
  message(sprintf("Removed the skip option from %d file(s).", length(todo)))
  invisible(names(todo))
}

# Section layout of an item file, split the same way as parse_md_to_csv():
# headings are "# name" lines outside fenced code blocks.
md_sections <- function(lines) {
  trimmed <- trimws(sub("\r$", "", lines))
  is_fence <- grepl("^\\s*(```|~~~)", lines)
  in_code <- (cumsum(is_fence) %% 2 == 1) & !is_fence
  starts <- which(!in_code & !is_fence & grepl("^#\\s+\\S", trimmed))
  ends <- c(starts[-1] - 1L, length(lines))
  values <- vapply(seq_along(starts), function(k) {
    s <- starts[k]
    e <- ends[k]
    body <- if (e > s) trimmed[(s + 1L):e] else character(0)
    v <- gsub("<![-]{2,3}(.|\\s|\\r)*?[-]{2,3}>", "", paste(body, collapse = "\n"))
    v <- trimws(v)
    if (!nzchar(v) || v == "NA") NA_character_ else v
  }, character(1))
  sec <- data.frame(
    name = trimws(sub("^#\\s+", "", trimmed[starts])),
    start = starts, end = ends, value = values
  )
  sec[!duplicated(sec$name), , drop = FALSE]
}

# NULL = nothing to do; a string = why the file needs a manual check; else the
# position (1-6) of the skip option to remove.
find_mc_skip <- function(sec) {
  val <- function(nm) {
    v <- sec$value[sec$name == nm]
    if (length(v) == 0L) NA_character_ else v
  }
  mode <- val("answer_mode")
  if (!is.na(mode) && mode != "mc") return(NULL)
  opts <- vapply(sprintf("answeroption_%02d", 1:6), val, character(1))
  hit <- which(opts %in% c("Frage überspringen.", "www/skip.png"))
  if (length(hit) == 0L) return(NULL)
  if (length(hit) > 1L) return("more than one skip option")
  filled <- which(!is.na(opts))
  if (hit != max(filled)) return(sprintf("skip option %02d is not the last option", hit))
  fb <- val(sprintf("if_answeroption_%02d", hit))
  if (!is.na(fb) && fb != "Alles klar! Du hast die Aufgabe übersprungen.") {
    return(sprintf("if_answeroption_%02d is not the skip feedback", hit))
  }
  # answer_correct counts filled options only (gaps don't count)
  if (identical(val("answer_correct"), as.character(length(filled)))) {
    return("the skip option is answer_correct")
  }
  hit
}

# Replaces the bodies of answeroption_0X and if_answeroption_0X by one empty line.
strip_sections <- function(lines, sec, pos) {
  targets <- sec[sec$name %in% sprintf(c("answeroption_%02d", "if_answeroption_%02d"), pos), ]
  # From the bottom up, so earlier line numbers stay valid
  for (k in order(targets$start, decreasing = TRUE)) {
    s <- targets$start[k]
    e <- targets$end[k]
    lines <- c(lines[seq_len(s)], "", if (e < length(lines)) lines[(e + 1L):length(lines)])
  }
  lines
}

# Only auto-run when invoked directly (Rscript). Tests set
# .strip_mc_skip_sourced_only before sourcing this file to get the functions
# without running them.
if (!exists(".strip_mc_skip_sourced_only", inherits = FALSE)) {
  args <- commandArgs(trailingOnly = TRUE)
  strip_mc_skip(
    item_folder = c(setdiff(args, "--apply"), "items")[1],
    apply = "--apply" %in% args
  )
}

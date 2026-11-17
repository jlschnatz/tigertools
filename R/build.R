#' @title Build the item pool the app runs on
#' @description Builds `db_item.sqlite` and `img_item/` for shinytigeR from
#'   scratch: every item file is validated and parsed, the item difficulties
#'   are merged in, and the images the items use are copied. Unlike [push()],
#'   nothing is updated in place, so the same inputs always give the same pool
#'   (an item whose file was deleted is gone, too).
#' @details Difficulties come from two CSV files, each with an id column
#'   (`id_item` or `item_id`) and a difficulty column (`irt_diff`,
#'   `difficulty_median` or `difficulty_logit`, the first one present):
#'
#'   * `calibrated`: estimated from students' answers (the IRT calibration);
#'     these items get `irt_provisional = 0`.
#'   * `provisional`: preliminary estimates for items without (enough) answers
#'     yet; these items get `irt_provisional = 1`.
#'
#'   A calibrated difficulty wins over a provisional one, so an item moves to
#'   calibrated as soon as a calibration covers it. Difficulties for ids that
#'   aren't in the pool are ignored with a note. Every item needs a difficulty,
#'   else the build stops (unless `allow_missing = TRUE`; those items get
#'   `irt_diff = NULL` and don't count towards the app's estimate).
#'
#'   The pool gets a second table, `build_info`, with an md5 hash of each input
#'   (the item files and images together count as one), so a pool on the
#'   server can be traced back to what it was built from. It holds no date:
#'   building twice from the same inputs gives an identical file.
#'
#'   `out_dir` must be a folder for the build only (it replaces its
#'   `db_item.sqlite` and `img_item/` as a whole); it refuses a folder holding
#'   the app's user or credentials databases. Copy the result into the app's
#'   DB folder from there.
#' @param out_dir Folder to write `db_item.sqlite` and `img_item/` into
#'   (created if missing).
#' @param calibrated,provisional Paths to the difficulty CSV files, see
#'   Details. `provisional = NULL` if there are none.
#' @param path Folder with the item files (default `"./items"`); images are
#'   expected in its `www/` subfolder, as in the item files' `www/...` paths.
#' @param allow_missing Logical: build even if some items have no difficulty.
#' @return The path of the written `db_item.sqlite`, invisibly.
#' @export
build_release <- function(out_dir = "./release",
                          calibrated = "./difficulty/calibrated.csv",
                          provisional = "./difficulty/provisional.csv",
                          path = "./items",
                          allow_missing = FALSE) {
  if (!dir.exists(path)) cli::cli_abort("The item folder {.file {path}} does not exist.")
  for (f in c(calibrated, provisional)) {
    if (!file.exists(f)) cli::cli_abort("The difficulty file {.file {f}} does not exist.")
  }
  if (any(file.exists(file.path(out_dir, c("db_user.sqlite", "db_credentials.sqlite"))))) {
    cli::cli_abort(c(
      "{.file {out_dir}} looks like the app's DB folder (it has user or credentials databases).",
      "i" = "Build into a separate folder and copy {.file db_item.sqlite} and {.file img_item/} from there."
    ))
  }

  # ── Items ──
  files <- sort(list.files(path, pattern = "\\.md$", full.names = TRUE))
  if (length(files) == 0L) cli::cli_abort("No item files in {.file {path}}.")
  items <- .read_items(files, path)
  cli::cli_alert_success("{nrow(items)} item{?s} validated.")

  # ── Difficulties ──
  cal <- .read_difficulties(calibrated)
  prov <- if (is.null(provisional)) .read_difficulties(NULL) else .read_difficulties(provisional)
  for (src in list(list("calibrated", cal), list("provisional", prov))) {
    extra <- setdiff(src[[2]]$id_item, items$id_item)
    if (length(extra) > 0L) {
      cli::cli_alert_info("{length(extra)} {src[[1]]} difficult{?y/ies} for ids not in the pool, ignored: {.val {extra}}")
    }
  }
  i_cal <- match(items$id_item, cal$id_item)
  i_prov <- match(items$id_item, prov$id_item)
  items$irt_diff <- ifelse(!is.na(i_cal), cal$irt_diff[i_cal], prov$irt_diff[i_prov])
  items$irt_provisional <- ifelse(!is.na(i_cal), 0L, ifelse(!is.na(i_prov), 1L, NA_integer_))
  n_cal <- sum(items$irt_provisional %in% 0L)
  n_prov <- sum(items$irt_provisional %in% 1L)
  cli::cli_alert_success("Difficulties: {n_cal} calibrated, {n_prov} provisional.")

  none <- items$id_item[is.na(items$irt_diff)]
  if (length(none) > 0L) {
    if (!allow_missing) {
      cli::cli_abort(c(
        "{length(none)} item{?s} without a difficulty: {.val {none}}",
        "i" = "Add them to the provisional difficulties, or build with {.code allow_missing = TRUE}."
      ))
    }
    cli::cli_alert_warning("{length(none)} item{?s} without a difficulty (they won't count towards the estimate): {.val {none}}")
  }

  # ── Images ──
  imgs <- .item_images(items)
  img_src <- file.path(path, "www", imgs)

  # ── Write ──
  # Into a temporary folder first, then swapped in, so a failed build never
  # leaves a half-written pool behind
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("build_", tmpdir = out_dir)
  dir.create(file.path(tmp, "img_item"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  db <- file.path(tmp, "db_item.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  types <- .item_field_types(names(items))
  types[["irt_diff"]] <- "REAL"
  types[["irt_provisional"]] <- "INTEGER"
  DBI::dbWriteTable(con, "item_db", items, field.types = types)
  DBI::dbWriteTable(con, "build_info", data.frame(
    input = c("items", "calibrated", "provisional"),
    file = c(basename(normalizePath(path)), basename(calibrated), if (is.null(provisional)) NA else basename(provisional)),
    md5 = c(
      .md5_combined(c(files, img_src)),
      unname(tools::md5sum(calibrated)),
      if (is.null(provisional)) NA else unname(tools::md5sum(provisional))
    ),
    n_items = c(nrow(items), n_cal, n_prov)
  ))
  DBI::dbDisconnect(con)
  if (length(imgs) > 0L && !all(file.copy(img_src, file.path(tmp, "img_item", imgs)))) {
    cli::cli_abort("Could not copy the item images.")
  }

  out_db <- file.path(out_dir, "db_item.sqlite")
  out_img <- file.path(out_dir, "img_item")
  unlink(out_img, recursive = TRUE)
  if (!file.rename(db, out_db) || !file.rename(file.path(tmp, "img_item"), out_img)) {
    cli::cli_abort("Could not write to {.file {out_dir}}.")
  }
  cli::cli_alert_success("Pool written: {.file {normalizePath(out_db)}} ({nrow(items)} items, {length(imgs)} image{?s}).")
  invisible(out_db)
}

# Validates and parses all item files into one data frame in schema order.
# Validation runs for every file before giving up, so one build lists all
# broken items. validate_item() resolves image paths against the item
# folder's parent, hence the working directory change.
#' @noRd
.read_items <- function(files, path) {
  files <- normalizePath(files)
  old <- setwd(dirname(normalizePath(path)))
  on.exit(setwd(old), add = TRUE)

  bad <- character(0)
  for (f in files) {
    ok <- tryCatch(suppressMessages(validate_item(f)), error = function(e) FALSE)
    if (!isTRUE(ok)) bad <- c(bad, basename(f))
  }
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{length(bad)} item file{?s} failed validation: {.file {bad}}",
      "i" = "Run {.code validate_item()} on a file to see what is wrong."
    ))
  }

  rows <- lapply(files, function(f) .as_item_schema(parse_md_to_csv(f)))
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    for (col in setdiff(cols, names(r))) r[[col]] <- NA
    r[, cols, drop = FALSE]
  })
  items <- do.call(rbind, rows)
  dup <- unique(items$id_item[duplicated(items$id_item)])
  if (length(dup) > 0L) {
    cli::cli_abort("Several item files have the same {.field id_item}: {.val {dup}}")
  }
  items <- items[order(items$id_item), , drop = FALSE]
  rownames(items) <- NULL
  items
}

# Reads a difficulty CSV into data.frame(id_item, irt_diff). NULL gives an
# empty one.
#' @noRd
.read_difficulties <- function(file) {
  if (is.null(file)) return(data.frame(id_item = integer(0), irt_diff = numeric(0)))
  df <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  id_col <- intersect(c("id_item", "item_id"), names(df))[1]
  diff_col <- intersect(c("irt_diff", "difficulty_median", "difficulty_logit"), names(df))[1]
  if (is.na(id_col) || is.na(diff_col)) {
    cli::cli_abort(c(
      "{.file {file}} needs an id column ({.field id_item} or {.field item_id}) and a difficulty column ({.field irt_diff}, {.field difficulty_median} or {.field difficulty_logit}).",
      "i" = "It has: {.field {names(df)}}"
    ))
  }
  out <- data.frame(
    id_item = suppressWarnings(as.integer(df[[id_col]])),
    irt_diff = suppressWarnings(as.numeric(df[[diff_col]]))
  )
  if (anyNA(out)) cli::cli_abort("{.file {file}} has missing or non-numeric values.")
  dup <- unique(out$id_item[duplicated(out$id_item)])
  if (length(dup) > 0L) cli::cli_abort("{.file {file}} lists ids more than once: {.val {dup}}")
  out
}

# File names of all images the items use (stimulus and image answer options).
#' @noRd
.item_images <- function(items) {
  cols <- intersect(c("stimulus_image", sprintf("answeroption_%02d", 1:6)), names(items))
  vals <- unlist(items[cols], use.names = FALSE)
  sort(unique(basename(vals[!is.na(vals) & grepl("^www/.*\\.(jpe?g|png|gif|bmp|tiff|webp|svg)$", vals, ignore.case = TRUE)])))
}

# One md5 over several files (their names and contents, order-independent).
#' @noRd
.md5_combined <- function(files) {
  files <- sort(files)
  sums <- paste(basename(files), unname(tools::md5sum(files)))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(sums, tmp)
  unname(tools::md5sum(tmp))
}

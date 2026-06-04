#!/usr/bin/env Rscript

input_pattern <- "ppcmbe2/all_LModE_nouns.csv"
meta_file <- "meta.csv"
default_output_file <- "all_LModE_nouns.csv"

args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_output_file

required_annis_columns <- c(
  "1_id",
  "1_span",
  "1_anno_default_ns::lemma",
  "2_span",
  "4_span",
  "4_anno_default_ns::lemma",
  "4_anno_ptb::pos",
  "meta_Author",
  "meta_Date_of_composition",
  "meta_Filename",
  "meta_Genre"
)

read_annis_export <- function(path) {
  read.delim(
    path,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fill = TRUE,
    quote = "",
    comment.char = "",
    colClasses = "character",
    na.strings = character(0)
  )
}

read_meta <- function(path) {
  read.csv(
    path,
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  missing <- is.na(x)
  x[missing] <- ""
  x <- trimws(x)
  x[missing] <- NA_character_
  x
}

validate_annis_columns <- function(df, path) {
  missing_columns <- setdiff(required_annis_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Missing required ANNIS column(s) in %s: %s",
        normalizePath(path, winslash = "/", mustWork = FALSE),
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}

build_annis_subset <- function(df, path) {
  validate_annis_columns(df, path)

  composition_year <- extract_composition_year(clean_text(df[["meta_Date_of_composition"]]))
  noun_pos <- df[["4_anno_ptb::pos"]]
  is_plural <- !is.na(noun_pos) & grepl("NS", noun_pos, fixed = TRUE)

  data.frame(
    id = clean_text(df[["1_id"]]),
    q_token = clean_text(df[["1_span"]]),
    q_lemma = clean_text(df[["1_anno_default_ns::lemma"]]),
    context = clean_text(df[["2_span"]]),
    token = clean_text(df[["4_span"]]),
    lemma = clean_text(df[["4_anno_default_ns::lemma"]]),
    plural = ifelse(is_plural, "1", ""),
    singular = ifelse(is_plural, "", "1"),
    author = clean_text(df[["meta_Author"]]),
    period = assign_period(composition_year),
    filename = clean_text(df[["meta_Filename"]]),
    genre = clean_text(df[["meta_Genre"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

extract_composition_year <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  year <- rep(NA_integer_, length(x))

  has_four_digit_year <- grepl("\\d{4}", x, perl = TRUE)
  year[has_four_digit_year] <- as.integer(
    sub(".*?(\\d{4}).*", "\\1", x[has_four_digit_year], perl = TRUE)
  )

  has_decade_x_year <- is.na(year) & grepl("\\d{3}x", x, perl = TRUE)
  year[has_decade_x_year] <- as.integer(
    paste0(sub(".*?(\\d{3})x.*", "\\1", x[has_decade_x_year], perl = TRUE), "5")
  )

  year
}

assign_period <- function(year) {
  period <- rep(NA_character_, length(year))

  period[!is.na(year) & year >= 1640 & year <= 1760] <- "l1"
  period[!is.na(year) & year >= 1761 & year <= 1810] <- "l2"
  period[!is.na(year) & year >= 1811 & year <= 1860] <- "l3"
  period[!is.na(year) & year >= 1861 & year <= 1920] <- "l4"

  period
}

files <- sort(Sys.glob(input_pattern))

if (length(files) == 0) {
  stop(sprintf("No files found matching '%s' in %s", input_pattern, normalizePath(getwd())))
}

if (!file.exists(meta_file)) {
  stop(sprintf("Required metadata file not found: %s", meta_file))
}

message(sprintf("Found %d ANNIS export file(s).", length(files)))

datasets <- lapply(files, function(path) build_annis_subset(read_annis_export(path), path))
merged <- do.call(rbind, datasets)
meta <- read_meta(meta_file)
names(meta) <- tolower(names(meta))

if (!"lemma" %in% names(meta)) {
  stop("Required column missing in meta.csv: lemma")
}

duplicate_meta_lemmas <- unique(meta$lemma[duplicated(meta$lemma)])
if (length(duplicate_meta_lemmas) > 0) {
  stop(
    sprintf(
      "Duplicate lemma values found in meta.csv: %s",
      paste(head(sort(duplicate_meta_lemmas), 10), collapse = ", ")
    )
  )
}

meta_match <- match(merged$lemma, meta$lemma)
if (anyNA(meta_match)) {
  missing_lemmas <- unique(merged$lemma[is.na(meta_match)])
  stop(
    sprintf(
      "Missing metadata for lemma values: %s",
      paste(head(sort(missing_lemmas), 10), collapse = ", ")
    )
  )
}

meta_columns <- setdiff(names(meta), "lemma")
merged <- cbind(merged, meta[meta_match, meta_columns, drop = FALSE])

write.table(
  merged,
  file = output_file,
  sep = ";",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  na = ""
)

message(sprintf("Merged %d row(s) into %s", nrow(merged), normalizePath(output_file, winslash = "/", mustWork = FALSE)))
message(sprintf("Assigned period for %d row(s).", sum(!is.na(merged$period))))
message(sprintf("Joined metadata for %d row(s) from %s", nrow(merged), normalizePath(meta_file, winslash = "/", mustWork = FALSE)))

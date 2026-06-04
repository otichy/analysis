#!/usr/bin/env Rscript

input_file <- "Q_ycoe/quantifiers_ycoe.csv"
meta_file <- "meta.csv"
q_lemma_file <- "Q lemma.xlsx"
default_output_file <- "all_OE_nouns.csv"

args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_output_file

required_ycoe_columns <- c(
  "1_id",
  "1_span",
  "1_span_lemma",
  "1_anno_ptb::pos",
  "2_id",
  "2_span",
  "2_span_lemma",
  "2_anno_ptb::pos",
  "3_id",
  "3_span",
  "3_anno_ptb::cat",
  "meta_Cameron_number",
  "meta_DOE_short_title",
  "meta_Dialect",
  "meta_Edition",
  "meta_Genre",
  "meta_Latin_translation",
  "meta_Manuscript",
  "meta_Manuscript_date",
  "meta_Remarks",
  "meta_Sawyer_number",
  "meta_Word_count",
  "meta_filename",
  "meta_period",
  "meta_text_name",
  "PDE word"
)

core_ycoe_columns <- c(
  "1_id",
  "1_span",
  "1_span_lemma",
  "2_id",
  "2_span",
  "2_span_lemma",
  "3_id",
  "3_span",
  "meta_filename",
  "meta_period",
  "PDE word"
)

clean_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  missing <- is.na(x)
  x[missing] <- ""
  x <- trimws(x)
  x[x %in% c("NULL", "'NULL'")] <- ""
  x
}

normalize_path_label <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

read_semicolon_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Input file not found: %s", normalize_path_label(path)))
  }

  data <- read.csv(
    path,
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0),
    comment.char = "",
    fileEncoding = "UTF-8"
  )
  data[] <- lapply(data, clean_text)
  data
}

require_columns <- function(df, required, source_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing required column(s) in %s: %s",
        source_name,
        paste(missing, collapse = ", ")
      )
    )
  }
}

validate_nonblank_columns <- function(df, columns, source_name) {
  for (field in columns) {
    blank <- clean_text(df[[field]]) == ""
    if (any(blank)) {
      stop(
        sprintf(
          "%s: blank value(s) in '%s' at data row(s): %s",
          source_name,
          field,
          paste(head(which(blank), 10), collapse = ", ")
        )
      )
    }
  }
}

read_q_lemma_map <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required q lemma file not found: %s", normalize_path_label(path)))
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read Q lemma.xlsx")
  }

  mapping <- as.data.frame(
    readxl::read_excel(path),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(mapping) <- tolower(names(mapping))
  require_columns(mapping, c("word", "lemma"), basename(path))

  mapping <- data.frame(
    word = tolower(clean_text(mapping[["word"]])),
    lemma = clean_text(mapping[["lemma"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  mapping <- mapping[mapping$word != "" | mapping$lemma != "", , drop = FALSE]

  incomplete <- mapping$word == "" | mapping$lemma == ""
  if (any(incomplete)) {
    stop(
      sprintf(
        "Blank word/lemma value(s) found in %s at data row(s): %s",
        normalize_path_label(path),
        paste(head(which(incomplete), 10), collapse = ", ")
      )
    )
  }

  grouped <- split(mapping$lemma, mapping$word)
  conflicting <- names(grouped)[vapply(grouped, function(values) {
    length(unique(values)) > 1
  }, logical(1))]
  if (length(conflicting) > 0) {
    details <- vapply(head(conflicting, 10), function(word) {
      sprintf("%s -> %s", word, paste(unique(grouped[[word]]), collapse = " / "))
    }, character(1))
    stop(
      sprintf(
        "Conflicting duplicate word mapping(s) found in %s: %s",
        normalize_path_label(path),
        paste(details, collapse = "; ")
      )
    )
  }

  unique_mapping <- mapping[!duplicated(mapping$word), , drop = FALSE]
  stats::setNames(unique_mapping$lemma, unique_mapping$word)
}

read_meta <- function(path) {
  meta <- read_semicolon_csv(path)
  names(meta) <- tolower(names(meta))
  require_columns(
    meta,
    c("lemma", "pde_count", "semantics", "orig", "oe_etymon", "oe_class", "oe_stem", "oe_gender"),
    basename(path)
  )

  duplicate_lemmas <- unique(meta$lemma[duplicated(meta$lemma)])
  if (length(duplicate_lemmas) > 0) {
    stop(
      sprintf(
        "Duplicate lemma values found in %s: %s",
        normalize_path_label(path),
        paste(head(sort(duplicate_lemmas), 10), collapse = ", ")
      )
    )
  }

  meta
}

map_q_lemmas <- function(source_lemmas, q_lemma_map, ids) {
  keys <- tolower(clean_text(source_lemmas))
  mapped <- unname(q_lemma_map[keys])
  missing <- is.na(mapped)
  if (any(missing)) {
    missing_values <- unique(source_lemmas[missing])
    missing_ids <- ids[missing]
    stop(
      sprintf(
        "Unmapped YCOE quantifier lemma(s): %s. Example row ID(s): %s",
        paste(head(sort(missing_values), 10), collapse = ", "),
        paste(head(missing_ids, 10), collapse = ", ")
      )
    )
  }
  mapped
}

build_ycoe_subset <- function(df, q_lemma_map, meta) {
  source_name <- basename(input_file)
  require_columns(df, required_ycoe_columns, source_name)
  validate_nonblank_columns(df, core_ycoe_columns, source_name)

  pde_lemma <- clean_text(df[["PDE word"]])
  meta_match <- match(pde_lemma, meta$lemma)
  if (anyNA(meta_match)) {
    missing_values <- unique(pde_lemma[is.na(meta_match)])
    missing_ids <- df[["1_id"]][is.na(meta_match)]
    stop(
      sprintf(
        "Missing metadata for PDE lemma value(s): %s. Example noun ID(s): %s",
        paste(head(sort(missing_values), 10), collapse = ", "),
        paste(head(missing_ids, 10), collapse = ", ")
      )
    )
  }

  subset <- data.frame(
    id = clean_text(df[["2_id"]]),
    q_token = clean_text(df[["2_span"]]),
    q_form = clean_text(df[["2_span_lemma"]]),
    q_lemma = map_q_lemmas(df[["2_span_lemma"]], q_lemma_map, df[["2_id"]]),
    noun_id = clean_text(df[["1_id"]]),
    token = clean_text(df[["1_span"]]),
    oe_lemma = clean_text(df[["1_span_lemma"]]),
    lemma = pde_lemma,
    context_id = clean_text(df[["3_id"]]),
    context = clean_text(df[["3_span"]]),
    noun_pos = clean_text(df[["1_anno_ptb::pos"]]),
    q_pos = clean_text(df[["2_anno_ptb::pos"]]),
    context_cat = clean_text(df[["3_anno_ptb::cat"]]),
    cameron_number = clean_text(df[["meta_Cameron_number"]]),
    doe_short_title = clean_text(df[["meta_DOE_short_title"]]),
    dialect = clean_text(df[["meta_Dialect"]]),
    edition = clean_text(df[["meta_Edition"]]),
    genre = clean_text(df[["meta_Genre"]]),
    latin_translation = clean_text(df[["meta_Latin_translation"]]),
    manuscript = clean_text(df[["meta_Manuscript"]]),
    manuscript_date = clean_text(df[["meta_Manuscript_date"]]),
    remarks = clean_text(df[["meta_Remarks"]]),
    sawyer_number = clean_text(df[["meta_Sawyer_number"]]),
    word_count = clean_text(df[["meta_Word_count"]]),
    filename = clean_text(df[["meta_filename"]]),
    period = clean_text(df[["meta_period"]]),
    text_name = clean_text(df[["meta_text_name"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  meta_columns <- setdiff(names(meta), "lemma")
  cbind(subset, meta[meta_match, meta_columns, drop = FALSE])
}

write_output <- function(df, path) {
  write.table(
    df,
    file = path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

if (!file.exists(input_file)) {
  stop(sprintf("Required YCOE export not found: %s", normalize_path_label(input_file)))
}
if (!file.exists(meta_file)) {
  stop(sprintf("Required metadata file not found: %s", normalize_path_label(meta_file)))
}

ycoe <- read_semicolon_csv(input_file)
q_lemma_map <- read_q_lemma_map(q_lemma_file)
meta <- read_meta(meta_file)
merged <- build_ycoe_subset(ycoe, q_lemma_map, meta)

if (nrow(merged) != nrow(ycoe)) {
  stop(sprintf("Row count changed while processing YCOE data: %d input row(s), %d output row(s).", nrow(ycoe), nrow(merged)))
}

write_output(merged, output_file)

message(sprintf("YCOE rows read: %d", nrow(ycoe)))
message(sprintf("YCOE rows written: %d", nrow(merged)))
message(sprintf("Joined metadata for %d row(s) from %s", nrow(merged), normalize_path_label(meta_file)))
message(sprintf("Wrote OE noun file: %s", normalize_path_label(output_file)))

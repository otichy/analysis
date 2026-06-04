#!/usr/bin/env Rscript

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }

  source_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(source_file)) {
    return(dirname(normalizePath(source_file, mustWork = TRUE)))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

script_dir <- get_script_dir()
input_pattern <- file.path(script_dir, "Q_ppcme2", "*.txt")
meta_file <- file.path(script_dir, "meta.csv")
q_lemma_file <- file.path(script_dir, "Q lemma.xlsx")
default_output_file <- file.path(script_dir, "all_ME_nouns.csv")

args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_output_file

required_ppcme_columns <- c(
  "1_id",
  "1_span",
  "2_span",
  "4_span"
)

read_export <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- iconv(lines, from = "", to = "UTF-8", sub = "")
  if (length(lines) == 0) {
    return(data.frame())
  }

  header <- strsplit(lines[[1]], "\t", fixed = TRUE)[[1]]
  if (length(lines) == 1) {
    empty <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(header)), stringsAsFactors = FALSE)
    names(empty) <- header
    return(empty)
  }

  rows <- lapply(lines[-1], function(line) strsplit(line, "\t", fixed = TRUE)[[1]])
  max_fields <- max(c(length(header), lengths(rows)))

  if (length(header) < max_fields) {
    header <- c(header, paste0("extra_", seq_len(max_fields - length(header))))
  }

  rows <- lapply(rows, function(fields) {
    length(fields) <- max_fields
    fields[is.na(fields)] <- ""
    fields
  })

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- header[seq_len(ncol(df))]
  df
}

read_meta <- function(path) {
  read.csv(
    path,
    sep = ";",
    header = TRUE,
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  missing <- is.na(x)
  x[missing] <- ""
  x <- trimws(x)
  x[x %in% c("NULL", "'NULL'")] <- ""
  x[missing] <- NA_character_
  x
}

normalize_ppcme_text <- function(x) {
  x <- clean_text(x)
  x <- gsub("+a", "æ", x, fixed = TRUE)
  x <- gsub("+t", "þ", x, fixed = TRUE)
  x <- gsub("+g", "ȝ", x, fixed = TRUE)
  x
}

read_q_lemma_map <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required q lemma file not found: %s", path))
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

  required_columns <- c("word", "lemma")
  missing_columns <- setdiff(required_columns, names(mapping))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Missing required column(s) in %s: %s",
        normalizePath(path, winslash = "/", mustWork = FALSE),
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  mapping <- data.frame(
    word = tolower(clean_text(mapping[["word"]])),
    lemma = clean_text(mapping[["lemma"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  mapping$word[is.na(mapping$word)] <- ""
  mapping$lemma[is.na(mapping$lemma)] <- ""

  empty_rows <- !nzchar(mapping$word) & !nzchar(mapping$lemma)
  mapping <- mapping[!empty_rows, , drop = FALSE]

  incomplete_rows <- !nzchar(mapping$word) | !nzchar(mapping$lemma)
  if (any(incomplete_rows)) {
    bad_rows <- which(incomplete_rows)
    stop(
      sprintf(
        "Blank word/lemma value(s) found in %s at row(s): %s",
        normalizePath(path, winslash = "/", mustWork = FALSE),
        paste(head(bad_rows + 1L, 10), collapse = ", ")
      )
    )
  }

  grouped <- split(mapping$lemma, mapping$word)
  conflicting_words <- names(grouped)[vapply(grouped, function(values) {
    length(unique(values)) > 1
  }, logical(1))]
  if (length(conflicting_words) > 0) {
    details <- vapply(head(conflicting_words, 10), function(word) {
      sprintf("%s -> %s", word, paste(unique(grouped[[word]]), collapse = " / "))
    }, character(1))
    stop(
      sprintf(
        "Conflicting duplicate word mapping(s) found in %s: %s",
        normalizePath(path, winslash = "/", mustWork = FALSE),
        paste(details, collapse = "; ")
      )
    )
  }

  mapping <- mapping[!duplicated(mapping$word), , drop = FALSE]
  stats::setNames(mapping$lemma, mapping$word)
}

apply_q_lemma_map <- function(df, q_lemma_map, path) {
  if (!all(c("q_form", "q_lemma") %in% names(df))) {
    stop("Expected q_form and q_lemma columns before remapping quantifier lemmas.")
  }

  keys <- normalize_ppcme_text(df$q_form)
  keys[is.na(keys)] <- ""
  keys <- tolower(keys)

  mapped <- unname(q_lemma_map[keys])
  has_key <- nzchar(keys)
  matched <- has_key & !is.na(mapped)

  df$q_lemma[matched] <- mapped[matched]
  df$q_lemma[has_key & !matched] <- df$q_form[has_key & !matched]
  df$q_lemma[!has_key] <- ""
  attr(df, "remapped_rows") <- sum(matched)
  attr(df, "unmapped_rows") <- sum(has_key & !matched)
  attr(df, "q_lemma_map_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
  df
}

get_column <- function(df, candidates, required = FALSE, path = NULL) {
  for (name in candidates) {
    if (name %in% names(df)) {
      return(clean_text(df[[name]]))
    }
  }

  if (required) {
    stop(
      sprintf(
        "Missing required column(s) in %s: %s",
        normalizePath(path, winslash = "/", mustWork = FALSE),
        paste(candidates, collapse = " or ")
      )
    )
  }

  rep("", nrow(df))
}

infer_lemma_from_path <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

build_ppcme_subset <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0 || !nzchar(trimws(first_line))) {
    return(NULL)
  }

  df <- read_export(path)
  if (nrow(df) == 0) {
    return(NULL)
  }

  missing_columns <- setdiff(required_ppcme_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Missing required column(s) in %s: %s",
        normalizePath(path, winslash = "/", mustWork = FALSE),
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  q_token <- normalize_ppcme_text(get_column(df, c("1_span"), required = TRUE, path = path))
  q_form <- tolower(q_token)
  noun_pos <- get_column(
    df,
    c("4_anno_ptb::pos", "4_anno_ptb:pos"),
    required = TRUE,
    path = path
  )
  is_plural <- nzchar(noun_pos) & grepl("NS", noun_pos, fixed = TRUE)
  lemma <- rep(infer_lemma_from_path(path), nrow(df))

  subset <- data.frame(
    id = get_column(df, c("1_id"), required = TRUE, path = path),
    q_token = q_token,
    q_form = q_form,
    q_lemma = q_form,
    context = normalize_ppcme_text(get_column(df, c("2_span"), required = TRUE, path = path)),
    token = normalize_ppcme_text(get_column(df, c("4_span"), required = TRUE, path = path)),
    lemma = lemma,
    plural = ifelse(is_plural, "1", ""),
    singular = ifelse(is_plural, "", "1"),
    author = get_column(df, c("meta_Author", "1_meta_Author")),
    period = get_column(df, c("meta_Period", "1_meta_Period")),
    dialect = get_column(df, c("meta_Dialect", "1_meta_Dialect")),
    filename = get_column(df, c("meta_File_name", "1_meta_File_name")),
    genre = get_column(df, c("meta_Genre", "1_meta_Genre")),
    verse_or_prose = get_column(df, c("meta_Verse_or_prose", "1_meta_Verse_or_prose")),
    foreign_original = get_column(df, c("meta_Foreign_original", "1_meta_Foreign_original")),
    relationship_to_spoken_language = get_column(
      df,
      c("meta_Relationship_to_spoken_language", "1_meta_Relationship_to_spoken_language")
    ),
    prototypicaltext_category = get_column(
      df,
      c("meta_Prototypicaltext_category", "1_meta_Prototypicaltext_category")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  valid_rows <- !is.na(subset$id) & nzchar(subset$id) &
    !is.na(subset$context) & nzchar(subset$context) &
    !is.na(subset$token) & nzchar(subset$token)
  dropped_rows <- sum(!valid_rows, na.rm = TRUE)
  subset <- subset[valid_rows, , drop = FALSE]

  if (nrow(subset) == 0) {
    return(NULL)
  }

  attr(subset, "dropped_rows") <- dropped_rows
  subset
}

files <- sort(Sys.glob(input_pattern))

if (length(files) == 0) {
  stop(sprintf("No files found matching '%s' in %s", input_pattern, normalizePath(getwd())))
}

if (!file.exists(meta_file)) {
  stop(sprintf("Required metadata file not found: %s", meta_file))
}

q_lemma_map <- read_q_lemma_map(q_lemma_file)

message(sprintf("Found %d PPCME2 text file(s).", length(files)))

datasets <- lapply(files, build_ppcme_subset)
dropped_rows <- sum(vapply(datasets, function(df) {
  if (is.null(df)) {
    return(0L)
  }

  value <- attr(df, "dropped_rows", exact = TRUE)
  if (is.null(value)) {
    0L
  } else {
    as.integer(value)
  }
}, integer(1)))
processed_files <- sum(vapply(datasets, Negate(is.null), logical(1)))
skipped_files <- length(files) - processed_files

datasets <- Filter(Negate(is.null), datasets)
if (length(datasets) == 0) {
  stop("No non-empty PPCME2 text files were available to process.")
}

merged <- do.call(rbind, datasets)
merged <- apply_q_lemma_map(merged, q_lemma_map, q_lemma_file)
remapped_rows <- attr(merged, "remapped_rows", exact = TRUE)
unmapped_rows <- attr(merged, "unmapped_rows", exact = TRUE)
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
  na = "",
  fileEncoding = "UTF-8"
)

message(sprintf("Processed %d non-empty file(s); skipped %d blank/header-only file(s).", processed_files, skipped_files))
message(sprintf("Dropped %d malformed row(s) with missing core fields.", dropped_rows))
message(sprintf("Remapped q_lemma for %d row(s) using %s", remapped_rows, normalizePath(q_lemma_file, winslash = "/", mustWork = FALSE)))
message(sprintf("Kept original q_form as q_lemma for %d row(s) without a workbook match.", unmapped_rows))
message(sprintf("Merged %d row(s) into %s", nrow(merged), normalizePath(output_file, winslash = "/", mustWork = FALSE)))
message(sprintf("Joined metadata for %d row(s) from %s", nrow(merged), normalizePath(meta_file, winslash = "/", mustWork = FALSE)))

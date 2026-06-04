#!/usr/bin/env Rscript

output_fields <- c(
  "id",
  "q_token",
  "q_lemma",
  "lemma",
  "text",
  "genre",
  "dialect",
  "plural",
  "singular",
  "count",
  "semantics",
  "orig",
  "oe reflex",
  "oe class",
  "oe stem",
  "oe gender",
  "period_simple",
  "period_sorted",
  "period_main"
)

period_fields <- c("period_simple", "period_sorted", "period_main")

# Collapse corpus-specific detailed labels to the simplified period view used
# in downstream analysis.
period_simple_map <- c(
  "?" = "?",
  "o1" = "o1",
  "o12" = "o1",
  "o2" = "o2",
  "o23" = "o2",
  "o24" = "o2",
  "ox2" = "o2",
  "o3" = "o3",
  "o34" = "o3",
  "o4" = "o4",
  "ox4" = "o4",
  "mx/1" = "m1",
  "m1" = "m1",
  "m2" = "m2",
  "m2/3" = "m2",
  "m2/4" = "m2",
  "m3" = "m3",
  "m3/4" = "m3",
  "m34" = "m3",
  "m4" = "m4",
  "mx4" = "m4",
  "l1" = "l1",
  "l2" = "l2",
  "l3" = "l3",
  "l4" = "l4"
)

period_sorted_map <- c(
  "?" = "a_o?",
  "o1" = "b_o1",
  "o2" = "c_o2",
  "o3" = "d_o3",
  "o4" = "e_o4",
  "m1" = "e_m1",
  "m2" = "f_m2",
  "m3" = "g_m3",
  "m4" = "h_m4",
  "l1" = "i_l1",
  "l2" = "j_l2",
  "l3" = "k_l3",
  "l4" = "l_l4"
)

period_main_map <- c(
  "?" = "a_oe",
  "o1" = "a_oe",
  "o12" = "a_oe",
  "o2" = "a_oe",
  "o23" = "a_oe",
  "o24" = "a_oe",
  "ox2" = "a_oe",
  "o3" = "a_oe",
  "o34" = "a_oe",
  "o4" = "a_oe",
  "ox4" = "a_oe",
  "mx/1" = "b_me",
  "m1" = "b_me",
  "m2" = "b_me",
  "m2/3" = "b_me",
  "m2/4" = "b_me",
  "m3" = "b_me",
  "m3/4" = "b_me",
  "m34" = "b_me",
  "m4" = "b_me",
  "mx4" = "b_me",
  "l1" = "c_lmod",
  "l2" = "c_lmod",
  "l3" = "c_lmod",
  "l4" = "c_lmod"
)

period_alias_map <- c(
  "?" = "?",
  "o1" = "o1",
  "o12" = "o12",
  "o2" = "o2",
  "o23" = "o23",
  "o24" = "o24",
  "ox2" = "ox2",
  "o3" = "o3",
  "o34" = "o34",
  "o4" = "o4",
  "ox4" = "ox4",
  "mx1" = "mx/1",
  "mx/1" = "mx/1",
  "m1" = "m1",
  "m2" = "m2",
  "m23" = "m2/3",
  "m2/3" = "m2/3",
  "m24" = "m2/4",
  "m2/4" = "m2/4",
  "m3" = "m3",
  "m3/4" = "m3/4",
  "m34" = "m34",
  "m4" = "m4",
  "mx4" = "mx4",
  "l1" = "l1",
  "l2" = "l2",
  "l3" = "l3",
  "l4" = "l4"
)

clean_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  missing <- is.na(x)
  x[missing] <- ""
  x <- trimws(x)
  x[x %in% c("NULL", "'NULL'")] <- ""
  x[missing] <- ""
  x
}

normalize_path_label <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

read_semicolon_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Input file not found: %s", normalize_path_label(path)))
  }

  raw_lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(raw_lines) == 0) {
    stop(sprintf("Input file is empty: %s", normalize_path_label(path)))
  }

  raw_lines[[1]] <- sub("^\ufeff", "", raw_lines[[1]])
  fixed_text <- paste(raw_lines, collapse = "\n")

  # Some rows contain backslash-escaped quotes (\") inside quoted fields.
  # Standard CSV readers treat those quotes as field terminators, so normalize
  # them to doubled quotes before parsing.
  fixed_text <- gsub("\\\\\"", "\"\"", fixed_text, perl = TRUE)

  data <- read.csv(
    text = fixed_text,
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(0),
    comment.char = ""
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

parse_nonnegative_int <- function(values, field_name, row_labels) {
  values <- clean_text(values)
  out <- integer(length(values))

  empty <- values == ""
  if (all(empty)) {
    return(out)
  }

  non_empty_values <- values[!empty]
  is_integer <- grepl("^[0-9]+$", non_empty_values)
  if (!all(is_integer)) {
    bad_value <- non_empty_values[which(!is_integer)[1]]
    bad_index <- which(values == bad_value & !empty)[1]
    row_label <- row_labels[[bad_index]]
    stop(sprintf("%s: invalid integer in '%s': %s", row_label, field_name, bad_value))
  }

  out[!empty] <- as.integer(non_empty_values)
  out
}

normalize_period <- function(values, row_labels) {
  values <- clean_text(values)
  values <- tolower(gsub("\\s+", "", values))
  values <- unname(period_alias_map[values])

  invalid <- is.na(values) | !(values %in% names(period_simple_map))
  if (any(invalid)) {
    bad_index <- which(invalid)[1]
    stop(
      sprintf(
        "%s: unexpected period value: %s",
        row_labels[[bad_index]],
        values[[bad_index]]
      )
    )
  }

  values
}

collapse_period_simple <- function(period_detail) {
  simple <- unname(period_simple_map[period_detail])

  invalid <- is.na(simple)
  if (any(invalid)) {
    stop(sprintf("Unexpected detailed period value while collapsing period_simple: %s", period_detail[[which(invalid)[1]]]))
  }

  simple
}

derive_period_columns <- function(period, ids, fallback_prefix) {
  row_labels <- build_row_labels(ids, fallback_prefix)
  period_detail <- normalize_period(period, row_labels)
  period_simple <- collapse_period_simple(period_detail)

  data.frame(
    period_simple = period_simple,
    period_sorted = unname(period_sorted_map[period_simple]),
    period_main = unname(period_main_map[period_detail]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

build_row_labels <- function(ids, fallback_prefix) {
  ids <- clean_text(ids)
  ifelse(ids == "", sprintf("<%s row>", fallback_prefix), ids)
}

append_missing_period_columns <- function(df, path, fallback_prefix) {
  missing_period_fields <- setdiff(period_fields, names(df))

  if (length(missing_period_fields) == 0) {
    return(list(data = df, changed = FALSE, added = character(0)))
  }

  require_columns(df, c("id", "period"), basename(path))
  derived_periods <- derive_period_columns(df$period, df$id, fallback_prefix)

  for (field in missing_period_fields) {
    df[[field]] <- derived_periods[[field]]
  }

  write_output(df, path)

  list(data = df, changed = TRUE, added = missing_period_fields)
}

normalize_oe_rows <- function(df) {
  source_name <- "all_OE_nouns.csv"
  require_columns(
    df,
    c(
      "id",
      "q_token",
      "q_lemma",
      "lemma",
      "filename",
      "genre",
      "dialect",
      "pde_count",
      "semantics",
      "orig",
      "oe_etymon",
      "oe_class",
      "oe_stem",
      "oe_gender",
      "period"
    ),
    source_name
  )

  periods <- derive_period_columns(df$period, df$id, "OE")

  data.frame(
    id = clean_text(df$id),
    q_token = clean_text(df$q_token),
    q_lemma = clean_text(df$q_lemma),
    lemma = clean_text(df$lemma),
    text = clean_text(df$filename),
    genre = clean_text(df$genre),
    dialect = clean_text(df$dialect),
    plural = rep(NA_integer_, nrow(df)),
    singular = rep(NA_integer_, nrow(df)),
    count = clean_text(df$pde_count),
    semantics = clean_text(df$semantics),
    orig = clean_text(df$orig),
    "oe reflex" = clean_text(df$oe_etymon),
    "oe class" = clean_text(df$oe_class),
    "oe stem" = clean_text(df$oe_stem),
    "oe gender" = clean_text(df$oe_gender),
    period_simple = periods$period_simple,
    period_sorted = periods$period_sorted,
    period_main = periods$period_main,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

normalize_me_rows <- function(df) {
  source_name <- "all_ME_nouns.csv"
  require_columns(
    df,
    c(
      "id",
      "q_token",
      "q_lemma",
      "lemma",
      "filename",
      "genre",
      "dialect",
      "plural",
      "singular",
      "pde_count",
      "semantics",
      "orig",
      "oe_etymon",
      "oe_class",
      "oe_stem",
      "oe_gender",
      "period"
    ),
    source_name
  )

  row_labels <- build_row_labels(df$id, "ME")
  periods <- derive_period_columns(df$period, df$id, "ME")

  data.frame(
    id = clean_text(df$id),
    q_token = clean_text(df$q_token),
    q_lemma = clean_text(df$q_lemma),
    lemma = clean_text(df$lemma),
    text = clean_text(df$filename),
    genre = clean_text(df$genre),
    dialect = clean_text(df$dialect),
    plural = parse_nonnegative_int(df$plural, "plural", row_labels),
    singular = parse_nonnegative_int(df$singular, "singular", row_labels),
    count = clean_text(df$pde_count),
    semantics = clean_text(df$semantics),
    orig = clean_text(df$orig),
    "oe reflex" = clean_text(df$oe_etymon),
    "oe class" = clean_text(df$oe_class),
    "oe stem" = clean_text(df$oe_stem),
    "oe gender" = clean_text(df$oe_gender),
    period_simple = periods$period_simple,
    period_sorted = periods$period_sorted,
    period_main = periods$period_main,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

normalize_lmode_rows <- function(df) {
  source_name <- "all_LModE_nouns.csv"
  require_columns(
    df,
    c(
      "id",
      "q_token",
      "q_lemma",
      "lemma",
      "filename",
      "genre",
      "plural",
      "singular",
      "pde_count",
      "semantics",
      "orig",
      "oe_etymon",
      "oe_class",
      "oe_stem",
      "oe_gender",
      "period"
    ),
    source_name
  )

  row_labels <- build_row_labels(df$id, "LModE")
  periods <- derive_period_columns(df$period, df$id, "LModE")

  data.frame(
    id = clean_text(df$id),
    q_token = clean_text(df$q_token),
    q_lemma = clean_text(df$q_lemma),
    lemma = clean_text(df$lemma),
    text = clean_text(df$filename),
    genre = clean_text(df$genre),
    dialect = "",
    plural = parse_nonnegative_int(df$plural, "plural", row_labels),
    singular = parse_nonnegative_int(df$singular, "singular", row_labels),
    count = clean_text(df$pde_count),
    semantics = clean_text(df$semantics),
    orig = clean_text(df$orig),
    "oe reflex" = clean_text(df$oe_etymon),
    "oe class" = clean_text(df$oe_class),
    "oe stem" = clean_text(df$oe_stem),
    "oe gender" = clean_text(df$oe_gender),
    period_simple = periods$period_simple,
    period_sorted = periods$period_sorted,
    period_main = periods$period_main,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

parse_args <- function(args) {
  config <- list(
    oe = "all_OE_nouns.csv",
    me = "all_ME_nouns.csv",
    lmode = "all_LModE_nouns.csv",
    out = "all_nouns_combined.csv"
  )

  positional <- character(0)

  for (arg in args) {
    if (arg %in% c("-h", "--help")) {
      cat(
        paste(
          "Usage:",
          "  prepare_noun_datasets.R [--oe=PATH] [--me=PATH] [--lmode=PATH] [--out=PATH]",
          "  prepare_noun_datasets.R [OUTPUT_PATH]",
          sep = "\n"
        )
      )
      cat("\n")
      quit(save = "no", status = 0)
    } else if (startsWith(arg, "--oe=")) {
      config$oe <- sub("^--oe=", "", arg)
    } else if (startsWith(arg, "--me=")) {
      config$me <- sub("^--me=", "", arg)
    } else if (startsWith(arg, "--lmode=")) {
      config$lmode <- sub("^--lmode=", "", arg)
    } else if (startsWith(arg, "--out=")) {
      config$out <- sub("^--out=", "", arg)
    } else if (startsWith(arg, "--merged-out=")) {
      config$out <- sub("^--merged-out=", "", arg)
    } else if (startsWith(arg, "--")) {
      stop(sprintf("Unknown option: %s", arg))
    } else {
      positional <- c(positional, arg)
    }
  }

  if (length(positional) > 1) {
    stop("Expected at most one positional argument (the output path).")
  }

  if (length(positional) == 1) {
    config$out <- positional[[1]]
  }

  config
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

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  oe_source <- read_semicolon_csv(args$oe)
  me_source <- read_semicolon_csv(args$me)
  lmode_source <- read_semicolon_csv(args$lmode)

  oe_source_update <- append_missing_period_columns(oe_source, args$oe, "OE")
  me_source_update <- append_missing_period_columns(me_source, args$me, "ME")
  lmode_source_update <- append_missing_period_columns(lmode_source, args$lmode, "LModE")

  oe <- normalize_oe_rows(oe_source_update$data)
  me <- normalize_me_rows(me_source_update$data)
  lmode <- normalize_lmode_rows(lmode_source_update$data)
  combined <- rbind(oe[, output_fields], me[, output_fields], lmode[, output_fields])

  expected_rows <- nrow(oe) + nrow(me) + nrow(lmode)
  if (nrow(combined) != expected_rows) {
    stop(sprintf("Row count changed while combining noun datasets: expected %d row(s), got %d.", expected_rows, nrow(combined)))
  }

  write_output(combined, args$out)

  if (oe_source_update$changed) {
    message(sprintf(
      "Added OE period column(s) to %s: %s",
      normalize_path_label(args$oe),
      paste(oe_source_update$added, collapse = ", ")
    ))
  }
  if (me_source_update$changed) {
    message(sprintf(
      "Added ME period column(s) to %s: %s",
      normalize_path_label(args$me),
      paste(me_source_update$added, collapse = ", ")
    ))
  }
  if (lmode_source_update$changed) {
    message(sprintf(
      "Added LModE period column(s) to %s: %s",
      normalize_path_label(args$lmode),
      paste(lmode_source_update$added, collapse = ", ")
    ))
  }
  message(sprintf("OE rows copied: %d", nrow(oe)))
  message(sprintf("ME rows copied: %d", nrow(me)))
  message(sprintf("LModE rows copied: %d", nrow(lmode)))
  message(sprintf("Total merged rows written: %d", nrow(combined)))
  message(sprintf("Wrote merged file: %s", normalize_path_label(args$out)))
}

main()

detect_combined_sep <- function(path) {
  first_line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  semicolons <- lengths(regmatches(first_line, gregexpr(";", first_line, fixed = TRUE)))
  commas <- lengths(regmatches(first_line, gregexpr(",", first_line, fixed = TRUE)))
  if (semicolons >= commas) ";" else ","
}

read_combined_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Input file not found: %s", path))
  }

  sep <- detect_combined_sep(path)
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::fread(
      path,
      sep = sep,
      data.table = FALSE,
      encoding = "UTF-8",
      na.strings = c("", "NA"),
      showProgress = FALSE
    ))
  }

  utils::read.csv(
    path,
    sep = sep,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM",
    na.strings = c("", "NA"),
    comment.char = ""
  )
}

clean_combined_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

canonical_period_simple <- function(x) {
  x <- tolower(gsub("\\s+", "", clean_combined_text(x)))
  aliases <- c(
    "?" = "?",
    "o1" = "o1", "o12" = "o1",
    "o2" = "o2", "o23" = "o2", "o24" = "o2", "ox2" = "o2",
    "o3" = "o3", "o34" = "o3",
    "o4" = "o4", "ox4" = "o4",
    "mx/1" = "m1", "mx1" = "m1", "m1" = "m1",
    "m2" = "m2", "m2/3" = "m2", "m23" = "m2", "m2/4" = "m2", "m24" = "m2",
    "m3" = "m3", "m3/4" = "m3", "m34" = "m3",
    "m4" = "m4", "mx4" = "m4",
    "l1" = "l1", "l2" = "l2", "l3" = "l3", "l4" = "l4"
  )
  unname(aliases[x])
}

canonical_period_main <- function(period_simple) {
  out <- rep(NA_character_, length(period_simple))
  out[period_simple %in% c("?", "o1", "o2", "o3", "o4")] <- "a_oe"
  out[period_simple %in% c("m1", "m2", "m3", "m4")] <- "b_me"
  out[period_simple %in% c("l1", "l2", "l3", "l4")] <- "c_lmod"
  out
}

canonical_period_sorted <- function(period_simple) {
  labels <- c(
    "?" = "a_o?", "o1" = "b_o1", "o2" = "c_o2", "o3" = "d_o3", "o4" = "e_o4",
    "m1" = "f_m1", "m2" = "g_m2", "m3" = "h_m3", "m4" = "i_m4",
    "l1" = "j_l1", "l2" = "k_l2", "l3" = "l_l3", "l4" = "m_l4"
  )
  unname(labels[period_simple])
}

combined_period_levels <- function(granularity) {
  switch(
    granularity,
    overall = "all_periods",
    period_main = c("a_oe", "b_me", "c_lmod"),
    period_sorted = c(
      "a_o?", "b_o1", "c_o2", "d_o3", "e_o4",
      "f_m1", "g_m2", "h_m3", "i_m4",
      "j_l1", "k_l2", "l_l3", "m_l4"
    ),
    stop(sprintf("Unsupported period granularity: %s", granularity))
  )
}

standardize_combined_dataset <- function(df, dataset_name) {
  required <- c("semantics", "period_simple")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop(sprintf("%s is missing required columns: %s", dataset_name, paste(missing, collapse = ", ")))
  }

  if (identical(dataset_name, "determiners")) {
    if (!"noun_lemma" %in% names(df)) {
      stop("determiners is missing required column: noun_lemma")
    }
    df$lemma <- df$noun_lemma
  } else if (!"lemma" %in% names(df)) {
    stop(sprintf("%s is missing required column: lemma", dataset_name))
  }

  dataset_required <- switch(
    dataset_name,
    quantifiers = c("q_lemma", "plural"),
    number = "plural",
    determiners = c("det_lemma", "noun_pos"),
    character()
  )
  dataset_missing <- setdiff(dataset_required, names(df))
  if (length(dataset_missing) > 0L) {
    stop(sprintf("%s is missing required columns: %s", dataset_name, paste(dataset_missing, collapse = ", ")))
  }

  df$lemma <- clean_combined_text(df$lemma)
  df$semantics <- clean_combined_text(df$semantics)
  df$period_simple_canonical <- canonical_period_simple(df$period_simple)
  df$period_main_canonical <- canonical_period_main(df$period_simple_canonical)
  df$period_sorted_canonical <- canonical_period_sorted(df$period_simple_canonical)

  df <- df[!is.na(df$lemma) & !is.na(df$semantics), , drop = FALSE]
  if (any(is.na(df$period_simple_canonical))) {
    bad <- unique(clean_combined_text(df$period_simple[is.na(df$period_simple_canonical)]))
    stop(sprintf("%s contains unsupported period_simple values: %s", dataset_name, paste(bad, collapse = ", ")))
  }
  df
}

add_analysis_period <- function(df, granularity) {
  df$analysis_period <- switch(
    granularity,
    overall = "all_periods",
    period_main = df$period_main_canonical,
    period_sorted = df$period_sorted_canonical,
    stop(sprintf("Unsupported period granularity: %s", granularity))
  )
  df
}

aggregate_binary_feature <- function(df, success, prefix) {
  if (nrow(df) == 0L) {
    out <- data.frame(
      lemma = character(),
      analysis_period = character(),
      success_n = integer(),
      total = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    success <- as.integer(success)
    valid <- !is.na(success)
    df <- df[valid, c("lemma", "analysis_period"), drop = FALSE]
    success <- success[valid]
    if (nrow(df) == 0L) {
      return(aggregate_binary_feature(df, logical(), prefix))
    }
    out <- stats::aggregate(
      list(success_n = success, total = rep.int(1L, length(success))),
      by = list(lemma = df$lemma, analysis_period = df$analysis_period),
      FUN = sum
    )
  }

  names(out)[names(out) == "success_n"] <- paste0(prefix, "_n")
  names(out)[names(out) == "total"] <- paste0(prefix, "_total")
  out
}

merge_feature_tables <- function(tables) {
  Reduce(
    function(x, y) merge(x, y, by = c("lemma", "analysis_period"), all = TRUE, sort = FALSE),
    tables
  )
}

validate_semantics <- function(datasets) {
  semantics <- unique(do.call(
    rbind,
    lapply(datasets, function(df) df[c("lemma", "semantics")])
  ))
  counts <- stats::aggregate(semantics ~ lemma, semantics, function(x) length(unique(x)))
  conflicts <- counts$lemma[counts$semantics > 1L]
  if (length(conflicts) > 0L) {
    stop(sprintf("Conflicting semantics labels for lemma(s): %s", paste(conflicts, collapse = ", ")))
  }
  semantics[!duplicated(semantics$lemma), , drop = FALSE]
}

add_proportion_columns <- function(df, prefix) {
  n_col <- paste0(prefix, "_n")
  total_col <- paste0(prefix, "_total")
  raw_col <- paste0(prefix, "_prop")
  smooth_col <- paste0(prefix, "_prop_smoothed")
  logit_col <- paste0(prefix, "_logit_smoothed")

  df[[raw_col]] <- df[[n_col]] / df[[total_col]]
  df[[smooth_col]] <- (df[[n_col]] + 0.5) / (df[[total_col]] + 1)
  df[[logit_col]] <- stats::qlogis(df[[smooth_col]])
  df
}

prepare_combined_aggregate <- function(
  quantifiers,
  number,
  determiners,
  granularity = c("period_main", "overall", "period_sorted")
) {
  granularity <- match.arg(granularity)

  q <- add_analysis_period(standardize_combined_dataset(quantifiers, "quantifiers"), granularity)
  n <- add_analysis_period(standardize_combined_dataset(number, "number"), granularity)
  d <- add_analysis_period(standardize_combined_dataset(determiners, "determiners"), granularity)
  semantics <- validate_semantics(list(q, n, d))

  q_choice <- q[clean_combined_text(q$q_lemma) %in% c("many", "much"), , drop = FALSE]
  q_many <- aggregate_binary_feature(q_choice, clean_combined_text(q_choice$q_lemma) == "many", "q_many")

  number_plural <- aggregate_binary_feature(n, as.character(n$plural) == "1", "number_plural")

  # The a/the comparison is intentionally restricted to morphologically singular
  # common nouns, exactly as requested. Other noun_pos levels are excluded.
  d_choice <- d[
    clean_combined_text(d$det_lemma) %in% c("a", "the") &
      clean_combined_text(d$noun_pos) == "N",
    ,
    drop = FALSE
  ]
  det_a <- aggregate_binary_feature(d_choice, clean_combined_text(d_choice$det_lemma) == "a", "det_a")

  q_plural <- aggregate_binary_feature(q, as.character(q$plural) == "1", "q_plural")
  d_with_number <- d[clean_combined_text(d$noun_pos) %in% c("N", "N$", "NS", "NS$"), , drop = FALSE]
  det_plural <- aggregate_binary_feature(
    d_with_number,
    clean_combined_text(d_with_number$noun_pos) %in% c("NS", "NS$"),
    "det_plural"
  )

  out <- merge_feature_tables(list(q_many, number_plural, det_a, q_plural, det_plural))
  out <- merge(out, semantics, by = "lemma", all.x = TRUE, sort = FALSE)

  count_columns <- grep("(_n|_total)$", names(out), value = TRUE)
  out[count_columns] <- lapply(out[count_columns], function(x) {
    x[is.na(x)] <- 0
    as.integer(x)
  })

  for (prefix in c("q_many", "number_plural", "det_a", "q_plural", "det_plural")) {
    out <- add_proportion_columns(out, prefix)
  }

  out$has_q_many <- out$q_many_total > 0L
  out$has_number_plural <- out$number_plural_total > 0L
  out$has_det_a <- out$det_a_total > 0L
  out$complete_primary <- out$has_q_many & out$has_number_plural & out$has_det_a
  out$min_primary_total <- pmin(out$q_many_total, out$number_plural_total, out$det_a_total)
  out$primary_total <- out$q_many_total + out$number_plural_total + out$det_a_total

  period_levels <- combined_period_levels(granularity)
  out$analysis_period <- factor(out$analysis_period, levels = period_levels, ordered = TRUE)
  out$semantics <- factor(out$semantics, levels = c("c", "a", "s"))
  out <- out[order(out$analysis_period, out$lemma), , drop = FALSE]
  rownames(out) <- NULL

  attr(out, "granularity") <- granularity
  out
}

load_combined_aggregates <- function(data_dir = ".") {
  paths <- file.path(
    data_dir,
    c("quantifiers_combined.csv", "number_combined.csv", "determiners_combined.csv")
  )
  names(paths) <- c("quantifiers", "number", "determiners")
  datasets <- lapply(paths, read_combined_csv)

  list(
    overall = prepare_combined_aggregate(datasets$quantifiers, datasets$number, datasets$determiners, "overall"),
    period_main = prepare_combined_aggregate(datasets$quantifiers, datasets$number, datasets$determiners, "period_main"),
    period_sorted = prepare_combined_aggregate(datasets$quantifiers, datasets$number, datasets$determiners, "period_sorted")
  )
}

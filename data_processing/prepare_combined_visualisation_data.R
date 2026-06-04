#!/usr/bin/env Rscript

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

script_dir <- get_script_dir()
repo_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
helper_path <- file.path(repo_dir, "combined_visualisations", "R", "combined_data.R")
source(helper_path, local = TRUE)

aggregates <- load_combined_aggregates(repo_dir)
output_dir <- file.path(repo_dir, "results", "combined_visualisations")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(aggregates, file.path(output_dir, "combined_aggregates.rds"))
for (name in names(aggregates)) {
  utils::write.csv(
    aggregates[[name]],
    file.path(output_dir, paste0("combined_aggregate_", name, ".csv")),
    row.names = FALSE,
    na = ""
  )
}

cat(sprintf("Wrote combined visualisation aggregates to %s\n", output_dir))
for (name in names(aggregates)) {
  x <- aggregates[[name]]
  cat(sprintf(
    "%s: %d cells, %d complete primary cells\n",
    name,
    nrow(x),
    sum(x$complete_primary)
  ))
}

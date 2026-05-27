#' Run an interactive wizard to build a SpotMap
#'
#' Guides users through choosing a data file, selecting columns, and generating
#' the map step by step. Designed for users who prefer a guided experience.
#'
#' @param output_path Where to save the HTML map (default: "spotmap.html").
#' @return Invisible NULL.
#' @export
run_interactive <- function(output_path = "spotmap.html") {
  .line("=")
  cat("   SpotMap - Interactive Map Builder\n")
  cat("   We'll guide you through 5 quick steps to build your map.\n")
  .line("=")
  cat("Tip: press Enter to accept the suggested answer in [brackets].\n\n")

  # Step 1 -- load file
  df <- .step_load_file()

  # Step 2 -- pick columns
  cols <- names(df)
  previews <- .build_previews(df)

  cat("\n"); .line("-")
  cat("Step 2 -- Which column holds the LATITUDE?\n")
  cat("   (Latitude is the North-South number, e.g. 28.61 for Delhi)\n")
  .line("-")
  lat_col <- .ask_choice("Your latitude column", cols,
                          .guess(cols, c("lat", "latitude", "y")), previews)

  cat("\n"); .line("-")
  cat("Step 3 -- Which column holds the LONGITUDE?\n")
  cat("   (Longitude is the East-West number, e.g. 77.20 for Delhi)\n")
  .line("-")
  lon_col <- .ask_choice("Your longitude column", cols,
                          .guess(cols, c("lon", "long", "lng", "longitude", "x")),
                          previews)

  cat("\n"); .line("-")
  cat("Step 4 -- Which column tells us CASE vs CONTROL?\n")
  .line("-")
  outcome_col <- .ask_choice("Your outcome column", cols,
                              .guess(cols, c("outcome", "status", "case_control",
                                             "class", "result")), previews)

  # Step 3 -- pick case value
  case_value <- .step_pick_case_value(df, outcome_col)

  # Step 4 -- build
  cat("\nBuilding the map...\n")
  tryCatch({
    spot_map(df, lat_col = lat_col, lon_col = lon_col,
             outcome_col = outcome_col, case_value = case_value,
             output = output_path)
    .line()
    cat(sprintf("\nOpen this file in your browser:\n   %s\n\n",
                normalizePath(output_path, mustWork = FALSE)))
    .line()
  }, error = function(e) {
    cat(sprintf("Error: %s\n", conditionMessage(e)))
  })
  invisible(NULL)
}


# --- Helpers -----------------------------------------------------------------

.line <- function(char = "=", n = 55L) cat(strrep(char, n), "\n")

.ask <- function(prompt, default = NULL) {
  suffix <- if (!is.null(default)) paste0(" [", default, "]") else ""
  repeat {
    val <- readline(paste0(prompt, suffix, ": "))
    val <- trimws(gsub("['\"]", "", val))
    if (nzchar(val)) return(val)
    if (!is.null(default)) return(default)
    cat("Please type a value (cannot be empty).\n")
  }
}

.ask_choice <- function(prompt, options, default_index = 1L, previews = NULL) {
  col_w <- max(nchar(options)) + 2L
  for (i in seq_along(options)) {
    marker <- if (i == default_index) " (default)" else ""
    if (!is.null(previews)) {
      cat(sprintf("  %d. %-*s ->  %s%s\n", i, col_w, options[i],
                  previews[i], marker))
    } else {
      cat(sprintf("  %d. %s%s\n", i, options[i], marker))
    }
  }
  repeat {
    raw <- .ask(prompt, default = as.character(default_index))
    if (grepl("^\\d+$", raw)) {
      idx <- as.integer(raw)
      if (idx >= 1L && idx <= length(options)) return(options[idx])
      cat(sprintf("Pick a number between 1 and %d.\n", length(options)))
      next
    }
    if (raw %in% options) return(raw)
    matches <- options[tolower(options) == tolower(raw)]
    if (length(matches) > 0) return(matches[1])
    cat(sprintf("'%s' is not in the list.\n", raw))
  }
}

.guess <- function(columns, candidates) {
  for (i in seq_along(columns)) {
    lc <- tolower(columns[i])
    if (any(vapply(candidates, function(n) grepl(n, lc, fixed = TRUE),
                   logical(1))))
      return(i)
  }
  1L
}

.build_previews <- function(df, max_samples = 3L, max_chars = 40L) {
  vapply(names(df), function(col) {
    vals <- unique(trimws(as.character(stats::na.omit(df[[col]]))))
    vals <- head(vals, max_samples)
    joined <- if (length(vals) == 0) "(empty)" else paste(vals, collapse = ", ")
    if (nchar(joined) > max_chars) joined <- paste0(substr(joined, 1, max_chars - 1), "...")
    joined
  }, character(1), USE.NAMES = FALSE)
}

.step_load_file <- function() {
  .line("-")
  cat("Step 1 -- Where is your data file?\n")
  cat("   Supported: CSV (.csv), Excel (.xlsx, .xls), TSV\n")
  .line("-")
  repeat {
    path <- .ask("Path to your file")
    if (!file.exists(path)) {
      cat(sprintf("File not found: %s\n", path))
      next
    }
    tryCatch({
      df <- load_data(path)
      if (nrow(df) == 0) {
        cat("The file is empty. Try a different file.\n")
        next
      }
      cat(sprintf("Loaded %d rows and %d columns\n", nrow(df), ncol(df)))
      cat("\nColumns found:\n")
      for (i in seq_along(names(df))) {
        vals <- as.character(stats::na.omit(df[[names(df)[i]]]))
        preview <- if (length(vals) > 0) trimws(vals[1]) else "(empty)"
        if (nchar(preview) > 30) preview <- paste0(substr(preview, 1, 30), "...")
        cat(sprintf("  %2d. %-25s ->  e.g.  %s\n", i, names(df)[i], preview))
      }
      cat("\n")
      return(df)
    }, error = function(e) {
      cat(sprintf("Could not read file: %s\n", conditionMessage(e)))
    })
  }
}

.step_pick_case_value <- function(df, outcome_col) {
  norm <- trimws(as.character(df[[outcome_col]]))
  counts <- sort(table(norm), decreasing = TRUE)
  values <- names(counts)

  if (length(values) == 0) stop("No values in outcome column.", call. = FALSE)

  if (length(values) == 1) {
    cat(sprintf("Only one value found: %s (%d rows). Treating all as cases.\n",
                values[1], counts[1]))
    return(values[1])
  }

  cat("\n"); .line("-")
  cat(sprintf("Step 5 -- Which value in '%s' means a CASE?\n", outcome_col))
  .line("-")
  cat(sprintf("\nValues in '%s':\n\n", outcome_col))
  max_count <- max(counts)
  for (i in seq_along(values)) {
    bar_len <- min(as.integer(counts[i] / max_count * 12), 12L)
    bar <- strrep("#", bar_len)
    cat(sprintf("  %d. %-15s %s  (%d rows)\n", i, values[i], bar, counts[i]))
  }
  cat("\n")

  default_idx <- 1L
  case_kw <- c("case", "cases", "1", "yes", "true", "positive", "present")
  for (i in seq_along(values)) {
    if (tolower(values[i]) %in% case_kw) {
      default_idx <- i
      break
    }
  }

  cat("Which value should be treated as CASE?\n")
  .ask_choice("Case value", values, default_idx)
}

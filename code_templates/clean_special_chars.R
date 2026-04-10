# ---------------------------------------------------------------
# clean_special_chars.R
#
# Standalone R script that does the same job as the
# "Special Character Replacer" Shiny app: reads a .sas7bdat,
# .rds, or .RData file, replaces or strips non-ASCII characters
# (and optionally ASCII control characters), and writes a
# cleaned dataset plus a change-report CSV.
#
# Usage:
#   source("clean_special_chars.R")
#   res <- clean_dataset_file(
#            in_file       = "sample_specialchars.sas7bdat",
#            out_file      = "sample_clean.sas7bdat",
#            report_file   = "sample_change_report.csv",
#            strip_control = TRUE)
#
# Dependencies: haven, stringi
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)
  library(stringi)
})

# --- Explicit replacement map for common non-ASCII symbols -------
char_map <- c(
  # curly quotes
  "\u2018" = "'",  "\u2019" = "'",  "\u201A" = "'",  "\u201B" = "'",
  "\u201C" = '"',  "\u201D" = '"',  "\u201E" = '"',  "\u201F" = '"',
  # dashes / minus
  "\u2013" = "-",  "\u2014" = "-",  "\u2212" = "-",  "\u2015" = "-",
  # spaces
  "\u00A0" = " ",  "\u2002" = " ",  "\u2003" = " ",  "\u2009" = " ",
  "\u200B" = "",
  # punctuation
  "\u2026" = "...", "\u2022" = "*",
  "\u2039" = "<",  "\u203A" = ">",  "\u00AB" = "<<", "\u00BB" = ">>",
  # symbols
  "\u00AE" = "(R)", "\u00A9" = "(C)", "\u2122" = "(TM)",
  "\u00B0" = " deg", "\u00B1" = "+/-", "\u00B2" = "2", "\u00B3" = "3",
  "\u00BC" = "1/4", "\u00BD" = "1/2", "\u00BE" = "3/4",
  "\u00B5" = "u", "\u00D7" = "x", "\u00F7" = "/",
  # currencies
  "\u20AC" = "EUR", "\u00A3" = "GBP", "\u00A5" = "JPY", "\u00A2" = "c",
  "\u20B9" = "INR"
)

# --- ASCII control-character map (applied only if strip_control) -
# (U+0000 NUL cannot exist in an R character vector, so it's
#  intentionally omitted.)
control_map <- c(
  "\x09" = " ",  # TAB -> space
  "\x0A" = " ",  # LF  -> space
  "\x0B" = "",   # VT
  "\x0C" = "",   # FF
  "\x0D" = "",   # CR
  "\x1A" = "",   # SUB
  "\x7F" = ""    # DEL
)

# --- Single-string cleaner ---------------------------------------
clean_string <- function(x, strip_control = FALSE) {
  if (is.na(x) || !nzchar(x)) return(list(clean = x, changes = NULL))
  orig_chars <- stri_split_boundaries(x, type = "character")[[1]]

  # Default:    keep \x01-\x7F      (all ASCII incl. control chars)
  # strip_ctrl: keep \x20-\x7E only (printable ASCII)
  bad_pat <- if (strip_control) "[^\\x20-\\x7E]" else "[^\\x01-\\x7F]"
  bad_idx <- stri_detect_regex(orig_chars, bad_pat)
  if (!any(bad_idx)) return(list(clean = x, changes = NULL))

  bad_chars <- orig_chars[bad_idx]
  replacements <- vapply(bad_chars, function(ch) {
    if (strip_control && ch %in% names(control_map))
      return(unname(control_map[ch]))
    if (ch %in% names(char_map)) return(unname(char_map[ch]))
    t <- stri_trans_general(ch, "Any-Latin; Latin-ASCII")
    gsub(bad_pat, "", t, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)

  cleaned_chars <- orig_chars
  cleaned_chars[bad_idx] <- replacements
  cleaned <- paste0(cleaned_chars, collapse = "")

  codepoints <- vapply(bad_chars, function(ch) {
    ints <- tryCatch(utf8ToInt(ch), error = function(e) NA_integer_)
    paste(sprintf("U+%04X", ints), collapse = "+")
  }, character(1), USE.NAMES = FALSE)

  list(clean = cleaned,
       changes = data.frame(original    = bad_chars,
                            codepoint   = codepoints,
                            replacement = replacements,
                            stringsAsFactors = FALSE))
}

# --- Whole-dataset cleaner ---------------------------------------
clean_dataset <- function(df, strip_control = FALSE) {
  report <- list()
  char_cols <- names(df)[vapply(df, function(col)
    is.character(col) || is.factor(col), logical(1))]

  for (col in char_cols) {
    was_factor <- is.factor(df[[col]])
    values <- as.character(df[[col]])
    for (i in seq_along(values)) {
      res <- clean_string(values[i], strip_control = strip_control)
      if (!is.null(res$changes)) {
        values[i] <- res$clean
        report[[length(report) + 1L]] <- data.frame(
          variable    = col,
          row         = i,
          original    = paste(res$changes$original,    collapse = " | "),
          codepoint   = paste(res$changes$codepoint,   collapse = " | "),
          replacement = paste(res$changes$replacement, collapse = " | "),
          stringsAsFactors = FALSE)
      }
    }
    if (was_factor) values <- factor(values)
    df[[col]] <- values
  }

  # Also clean column names themselves
  new_names <- names(df)
  for (i in seq_along(new_names)) {
    res <- clean_string(new_names[i], strip_control = strip_control)
    if (!is.null(res$changes)) new_names[i] <- res$clean
  }
  names(df) <- new_names

  report_df <- if (length(report)) do.call(rbind, report)
               else data.frame(variable = character(), row = integer(),
                               original = character(), codepoint = character(),
                               replacement = character())
  list(data = df, report = report_df)
}

# --- I/O helpers --------------------------------------------------
read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "sas7bdat" = haven::read_sas(path),
    "rds"      = readRDS(path),
    "rdata"    = { e <- new.env(); load(path, envir = e)
                   get(ls(e)[1], envir = e) },
    stop("Unsupported input type: ", ext))
}

write_any <- function(df, path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "sas7bdat" = suppressWarnings(haven::write_sas(df, path)),
    "rds"      = saveRDS(df, path),
    "csv"      = write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8"),
    stop("Unsupported output type: ", ext))
}

# --- Top-level convenience function -------------------------------
clean_dataset_file <- function(in_file,
                               out_file      = NULL,
                               report_file   = NULL,
                               strip_control = FALSE) {
  df  <- as.data.frame(read_any(in_file))
  res <- clean_dataset(df, strip_control = strip_control)

  if (!is.null(out_file))
    write_any(res$data, out_file)
  if (!is.null(report_file))
    write.csv(res$report, report_file,
              row.names = FALSE, fileEncoding = "UTF-8")

  message(sprintf("Cleaned %d value(s) across %d variable(s).",
                  nrow(res$report),
                  length(unique(res$report$variable))))
  invisible(res)
}

# ---------------- Example ----------------
# res <- clean_dataset_file(
#          in_file       = "sample_specialchars.sas7bdat",
#          out_file      = "sample_clean.sas7bdat",
#          report_file   = "sample_change_report.csv",
#          strip_control = TRUE)
# head(res$report)

# ---------------------------------------------------------------
# Special Character Replacer
# A Shiny tool to strip/replace non-ASCII characters in SAS or R
# datasets, with a transparent change report and sample data.
# ---------------------------------------------------------------

# Auto-install any missing packages on first run
.required <- c("shiny", "bslib", "bsicons", "DT", "haven", "stringi")
.missing  <- .required[!vapply(.required, requireNamespace,
                               logical(1), quietly = TRUE)]
if (length(.missing)) {
  message("Installing missing packages: ", paste(.missing, collapse = ", "))
  install.packages(.missing, repos = "https://cloud.r-project.org")
}

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(haven)
library(stringi)

options(shiny.maxRequestSize = 200 * 1024^2)  # allow up to 200 MB uploads

# ---- Explicit replacement map (applied before transliteration) ----
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

# ---- Explicit map for ASCII control characters -------------------
# Applied only when the "strip control characters" toggle is on.
# Tabs/LFs/CRs embedded in text cells (common in Excel imports) are
# usually noise; map the friendly ones to a space and drop the rest.
# (NUL / U+0000 cannot exist in an R character vector, so it's not
#  listed here -- haven/readRDS will never hand us one.)
control_map <- c(
  "\x09" = " ",  # TAB  -> space
  "\x0A" = " ",  # LF   -> space
  "\x0B" = "",   # VT
  "\x0C" = "",   # FF
  "\x0D" = "",   # CR   -> removed (Excel line-break in cells)
  "\x1A" = "",   # SUB
  "\x7F" = ""    # DEL
)

# ---- Core cleaner ------------------------------------------------
# Returns list(clean = chr, changes = data.frame(original, replacement))
clean_string <- function(x, strip_control = FALSE) {
  if (is.na(x) || !nzchar(x)) return(list(clean = x, changes = NULL))

  original <- x
  # split into chars up-front so we can report what was removed
  orig_chars <- stri_split_boundaries(original, type = "character")[[1]]

  # "bad" = chars to be replaced/removed
  #   default    -> anything outside \x01-\x7F  (non-ASCII only)
  #   strip-ctrl -> anything outside \x20-\x7E  (also catches CR/LF/TAB/etc.)
  bad_pat  <- if (strip_control) "[^\\x20-\\x7E]" else "[^\\x01-\\x7F]"
  bad_idx  <- stri_detect_regex(orig_chars, bad_pat)
  if (!any(bad_idx)) return(list(clean = original, changes = NULL))

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

  changes <- data.frame(
    original    = bad_chars,
    codepoint   = codepoints,
    replacement = replacements,
    stringsAsFactors = FALSE
  )
  list(clean = cleaned, changes = changes)
}

# ---- Whole-dataset cleaner ---------------------------------------
clean_dataset <- function(df, strip_control = FALSE) {
  report <- list()
  char_cols <- names(df)[vapply(df, function(col) is.character(col) || is.factor(col), logical(1))]

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
          original    = paste(res$changes$original, collapse = " | "),
          codepoint   = paste(res$changes$codepoint, collapse = " | "),
          replacement = paste(res$changes$replacement, collapse = " | "),
          stringsAsFactors = FALSE
        )
      }
    }
    if (was_factor) values <- factor(values)
    df[[col]] <- values
  }

  # Also scan column names themselves
  name_report <- list()
  new_names <- names(df)
  for (i in seq_along(new_names)) {
    res <- clean_string(new_names[i], strip_control = strip_control)
    if (!is.null(res$changes)) {
      name_report[[length(name_report) + 1L]] <- data.frame(
        variable    = new_names[i],
        row         = NA_integer_,
        original    = paste(res$changes$original, collapse = " | "),
        codepoint   = paste(res$changes$codepoint, collapse = " | "),
        replacement = paste(res$changes$replacement, collapse = " | "),
        stringsAsFactors = FALSE
      )
      new_names[i] <- res$clean
    }
  }
  names(df) <- new_names

  report_df <- if (length(report) || length(name_report)) {
    do.call(rbind, c(name_report, report))
  } else {
    data.frame(variable = character(), row = integer(),
               original = character(), codepoint = character(),
               replacement = character(), stringsAsFactors = FALSE)
  }

  list(data = df, report = report_df)
}

# ---- Load the bundled code templates at startup ------------------
# These are standalone scripts that do the same cleaning as the
# app. Users can view them in the "View Code" tab and download
# them to drop straight into their own R / SAS pipelines.
code_r_path   <- file.path("code_templates", "clean_special_chars.R")
code_sas_path <- file.path("code_templates", "clean_special_chars.sas")
read_code <- function(p, fallback) {
  if (file.exists(p)) paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
                            collapse = "\n")
  else fallback
}
code_r_text   <- read_code(code_r_path,
                           "# clean_special_chars.R not found in deployment")
code_sas_text <- read_code(code_sas_path,
                           "* clean_special_chars.sas not found in deployment ;")

# ---- Sample dataset with intentional special characters ----------
# Includes both non-ASCII chars (accents, smart quotes, em-dashes,
# currency symbols, etc.) AND a handful of ASCII control characters
# (CR, LF, TAB, trailing whitespace) that typically sneak in from
# Excel imports, so both cleaning modes can be demonstrated.
make_sample_data <- function() {
  data.frame(
    subjid  = sprintf("S-%03d", 1:8),
    name    = c("Jos\u00e9 Garc\u00eda",
                "Fran\u00e7ois Dup\u00f4nt",
                "M\u00fcller Wei\u00df",
                "\u00c5sa \u00d8stergaard\r",                # trailing CR
                "Na\u00efve R\u00e9sum\u00e9",
                "Zo\u00eb Q\u2019Brien",
                "Lena\tSm\u00edth",                          # embedded TAB
                "Pedro Ni\u00f1o"),
    comment = c("Subject said \u201cfeeling fine\u201d today.\r\n(confirmed by RA)",  # CRLF from Alt+Enter
                "Dose reduced \u2014 mild nausea\u2026",
                "Weight \u2248 70 kg;\tBP normal.\tTemp OK.", # tabs between fields
                "Temperature 37\u00b0C \u00b1 0.2.",
                "Caf\u00e9 intake high \u2022 noted.\r",      # trailing CR
                "Price: \u20ac25 \u2013 includes VAT.",
                "Trademark\u2122 product used.",
                "Notes: \u00bd tablet, not \u00bc."),
    site    = c("Paris", "Z\u00fcrich", "M\u00e1laga", "K\u00f8benhavn",
                "M\u00fcnchen", "Bogot\u00e1", "S\u00e3o Paulo", "Montr\u00e9al"),
    dose_mg = c(10, 20, 15, 25, 10, 5, 30, 20),
    stringsAsFactors = FALSE
  )
}

# ==================================================================
# UI
# ==================================================================
app_theme <- bs_theme(
  version   = 5,
  bootswatch = "flatly",
  primary   = "#2C3E50",
  secondary = "#18BC9C",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "navbar-bg" = "#2C3E50"
)

ui <- page_navbar(
  title = tagList(bs_icon("magic"), "Special Character Replacer"),
  theme = app_theme,
  fillable = TRUE,
  header = tags$head(
    tags$style(HTML("
      .hero { padding: 1rem 1.25rem; border-radius: .75rem;
              background: linear-gradient(135deg, #2C3E50 0%, #18BC9C 100%);
              color: white; margin-bottom: 1rem; }
      .hero h4 { margin: 0; font-weight: 600; }
      .hero p  { margin: .25rem 0 0 0; opacity: .9; font-size: .9rem; }
      .metric-card .value { font-size: 1.75rem; font-weight: 700; color: #2C3E50; }
      .metric-card .label { color: #6c757d; font-size: .85rem; text-transform: uppercase;
                             letter-spacing: .05em; }
      .footnote { color: #6c757d; font-size: .8rem; margin-top: 1rem; }
      .btn-primary { background-color: #18BC9C; border-color: #18BC9C; }
      .btn-primary:hover { background-color: #149a80; border-color: #149a80; }
      .drop-zone .form-control { border-style: dashed; }
      code.cp { background:#f4f6f8; padding:1px 5px; border-radius:3px;
                font-family: 'JetBrains Mono', monospace; color:#c0392b; }
      .ctrl-help-popover { max-width: 380px !important; }
      .ctrl-help-popover .popover-body table td,
      .ctrl-help-popover .popover-body table th { padding:.2rem .4rem;
                                                  font-size:.8rem; }
      .code-block { background:#1f2933; color:#f5f7fa; padding:1rem 1.25rem;
                    border-radius:.5rem; overflow:auto; max-height:640px;
                    font-size:.82rem; line-height:1.5; white-space:pre;
                    font-family: 'JetBrains Mono', Consolas, 'Courier New', monospace;
                    border: 1px solid #2c3e50; }
      .code-toolbar { display:flex; justify-content:space-between;
                      align-items:center; margin-bottom:.5rem; gap:.5rem; }
      .code-toolbar .btn { font-size:.85rem; }
    "))
  ),

  nav_panel(
    title = tagList(bs_icon("cloud-upload"), " Clean Dataset"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        div(class = "hero",
            h4("Strip non-ASCII characters"),
            p("Upload a SAS or R dataset. Special characters are replaced or removed, and every change is logged for audit.")),
        div(class = "drop-zone",
            fileInput("file", "Upload dataset",
                      accept = c(".sas7bdat", ".rds", ".RData", ".rdata"),
                      buttonLabel = "Browse...",
                      placeholder = ".sas7bdat / .rds / .RData")),
        # --- ASCII control-character toggle + help popover ---
        div(
          style = "display:flex; align-items:center; gap:.25rem; margin-top:.25rem;",
          div(style = "flex:1;",
              checkboxInput("strip_control",
                            "Also strip ASCII control characters",
                            value = FALSE)),
          popover(
            span(style = "cursor:pointer; color:#18BC9C;",
                 bs_icon("question-circle-fill")),
            title = "ASCII control characters",
            placement = "right",
            options = list(customClass = "ctrl-help-popover"),
            p(style = "margin-bottom:.5rem;",
              "Characters below 0x20 (plus DEL, 0x7F) are non-printable ",
              "ASCII control codes. They are invisible in most viewers ",
              "but commonly sneak in from Excel imports (carriage ",
              "returns from Alt+Enter line breaks, stray tabs, etc.)."),
            tags$table(class = "table table-sm mb-1",
              tags$thead(tags$tr(
                tags$th("Char"), tags$th("Code"), tags$th("Replaced with"))),
              tags$tbody(
                tags$tr(tags$td(tags$code("\\r")), tags$td("U+000D CR"),    tags$td("(removed)")),
                tags$tr(tags$td(tags$code("\\n")), tags$td("U+000A LF"),    tags$td("space")),
                tags$tr(tags$td(tags$code("\\t")), tags$td("U+0009 TAB"),   tags$td("space")),
                tags$tr(tags$td(tags$code("\\v")), tags$td("U+000B VT"),    tags$td("(removed)")),
                tags$tr(tags$td(tags$code("\\f")), tags$td("U+000C FF"),    tags$td("(removed)")),
                tags$tr(tags$td(tags$code("SUB")), tags$td("U+001A"),       tags$td("(removed)")),
                tags$tr(tags$td(tags$code("DEL")), tags$td("U+007F"),       tags$td("(removed)"))
              )
            ),
            p(style = "font-size:.8rem; color:#6c757d; margin-bottom:0;",
              "Every removal is logged in the change report.")
          )
        ),
        actionButton("clean_btn", " Clean Dataset",
                     icon = bs_icon("magic"),
                     class = "btn-primary w-100"),
        hr(),
        h6("Download cleaned output"),
        downloadButton("dl_rds", " as .rds",  class = "w-100 mb-2",
                       icon = bs_icon("file-earmark-binary")),
        downloadButton("dl_sas", " as .sas7bdat", class = "w-100 mb-2",
                       icon = bs_icon("file-earmark-code")),
        downloadButton("dl_csv", " as .csv", class = "w-100 mb-2",
                       icon = bs_icon("file-earmark-spreadsheet")),
        downloadButton("dl_report", " Change report (.csv)",
                       class = "w-100", icon = bs_icon("file-earmark-text")),
        hr(),
        h6("Need test data?"),
        downloadButton("dl_sample_rds", " Sample .rds",
                       class = "w-100 mb-2", icon = bs_icon("download")),
        downloadButton("dl_sample_sas", " Sample .sas7bdat",
                       class = "w-100", icon = bs_icon("download")),
        div(class = "footnote",
            "Tip: the sample includes curly quotes, accents, em-dashes, \u20ac,  \u00b0C and more.")
      ),

      layout_column_wrap(
        width = 1/3, heights_equal = "row",
        value_box(title = "Rows",           value = textOutput("m_rows"),
                  showcase = bs_icon("list-ol"),    theme = "primary"),
        value_box(title = "Columns",        value = textOutput("m_cols"),
                  showcase = bs_icon("layout-three-columns"), theme = "secondary"),
        value_box(title = "Values changed", value = textOutput("m_changes"),
                  showcase = bs_icon("pencil-square"), theme = "success")
      ),

      navset_card_tab(
        id = "results_tabs",
        nav_panel(
          title = tagList(bs_icon("eye"), " Original"),
          DTOutput("orig_table")
        ),
        nav_panel(
          title = tagList(bs_icon("check2-circle"), " Cleaned"),
          DTOutput("clean_table")
        ),
        nav_panel(
          title = tagList(bs_icon("journal-text"), " Change Report"),
          uiOutput("report_summary"),
          DTOutput("report_table")
        )
      )
    )
  ),

  nav_panel(
    title = tagList(bs_icon("code-slash"), " View Code"),
    div(class = "hero",
        h4("Do this in your own pipeline"),
        p("These are standalone scripts that replicate the app's cleaning logic. ",
          "Drop them into your own R project or SAS program and call them directly \u2014 ",
          "no Shiny required.")),
    navset_card_tab(
      id = "code_tabs",
      nav_panel(
        title = tagList(bs_icon("braces"), " R script"),
        div(class = "code-toolbar",
            span(tags$code("clean_special_chars.R"),
                 " \u00b7 requires ", tags$code("haven"), ", ",
                 tags$code("stringi")),
            downloadButton("dl_code_r", " Download .R",
                           icon = bs_icon("download"),
                           class = "btn-primary btn-sm")),
        tags$pre(class = "code-block", code_r_text)
      ),
      nav_panel(
        title = tagList(bs_icon("terminal"), " SAS program"),
        div(class = "code-toolbar",
            span(tags$code("clean_special_chars.sas"),
                 " \u00b7 requires UTF-8 SAS session"),
            downloadButton("dl_code_sas", " Download .sas",
                           icon = bs_icon("download"),
                           class = "btn-primary btn-sm")),
        tags$pre(class = "code-block", code_sas_text)
      )
    )
  ),

  nav_panel(
    title = tagList(bs_icon("info-circle"), " About"),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("How it works"),
        card_body(
          tags$ol(
            tags$li("Upload a ", tags$b(".sas7bdat"),
                    ", ", tags$b(".rds"), " or ", tags$b(".RData"), " file."),
            tags$li("The app scans every character / factor column (and variable names)."),
            tags$li("Known symbols are replaced via an explicit map (e.g. ",
                    tags$code("\u201csmart\u201d"), " \u2192 ", tags$code("\"straight\""), ")."),
            tags$li("Remaining non-ASCII characters are transliterated with ",
                    tags$code("stringi::stri_trans_general"),
                    " (Latin-ASCII)."),
            tags$li("Anything still outside ASCII is dropped."),
            tags$li("Enable ", tags$b("\u201cAlso strip ASCII control characters\u201d"),
                    " to also remove CR, LF, TAB, NUL and other invisible ",
                    "control codes \u2014 useful for data imported from Excel."),
            tags$li("Every change is logged: variable, row, codepoint, replacement.")
          )
        )
      ),
      card(
        card_header("Replacement examples"),
        card_body(
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(tags$th("From"), tags$th("Codepoint"), tags$th("To"))),
            tags$tbody(
              tags$tr(tags$td("\u201c \u201d"),  tags$td(tags$code(class="cp","U+201C/D")), tags$td('"')),
              tags$tr(tags$td("\u2018 \u2019"),  tags$td(tags$code(class="cp","U+2018/9")), tags$td("'")),
              tags$tr(tags$td("\u2013 \u2014"),  tags$td(tags$code(class="cp","U+2013/4")), tags$td("-")),
              tags$tr(tags$td("\u2026"),         tags$td(tags$code(class="cp","U+2026")),   tags$td("...")),
              tags$tr(tags$td("\u00e9 \u00fc \u00f1"), tags$td(tags$code(class="cp","Latin")),   tags$td("e u n")),
              tags$tr(tags$td("\u20ac"),         tags$td(tags$code(class="cp","U+20AC")),   tags$td("EUR")),
              tags$tr(tags$td("\u00b0"),         tags$td(tags$code(class="cp","U+00B0")),   tags$td(" deg")),
              tags$tr(tags$td("\u2122"),         tags$td(tags$code(class="cp","U+2122")),   tags$td("(TM)"))
            )
          )
        )
      )
    )
  )
)

# ==================================================================
# Server
# ==================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    original = NULL,
    cleaned  = NULL,
    report   = NULL,
    fname    = NULL
  )

  # ---- Read uploaded file ----
  read_any <- function(path, name) {
    ext <- tolower(tools::file_ext(name))
    switch(ext,
      "sas7bdat" = haven::read_sas(path),
      "rds"      = readRDS(path),
      "rdata"    = { e <- new.env(); load(path, envir = e)
                     obj <- ls(e); if (!length(obj))
                       stop("No objects found in .RData file.")
                     get(obj[1], envir = e) },
      stop("Unsupported file type: ", ext)
    )
  }

  # Sanitize names for sas7bdat: letters/digits/underscore,
  # start with letter or underscore, max 32 chars
  sas_safe_names <- function(nms) {
    nm <- gsub("[^A-Za-z0-9_]", "_", nms)
    nm <- ifelse(grepl("^[A-Za-z_]", nm), nm, paste0("V_", nm))
    nm <- substr(nm, 1, 32)
    make.unique(nm, sep = "_")
  }

  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read_any(input$file$datapath, input$file$name)
      if (!is.data.frame(df)) stop("Loaded object is not a data frame.")
      rv$original <- as.data.frame(df)
      rv$cleaned  <- NULL
      rv$report   <- NULL
      rv$fname    <- tools::file_path_sans_ext(input$file$name)
      showNotification(sprintf("Loaded %s (%d rows \u00d7 %d cols)",
                               input$file$name, nrow(df), ncol(df)),
                       type = "message")
    }, error = function(e) {
      showNotification(paste("Load failed:", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  })

  # ---- Clean button ----
  observeEvent(input$clean_btn, {
    req(rv$original)
    withProgress(message = "Cleaning dataset...", value = 0.3, {
      res <- clean_dataset(rv$original,
                           strip_control = isTRUE(input$strip_control))
      incProgress(0.6)
      rv$cleaned <- res$data
      rv$report  <- res$report
    })
    showNotification(
      sprintf("Done. %d value(s) changed%s.",
              nrow(rv$report),
              if (isTRUE(input$strip_control))
                " (incl. ASCII control chars)" else ""),
      type = "message"
    )
  })

  # ---- Metric boxes ----
  output$m_rows    <- renderText({ if (is.null(rv$original)) "-" else
                                   format(nrow(rv$original), big.mark = ",") })
  output$m_cols    <- renderText({ if (is.null(rv$original)) "-" else
                                   format(ncol(rv$original), big.mark = ",") })
  output$m_changes <- renderText({ if (is.null(rv$report))   "-" else
                                   format(nrow(rv$report), big.mark = ",") })

  # ---- Tables ----
  dt_opts <- list(pageLength = 10, scrollX = TRUE, dom = "ftip",
                  autoWidth = TRUE)

  output$orig_table <- renderDT({
    req(rv$original)
    datatable(rv$original, options = dt_opts, rownames = FALSE,
              class = "stripe hover compact")
  })

  output$clean_table <- renderDT({
    req(rv$cleaned)
    datatable(rv$cleaned, options = dt_opts, rownames = FALSE,
              class = "stripe hover compact")
  })

  output$report_table <- renderDT({
    req(rv$report)
    datatable(rv$report, options = dt_opts, rownames = FALSE,
              class = "stripe hover compact",
              colnames = c("Variable", "Row", "Original char(s)",
                           "Unicode", "Replacement")) |>
      formatStyle("original",    color = "#c0392b", fontWeight = "bold") |>
      formatStyle("replacement", color = "#18BC9C", fontWeight = "bold")
  })

  output$report_summary <- renderUI({
    req(rv$report)
    if (nrow(rv$report) == 0) {
      div(class = "alert alert-success",
          bs_icon("check-circle"),
          " No non-ASCII characters were found. Dataset is clean.")
    } else {
      unique_chars <- length(unique(rv$report$original))
      unique_vars  <- length(unique(rv$report$variable))
      div(class = "alert alert-info",
          bs_icon("info-circle"),
          sprintf(" %d change(s) across %d variable(s), %d unique character sequence(s).",
                  nrow(rv$report), unique_vars, unique_chars))
    }
  })

  # ---- Downloads (cleaned output) ----
  # NOTE: `f` is a tempfile path provided by Shiny — always writable,
  # including on shinyapps.io. Never write next to the app bundle.
  output$dl_rds <- downloadHandler(
    filename = function() paste0(rv$fname %||% "cleaned", "_clean.rds"),
    content  = function(f) {
      req(rv$cleaned)
      saveRDS(rv$cleaned, f)
    }
  )
  output$dl_sas <- downloadHandler(
    filename = function() paste0(rv$fname %||% "cleaned", "_clean.sas7bdat"),
    content  = function(f) {
      req(rv$cleaned)
      d <- rv$cleaned
      names(d) <- sas_safe_names(names(d))
      # write_sas is deprecated but still functional in haven >= 2.5
      suppressWarnings(haven::write_sas(d, f))
    }
  )
  output$dl_csv <- downloadHandler(
    filename = function() paste0(rv$fname %||% "cleaned", "_clean.csv"),
    content  = function(f) {
      req(rv$cleaned)
      write.csv(rv$cleaned, f, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$dl_report <- downloadHandler(
    filename = function() paste0(rv$fname %||% "dataset", "_change_report.csv"),
    content  = function(f) {
      req(rv$report)
      write.csv(rv$report, f, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ---- Sample dataset downloads ----
  # Always copy the bundled file from data/ into Shiny's tempfile `f`.
  # The bundled files are included in the deployment, so no runtime writes
  # to the app directory are needed (shinyapps.io may make it read-only).
  sample_path <- function(ext) {
    # Resolve relative to the running app directory so the lookup is
    # stable regardless of the current working directory.
    file.path(getwd(), "data", paste0("sample_specialchars.", ext))
  }

  # ---- Code-template downloads ----
  # Write the in-memory code text to Shiny's tempfile `f`.
  # Using writeLines (rather than file.copy) so it works the same
  # whether or not the source file was bundled at deploy time.
  output$dl_code_r <- downloadHandler(
    filename = function() "clean_special_chars.R",
    content  = function(f) writeLines(code_r_text, f, useBytes = TRUE)
  )
  output$dl_code_sas <- downloadHandler(
    filename = function() "clean_special_chars.sas",
    content  = function(f) writeLines(code_sas_text, f, useBytes = TRUE)
  )

  output$dl_sample_rds <- downloadHandler(
    filename = function() "sample_specialchars.rds",
    content  = function(f) {
      src <- sample_path("rds")
      if (!file.exists(src)) {
        # Defensive fallback: regenerate into Shiny's tempfile `f`
        saveRDS(make_sample_data(), f)
      } else {
        file.copy(src, f, overwrite = TRUE)
      }
    }
  )
  output$dl_sample_sas <- downloadHandler(
    filename = function() "sample_specialchars.sas7bdat",
    content  = function(f) {
      src <- sample_path("sas7bdat")
      if (!file.exists(src)) {
        d <- make_sample_data()
        names(d) <- sas_safe_names(names(d))
        suppressWarnings(haven::write_sas(d, f))
      } else {
        file.copy(src, f, overwrite = TRUE)
      }
    }
  )
}

# small helper
`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)

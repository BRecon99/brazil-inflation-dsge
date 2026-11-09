# =============================================================================
# 06_trade_steady_state_shares.R
# =============================================================================
# Purpose
# -------
# Calibrate the pre-oil-shock trade shares used in the DSGE:
#
#   x_y  = exports / GDP
#   cf_y = imported final-consumption goods / GDP
#   z_y  = imported intermediate goods / GDP
#
# and, when g_y and ig_y are already available:
#
#   cH_y = 1 - x_y - g_y - ig_y
#
# Baseline calibration window: 1968-1972
# Robustness window:          1967-1972
#
# IMPORTANT DESIGN CHANGES IN THIS VERSION
# ----------------------------------------
# 1. The IBGE national-accounts workbook is read from the exact historical sheet
#    I.2.1 and the exact published labels:
#       "Produto Interno Bruto"
#       "Exportacao de bens e servicos"
#       "Menos: Importacao de bens e servicos"
#
# 2. The script DOES NOT try to guess generic series names such as
#    exports_fob_usd_million in the external-sector workbook.
#
# 3. The external-sector FOB workbook is now OPTIONAL. It is used only as an
#    additional historical diagnostic. If it cannot be parsed, the calibration
#    still runs because x_y, M/Y, cf_y and z_y do not depend on it.
#
# 4. The parser detects whether years run across a row or down a column and is
#    written for old mixed-format Excel files.
#
# Economic mapping
# ----------------
# The national accounts give same-unit nominal ratios:
#
#   x_y,t = X_t / Y_t
#   m_y,t = M_t / Y_t
#
# Doellinger et al. (1974), Table VI.2 gives merchandise-import composition:
#
#   sC_t = consumer imports / merchandise imports
#   sZ_t = intermediate imports / merchandise imports
#
# We therefore construct the calibration bridge:
#
#   cf_y,t = m_y,t * sC_t
#   z_y,t  = m_y,t * sZ_t
#
# Capital-goods imports remain separate diagnostics. They are NOT folded into
# z_y because the current model's Z block represents imported intermediate
# inputs rather than private imported capital.
# =============================================================================

options(stringsAsFactors = FALSE)

# =============================================================================
# 0. PACKAGES AND DIRECTORIES
# =============================================================================

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    'Package "readxl" is required. Install it with:\n',
    'install.packages("readxl")'
  )
}

raw_dir       <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
estimates_dir <- file.path("output", "estimates")
figures_dir   <- file.path("output", "figures")
docs_dir      <- "docs"

for (d in c(processed_dir, estimates_dir, figures_dir, docs_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# =============================================================================
# 1. SMALL SAFE HELPERS
# =============================================================================

first_existing <- function(paths, label, required = TRUE) {
  hit <- paths[file.exists(paths)]
  
  if (length(hit) > 0) {
    return(hit[1])
  }
  
  if (required) {
    stop(
      "Could not find ", label, ". Tried:\n - ",
      paste(paths, collapse = "\n - ")
    )
  }
  
  NA_character_
}

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  y <- suppressWarnings(
    iconv(x, from = "", to = "ASCII//TRANSLIT")
  )
  
  y[is.na(y)] <- x[is.na(y)]
  y <- tolower(y)
  y <- gsub("[^a-z0-9]+", " ", y)
  y <- trimws(gsub("[[:space:]]+", " ", y))
  y
}

as_number_safe <- function(x) {
  # readxl usually returns numeric Excel cells as numeric already.
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  s <- trimws(as.character(x))
  s[is.na(x)] <- NA_character_
  s[s %in% c("", "NA", "N/A", "-", "--")] <- NA_character_
  
  out <- suppressWarnings(as.numeric(s))
  missing <- which(is.na(out) & !is.na(s))
  
  if (length(missing) > 0) {
    for (i in missing) {
      z <- s[i]
      
      # Remove common currency/text clutter without assuming a particular unit.
      z <- gsub("R\\$|US\\$", "", z)
      z <- gsub("[[:space:]]", "", z)
      
      # If a comma is present, interpret Brazilian-style decimal formatting.
      if (grepl(",", z, fixed = TRUE)) {
        z <- gsub("\\.", "", z)
        z <- sub(",", ".", z, fixed = TRUE)
      }
      
      out[i] <- suppressWarnings(as.numeric(z))
    }
  }
  
  out
}

extract_year_scalar <- function(x, year_min = 1800L, year_max = 2100L) {
  if (length(x) == 0 || is.na(x)) {
    return(NA_integer_)
  }
  
  # Direct numeric interpretation first.
  direct <- suppressWarnings(as.numeric(x))
  
  if (
    length(direct) == 1 &&
    is.finite(direct) &&
    abs(direct - round(direct)) < 1e-8 &&
    direct >= year_min &&
    direct <= year_max
  ) {
    return(as.integer(round(direct)))
  }
  
  # Safe text parser. No regexpr/regmatches/substring machinery.
  s <- trimws(as.character(x))
  
  if (!nzchar(s)) {
    return(NA_integer_)
  }
  
  cleaned <- gsub("[^0-9]+", " ", s)
  tokens <- strsplit(trimws(cleaned), "[[:space:]]+")[[1]]
  
  if (length(tokens) == 0) {
    return(NA_integer_)
  }
  
  tokens <- tokens[nchar(tokens) == 4]
  
  if (length(tokens) == 0) {
    return(NA_integer_)
  }
  
  candidates <- suppressWarnings(as.integer(tokens))
  candidates <- candidates[
    !is.na(candidates) &
      candidates >= year_min &
      candidates <= year_max
  ]
  
  if (length(candidates) == 0) {
    return(NA_integer_)
  }
  
  candidates[1]
}

make_year_matrix <- function(raw, year_min, year_max) {
  nr <- nrow(raw)
  nc <- ncol(raw)
  
  yrs <- matrix(
    NA_integer_,
    nrow = nr,
    ncol = nc
  )
  
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      yrs[i, j] <- extract_year_scalar(
        raw[[j]][i],
        year_min = year_min,
        year_max = year_max
      )
    }
  }
  
  yrs
}

# =============================================================================
# 2. READ AN OLD EXCEL TABLE AND DETECT ITS YEAR AXIS
# =============================================================================

read_raw_sheet <- function(path, sheet) {
  as.data.frame(
    readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      .name_repair = "minimal"
    ),
    stringsAsFactors = FALSE
  )
}

detect_year_layout <- function(raw, year_min, year_max, minimum_hits = 5) {
  yrs <- make_year_matrix(raw, year_min, year_max)
  
  row_hits <- apply(
    yrs,
    1,
    function(z) length(unique(z[!is.na(z)]))
  )
  
  col_hits <- apply(
    yrs,
    2,
    function(z) length(unique(z[!is.na(z)]))
  )
  
  best_row <- which.max(row_hits)
  best_col <- which.max(col_hits)
  
  if (row_hits[best_row] >= minimum_hits && row_hits[best_row] >= col_hits[best_col]) {
    return(list(
      orientation = "years_across_row",
      axis_index = best_row,
      year_matrix = yrs,
      hits = row_hits[best_row]
    ))
  }
  
  if (col_hits[best_col] >= minimum_hits) {
    return(list(
      orientation = "years_down_column",
      axis_index = best_col,
      year_matrix = yrs,
      hits = col_hits[best_col]
    ))
  }
  
  stop(
    "Could not identify a reliable year axis. Best row contained ",
    row_hits[best_row], " years; best column contained ",
    col_hits[best_col], " years."
  )
}

# =============================================================================
# 3. EXACT IBGE NATIONAL-ACCOUNTS EXTRACTOR
# =============================================================================

choose_axis_series <- function(
    labels,
    exact = NULL,
    contains = NULL,
    exclude = NULL,
    series_name
) {
  labels <- normalize_text(labels)
  keep <- rep(TRUE, length(labels))
  
  if (!is.null(contains)) {
    for (p in contains) {
      keep <- keep & grepl(normalize_text(p), labels, fixed = TRUE)
    }
  }
  
  if (!is.null(exclude)) {
    for (p in exclude) {
      keep <- keep & !grepl(normalize_text(p), labels, fixed = TRUE)
    }
  }
  
  # Prefer a literal exact label when available.
  if (!is.null(exact)) {
    target <- normalize_text(exact)
    exact_idx <- which(labels == target & keep)
    if (length(exact_idx) == 1) {
      return(exact_idx)
    }
    
    # Historical sheets often attach footnote numbers to the label. In that
    # case use containment while preserving the exclusion rules.
    keep <- keep & grepl(target, labels, fixed = TRUE)
  }
  
  idx <- which(keep)
  
  if (length(idx) != 1) {
    cat("\nAvailable normalized series labels:\n")
    for (k in seq_along(labels)) {
      if (nzchar(labels[k])) {
        cat(sprintf("[%03d] %s\n", k, labels[k]))
      }
    }
    
    stop(
      "Could not uniquely identify '", series_name,
      "'. Found ", length(idx), " match(es)."
    )
  }
  
  idx
}

extract_ibge_national_accounts <- function(path) {
  sheets <- readxl::excel_sheets(path)
  preferred <- "I.2.1"
  
  if (!(preferred %in% sheets)) {
    stop(
      "Expected sheet 'I.2.1' in ", basename(path),
      ". Available sheets include:\n",
      paste(sheets, collapse = ", ")
    )
  }
  
  raw <- read_raw_sheet(path, preferred)
  
  layout <- detect_year_layout(
    raw,
    year_min = 1947,
    year_max = 1989,
    minimum_hits = 10
  )
  
  cat(
    "\nNational-accounts sheet: ", preferred,
    "\nDetected layout: ", layout$orientation,
    "\n",
    sep = ""
  )
  
  wanted_years <- 1967:1972
  
  # ---------------------------------------------------------------------------
  # CASE A: years run across columns; economic series run down rows.
  # ---------------------------------------------------------------------------
  if (layout$orientation == "years_across_row") {
    year_row <- layout$axis_index
    year_vector <- layout$year_matrix[year_row, ]
    year_cols <- match(wanted_years, year_vector)
    
    if (any(is.na(year_cols))) {
      stop(
        "Could not find all years 1967-1972 in I.2.1. Found: ",
        paste(sort(unique(year_vector[!is.na(year_vector)])), collapse = ", ")
      )
    }
    
    first_year_col <- min(which(!is.na(year_vector)))
    label_columns <- if (first_year_col > 1) seq_len(first_year_col - 1) else 1
    candidate_rows <- seq.int(year_row + 1, nrow(raw))
    
    row_labels <- vapply(
      candidate_rows,
      function(i) {
        normalize_text(
          paste(
            unlist(raw[i, label_columns, drop = FALSE], use.names = FALSE),
            collapse = " "
          )
        )
      },
      character(1)
    )
    
    gdp_rel <- choose_axis_series(
      row_labels,
      exact = "Produto Interno Bruto",
      contains = "Produto Interno Bruto",
      exclude = c(
        "per capita",
        "custo de fatores",
        "precos constantes",
        "produto real",
        "produto nacional"
      ),
      series_name = "Produto Interno Bruto"
    )
    
    exp_rel <- choose_axis_series(
      row_labels,
      exact = "Exportacao de bens e servicos",
      contains = "Exportacao de bens e servicos",
      series_name = "Exportacao de bens e servicos"
    )
    
    imp_rel <- choose_axis_series(
      row_labels,
      exact = "Menos: Importacao de bens e servicos",
      contains = c("Menos", "Importacao de bens e servicos"),
      series_name = "Menos: Importacao de bens e servicos"
    )
    
    gdp_row <- candidate_rows[gdp_rel]
    exp_row <- candidate_rows[exp_rel]
    imp_row <- candidate_rows[imp_rel]
    
    cat(
      "Matched GDP row:     ", gdp_row, " -> ", row_labels[gdp_rel], "\n",
      "Matched export row:  ", exp_row, " -> ", row_labels[exp_rel], "\n",
      "Matched import row:  ", imp_row, " -> ", row_labels[imp_rel], "\n",
      sep = ""
    )
    
    value_at <- function(row_index) {
      vapply(
        year_cols,
        function(j) as_number_safe(raw[[j]][row_index]),
        numeric(1)
      )
    }
    
    out <- data.frame(
      year = wanted_years,
      gdp_current = value_at(gdp_row),
      exports_current = value_at(exp_row),
      imports_current_raw = value_at(imp_row),
      stringsAsFactors = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # CASE B: years run down rows; economic series run across columns.
  # ---------------------------------------------------------------------------
  if (layout$orientation == "years_down_column") {
    year_col <- layout$axis_index
    year_vector <- layout$year_matrix[, year_col]
    year_rows <- match(wanted_years, year_vector)
    
    if (any(is.na(year_rows))) {
      stop(
        "Could not find all years 1967-1972 in I.2.1. Found: ",
        paste(sort(unique(year_vector[!is.na(year_vector)])), collapse = ", ")
      )
    }
    
    first_year_row <- min(which(!is.na(year_vector)))
    header_rows <- if (first_year_row > 1) seq_len(first_year_row - 1) else 1
    
    col_labels <- vapply(
      seq_len(ncol(raw)),
      function(j) {
        normalize_text(
          paste(
            unlist(raw[header_rows, j, drop = FALSE], use.names = FALSE),
            collapse = " "
          )
        )
      },
      character(1)
    )
    
    gdp_col <- choose_axis_series(
      col_labels,
      exact = "Produto Interno Bruto",
      contains = "Produto Interno Bruto",
      exclude = c(
        "per capita",
        "custo de fatores",
        "precos constantes",
        "produto real",
        "produto nacional"
      ),
      series_name = "Produto Interno Bruto"
    )
    
    exp_col <- choose_axis_series(
      col_labels,
      exact = "Exportacao de bens e servicos",
      contains = "Exportacao de bens e servicos",
      series_name = "Exportacao de bens e servicos"
    )
    
    imp_col <- choose_axis_series(
      col_labels,
      exact = "Menos: Importacao de bens e servicos",
      contains = c("Menos", "Importacao de bens e servicos"),
      series_name = "Menos: Importacao de bens e servicos"
    )
    
    cat(
      "Matched GDP column:     ", gdp_col, " -> ", col_labels[gdp_col], "\n",
      "Matched export column:  ", exp_col, " -> ", col_labels[exp_col], "\n",
      "Matched import column:  ", imp_col, " -> ", col_labels[imp_col], "\n",
      sep = ""
    )
    
    out <- data.frame(
      year = wanted_years,
      gdp_current = as_number_safe(raw[[gdp_col]][year_rows]),
      exports_current = as_number_safe(raw[[exp_col]][year_rows]),
      imports_current_raw = as_number_safe(raw[[imp_col]][year_rows]),
      stringsAsFactors = FALSE
    )
  }
  
  out$imports_current <- abs(out$imports_current_raw)
  
  if (any(!is.finite(out$gdp_current)) || any(out$gdp_current <= 0)) {
    stop("GDP extraction from I.2.1 produced missing/non-positive values.")
  }
  
  if (any(!is.finite(out$exports_current))) {
    stop("Export extraction from I.2.1 produced missing values.")
  }
  
  if (any(!is.finite(out$imports_current))) {
    stop("Import extraction from I.2.1 produced missing values.")
  }
  
  out$x_y <- out$exports_current / out$gdp_current
  out$m_y <- out$imports_current / out$gdp_current
  
  attr(out, "sheet") <- preferred
  attr(out, "layout") <- layout$orientation
  out
}

# =============================================================================
# 4. OPTIONAL EXTERNAL-SECTOR FOB EXTRACTOR
# =============================================================================
# This part is diagnostic only. If parsing fails, Script 06 continues.
# Exact labels verified from the workbook:
#   "Exportacoes (FOB)"
#   "Importacoes (FOB)"
#   "Saldo Comercial"

build_axis_labels <- function(raw, layout) {
  if (layout$orientation == "years_down_column") {
    year_col <- layout$axis_index
    year_values <- layout$year_matrix[, year_col]
    year_rows <- which(!is.na(year_values))
    first_year_row <- min(year_rows)
    
    header_rows <- if (first_year_row > 1) seq_len(first_year_row - 1) else 1
    
    labels <- vapply(
      seq_len(ncol(raw)),
      function(j) {
        paste(
          normalize_text(
            unlist(raw[header_rows, j, drop = FALSE], use.names = FALSE)
          ),
          collapse = " "
        ) |> normalize_text()
      },
      character(1)
    )
    
    return(list(
      labels = labels,
      year_values = year_values,
      year_rows = year_rows
    ))
  }
  
  year_row <- layout$axis_index
  year_values <- layout$year_matrix[year_row, ]
  year_cols <- which(!is.na(year_values))
  first_year_col <- min(year_cols)
  
  label_cols <- if (first_year_col > 1) seq_len(first_year_col - 1) else 1
  
  labels <- vapply(
    seq_len(nrow(raw)),
    function(i) {
      paste(
        normalize_text(
          unlist(raw[i, label_cols, drop = FALSE], use.names = FALSE)
        ),
        collapse = " "
      ) |> normalize_text()
    },
    character(1)
  )
  
  list(
    labels = labels,
    year_values = year_values,
    year_cols = year_cols
  )
}

choose_label_index <- function(labels, exact_phrase, label_name) {
  target <- normalize_text(exact_phrase)
  
  exact <- which(labels == target)
  if (length(exact) == 1) return(exact)
  
  contains <- which(grepl(target, labels, fixed = TRUE))
  if (length(contains) == 1) return(contains)
  
  stop(
    "Could not uniquely identify '", label_name,
    "'. Matches found: ", length(contains), "."
  )
}

extract_external_fob_optional <- function(path) {
  tryCatch({
    sheets <- readxl::excel_sheets(path)
    
    # The workbook itself identifies this historical table as sheet 2.
    sheet <- if ("2" %in% sheets) "2" else sheets[2]
    
    raw <- read_raw_sheet(path, sheet)
    
    layout <- detect_year_layout(
      raw,
      year_min = 1901,
      year_max = 2000,
      minimum_hits = 20
    )
    
    cat(
      "\nExternal-sector sheet: ", sheet,
      "\nDetected layout: ", layout$orientation,
      "\n",
      sep = ""
    )
    
    info <- build_axis_labels(raw, layout)
    
    exp_idx <- choose_label_index(
      info$labels,
      "Exportacoes FOB",
      "Exportacoes (FOB)"
    )
    
    imp_idx <- choose_label_index(
      info$labels,
      "Importacoes FOB",
      "Importacoes (FOB)"
    )
    
    if (layout$orientation == "years_down_column") {
      year_col <- layout$axis_index
      years <- layout$year_matrix[, year_col]
      rows <- which(!is.na(years) & years >= 1967 & years <= 1972)
      
      out <- data.frame(
        year = years[rows],
        exports_fob_usd_million = as_number_safe(raw[[exp_idx]][rows]),
        imports_fob_usd_million = abs(as_number_safe(raw[[imp_idx]][rows])),
        stringsAsFactors = FALSE
      )
    } else {
      year_row <- layout$axis_index
      years <- layout$year_matrix[year_row, ]
      cols <- which(!is.na(years) & years >= 1967 & years <= 1972)
      
      out <- data.frame(
        year = years[cols],
        exports_fob_usd_million = vapply(
          cols,
          function(j) as_number_safe(raw[[j]][exp_idx]),
          numeric(1)
        ),
        imports_fob_usd_million = abs(vapply(
          cols,
          function(j) as_number_safe(raw[[j]][imp_idx]),
          numeric(1)
        )),
        stringsAsFactors = FALSE
      )
    }
    
    out <- out[order(out$year), ]
    out$trade_balance_usd_million <-
      out$exports_fob_usd_million - out$imports_fob_usd_million
    
    out
  }, error = function(e) {
    warning(
      paste0(
        "Optional FOB diagnostic could not be read and will be skipped. ",
        "This does NOT affect x_y, cf_y or z_y. Details: ",
        conditionMessage(e)
      )
    )
    NULL
  })
}

# =============================================================================
# 5. INPUT FILES
# =============================================================================

file_na <- first_existing(
  c(
    file.path(raw_dir, "1_2_scn_consolidado.xls"),
    "1_2_scn_consolidado.xls"
  ),
  "IBGE national-accounts workbook"
)

file_external <- first_existing(
  c(
    file.path(raw_dir, "02_setor_externo_1901_00 (2).xls"),
    file.path(raw_dir, "02_setor_externo_1901_00.xls"),
    file.path(raw_dir, "02_setor_externo_1901_00(2).xls"),
    "02_setor_externo_1901_00 (2).xls",
    "02_setor_externo_1901_00.xls"
  ),
  "IBGE external-sector workbook",
  required = FALSE
)

file_doellinger <- file.path(
  processed_dir,
  "brazil_import_composition_doellinger_1967_1972.csv"
)

if (!file.exists(file_doellinger)) {
  script00 <- file.path("R", "00_build_historical_source_database.R")
  
  if (file.exists(script00)) {
    cat("\nDoellinger processed database not found. Running Script 00...\n")
    source(script00, local = new.env())
  }
}

if (!file.exists(file_doellinger)) {
  stop(
    "Missing ", file_doellinger,
    ". Run R/00_build_historical_source_database.R first."
  )
}

cat("\n============================================================\n")
cat("SCRIPT 06 INPUTS\n")
cat("============================================================\n")
cat("National accounts: ", file_na, "\n", sep = "")
cat(
  "External FOB file: ",
  if (is.na(file_external)) "NOT FOUND - diagnostic will be skipped" else file_external,
  "\n",
  sep = ""
)
cat("Doellinger:       ", file_doellinger, "\n", sep = "")

# =============================================================================
# 6. NATIONAL ACCOUNTS: DIRECT SAME-UNIT RATIOS
# =============================================================================

na <- extract_ibge_national_accounts(file_na)

write.csv(
  na,
  file.path(processed_dir, "brazil_national_accounts_trade_shares_1967_1972.csv"),
  row.names = FALSE,
  na = ""
)

cat("\n============================================================\n")
cat("NATIONAL ACCOUNTS, 1967-1972\n")
cat("============================================================\n")

print(
  na[, c(
    "year",
    "gdp_current",
    "exports_current",
    "imports_current",
    "x_y",
    "m_y"
  )],
  row.names = FALSE
)

# =============================================================================
# 7. DOELLINGER IMPORT COMPOSITION
# =============================================================================

doel <- read.csv(
  file_doellinger,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

required_doellinger <- c(
  "year",
  "capital_goods_pct",
  "intermediate_goods_pct",
  "consumer_nondurable_pct",
  "consumer_durable_pct",
  "consumer_goods_total_pct",
  "other_pct"
)

missing_doellinger <- setdiff(required_doellinger, names(doel))

if (length(missing_doellinger) > 0) {
  stop(
    "Doellinger database is missing expected columns: ",
    paste(missing_doellinger, collapse = ", ")
  )
}

doel$year <- as.integer(doel$year)
doel <- doel[doel$year >= 1967 & doel$year <= 1972, ]

doel$composition_sum <- with(
  doel,
  capital_goods_pct +
    intermediate_goods_pct +
    consumer_goods_total_pct +
    other_pct
)

if (any(abs(doel$composition_sum - 100) > 0.05)) {
  warning("At least one Doellinger row does not sum to approximately 100%.")
}

# =============================================================================
# 8. MAP HISTORICAL DATA INTO MODEL SHARES
# =============================================================================

bridge <- merge(
  na,
  doel,
  by = "year",
  all = FALSE
)

bridge <- bridge[order(bridge$year), ]

if (nrow(bridge) != 6 || !all(bridge$year == 1967:1972)) {
  stop(
    "Expected complete merged observations for 1967-1972. Found years: ",
    paste(bridge$year, collapse = ", ")
  )
}

# Model-relevant bridge:
# category/GDP = (total imports/GDP) * (category/total merchandise imports)

bridge$cf_y <-
  bridge$m_y * bridge$consumer_goods_total_pct / 100

bridge$z_y <-
  bridge$m_y * bridge$intermediate_goods_pct / 100

# Deliberately separate diagnostics.
bridge$capital_import_y_bridge <-
  bridge$m_y * bridge$capital_goods_pct / 100

bridge$other_import_y_bridge <-
  bridge$m_y * bridge$other_pct / 100

bridge$modeled_import_y <- bridge$cf_y + bridge$z_y

bridge$bridge_residual_y <-
  bridge$m_y - bridge$modeled_import_y

bridge$modeled_import_share_pct <-
  100 * bridge$modeled_import_y / bridge$m_y

write.csv(
  bridge,
  file.path(processed_dir, "trade_steady_state_annual_1967_1972.csv"),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 9. BASELINE CALIBRATION: 1968-1972
# =============================================================================

base <- bridge[bridge$year >= 1968 & bridge$year <= 1972, ]

if (nrow(base) != 5) {
  stop("Expected five baseline years 1968-1972; found ", nrow(base), ".")
}

summary_stats <- function(x) {
  x <- x[is.finite(x)]
  
  c(
    n = length(x),
    mean = mean(x),
    median = median(x),
    sd = if (length(x) > 1) sd(x) else NA_real_,
    min = min(x),
    max = max(x)
  )
}

make_parameter_row <- function(parameter, values, interpretation, method) {
  s <- summary_stats(values)
  
  data.frame(
    parameter = parameter,
    value = unname(s["mean"]),
    n = as.integer(s["n"]),
    median = unname(s["median"]),
    annual_sd = unname(s["sd"]),
    min = unname(s["min"]),
    max = unname(s["max"]),
    sample = "1968-1972",
    method = method,
    interpretation = interpretation,
    recommended_baseline = "YES",
    stringsAsFactors = FALSE
  )
}

params <- rbind(
  make_parameter_row(
    "x_y",
    base$x_y,
    "Exports as a share of GDP",
    "Arithmetic mean of annual same-unit national-accounts X/Y"
  ),
  make_parameter_row(
    "cf_y",
    base$cf_y,
    "Imported final-consumption goods as a share of GDP",
    "Arithmetic mean of annual (M/Y x Doellinger consumer-goods share)"
  ),
  make_parameter_row(
    "z_y",
    base$z_y,
    "Imported intermediate goods as a share of GDP",
    "Arithmetic mean of annual (M/Y x Doellinger intermediate-goods share)"
  )
)

diagnostics <- rbind(
  make_parameter_row(
    "m_y_diagnostic",
    base$m_y,
    "Total national-accounts imports as a share of GDP",
    "Arithmetic mean of annual M/Y"
  ),
  make_parameter_row(
    "capital_import_y_bridge",
    base$capital_import_y_bridge,
    "Capital-goods import share implied by the composition bridge",
    "Arithmetic mean of annual (M/Y x Doellinger capital-goods share)"
  ),
  make_parameter_row(
    "bridge_residual_y",
    base$bridge_residual_y,
    "Imports not explicitly represented by cf_y + z_y",
    "Arithmetic mean of annual M/Y - cf_y - z_y"
  )
)

diagnostics$recommended_baseline <- "DIAGNOSTIC"

write.csv(
  params,
  file.path(estimates_dir, "trade_steady_state_share_parameters.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  diagnostics,
  file.path(estimates_dir, "trade_steady_state_share_diagnostics.csv"),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 10. ROBUSTNESS: INCLUDE 1967 + MEDIAN
# =============================================================================

rob <- bridge[bridge$year >= 1967 & bridge$year <= 1972, ]

robustness <- data.frame(
  parameter = c("x_y", "cf_y", "z_y"),
  baseline_mean_1968_1972 = c(
    mean(base$x_y),
    mean(base$cf_y),
    mean(base$z_y)
  ),
  mean_1967_1972 = c(
    mean(rob$x_y),
    mean(rob$cf_y),
    mean(rob$z_y)
  ),
  median_1968_1972 = c(
    median(base$x_y),
    median(base$cf_y),
    median(base$z_y)
  ),
  stringsAsFactors = FALSE
)

robustness$change_include_1967 <-
  robustness$mean_1967_1972 - robustness$baseline_mean_1968_1972

robustness$pct_change_include_1967 <-
  100 * robustness$change_include_1967 /
  robustness$baseline_mean_1968_1972

write.csv(
  robustness,
  file.path(estimates_dir, "trade_steady_state_share_robustness.csv"),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 11. OPTIONAL FOB RECONSTRUCTION
# =============================================================================

fob <- NULL

if (!is.na(file_external)) {
  fob <- extract_external_fob_optional(file_external)
}

if (!is.null(fob)) {
  fob_use <- merge(
    fob,
    doel,
    by = "year",
    all = FALSE
  )
  
  fob_use$intermediate_goods_fob_usd_million <-
    fob_use$imports_fob_usd_million * fob_use$intermediate_goods_pct / 100
  
  fob_use$consumer_goods_fob_usd_million <-
    fob_use$imports_fob_usd_million * fob_use$consumer_goods_total_pct / 100
  
  fob_use$capital_goods_fob_usd_million <-
    fob_use$imports_fob_usd_million * fob_use$capital_goods_pct / 100
  
  write.csv(
    fob_use,
    file.path(processed_dir, "brazil_imports_by_use_fob_1967_1972.csv"),
    row.names = FALSE,
    na = ""
  )
}

# =============================================================================
# 12. DERIVE cH_y IF g_y AND ig_y ARE ALREADY AVAILABLE
# =============================================================================

registry_path <- file.path(docs_dir, "parameter_registry.csv")

read_existing_parameter <- function(parameter_name) {
  candidate_files <- c(
    registry_path,
    file.path("output", "estimates", "steady_state_fiscal_share_parameters.csv"),
    file.path("output", "steady_state_fiscal_share_parameters.csv"),
    file.path("outputs", "steady_state_fiscal_share_parameters.csv")
  )
  
  candidate_files <- candidate_files[file.exists(candidate_files)]
  
  for (f in candidate_files) {
    d <- tryCatch(
      read.csv(f, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(d)) next
    
    key_col <- c("dynare_name", "parameter", "name")
    key_col <- key_col[key_col %in% names(d)]
    
    value_col <- c(
      "research_value",
      "value",
      "baseline_value",
      "calibrated_value",
      "estimate"
    )
    value_col <- value_col[value_col %in% names(d)]
    
    if (length(key_col) == 0 || length(value_col) == 0) next
    
    idx <- which(
      trimws(as.character(d[[key_col[1]]])) == parameter_name
    )
    
    if (length(idx) != 1) next
    
    value <- as_number_safe(d[[value_col[1]]][idx])
    
    if (length(value) == 1 && is.finite(value)) {
      return(value)
    }
  }
  
  NA_real_
}

g_y  <- read_existing_parameter("g_y")
ig_y <- read_existing_parameter("ig_y")
x_y  <- params$value[params$parameter == "x_y"]

cH_y <- NA_real_

if (is.finite(g_y) && is.finite(ig_y)) {
  cH_y <- 1 - x_y - g_y - ig_y
  
  params <- rbind(
    params,
    data.frame(
      parameter = "cH_y",
      value = cH_y,
      n = NA_integer_,
      median = NA_real_,
      annual_sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      sample = "Derived identity",
      method = "cH_y = 1 - x_y - g_y - ig_y",
      interpretation = "Domestic household-consumption weight in domestic output",
      recommended_baseline = "YES",
      stringsAsFactors = FALSE
    )
  )
  
  write.csv(
    params,
    file.path(estimates_dir, "trade_steady_state_share_parameters.csv"),
    row.names = FALSE,
    na = ""
  )
}

# =============================================================================
# 13. UPDATE PARAMETER REGISTRY, CONSERVATIVELY
# =============================================================================

set_if_column_exists <- function(df, row_id, column, value) {
  if (column %in% names(df)) {
    df[row_id, column] <- value
  }
  df
}

if (file.exists(registry_path)) {
  registry <- read.csv(
    registry_path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  key_col <- if ("dynare_name" %in% names(registry)) {
    "dynare_name"
  } else if ("parameter" %in% names(registry)) {
    "parameter"
  } else {
    NA_character_
  }
  
  if (!is.na(key_col)) {
    update_one <- function(registry, p, value, method, sample, note, classification = "Calibrated") {
      idx <- which(trimws(as.character(registry[[key_col]])) == p)
      
      if (length(idx) != 1) {
        warning(
          "Registry update skipped for ", p,
          ": expected exactly one row, found ", length(idx), "."
        )
        return(registry)
      }
      
      registry <- set_if_column_exists(registry, idx, "classification", classification)
      registry <- set_if_column_exists(
        registry,
        idx,
        "research_value",
        format(value, digits = 12)
      )
      registry <- set_if_column_exists(
        registry,
        idx,
        "status",
        "Calibrated candidate - review Script 06 diagnostics"
      )
      registry <- set_if_column_exists(registry, idx, "method", method)
      registry <- set_if_column_exists(registry, idx, "sample", sample)
      registry <- set_if_column_exists(
        registry,
        idx,
        "raw_or_primary_data",
        "IBGE historical national accounts I.2.1; Doellinger et al. (1974), Table VI.2"
      )
      registry <- set_if_column_exists(
        registry,
        idx,
        "source_or_literature",
        "IBGE historical national accounts; Doellinger, Faria & Cavalcanti (1974)"
      )
      registry <- set_if_column_exists(registry, idx, "notes", note)
      
      registry
    }
    
    for (p in c("x_y", "cf_y", "z_y")) {
      r <- params[params$parameter == p, ]
      
      note <- if (p == "x_y") {
        paste0(
          "Direct same-unit national-accounts ratio. 1968-1972 mean; median=",
          sprintf("%.6f", r$median),
          "; annual SD=", sprintf("%.6f", r$annual_sd), "."
        )
      } else {
        paste0(
          "1968-1972 calibration bridge from national-accounts M/Y and ",
          "Doellinger merchandise-import composition. Median=",
          sprintf("%.6f", r$median),
          "; annual SD=", sprintf("%.6f", r$annual_sd),
          ". Capital-goods imports are not folded into z_y."
        )
      }
      
      registry <- update_one(
        registry,
        p,
        r$value,
        r$method,
        "1968-1972",
        note
      )
    }
    
    if (is.finite(cH_y)) {
      registry <- update_one(
        registry,
        "cH_y",
        cH_y,
        "cH_y = 1 - x_y - g_y - ig_y",
        "Steady-state identity",
        paste0(
          "Derived using x_y=", sprintf("%.6f", x_y),
          ", g_y=", sprintf("%.6f", g_y),
          ", ig_y=", sprintf("%.6f", ig_y), "."
        ),
        classification = "Derived"
      )
    }
    
    write.csv(
      registry,
      registry_path,
      row.names = FALSE,
      na = ""
    )
  }
}

# =============================================================================
# 14. DOCUMENTATION
# =============================================================================

fmt <- function(x) sprintf("%.6f", x)

xrow  <- params[params$parameter == "x_y", ]
cfrow <- params[params$parameter == "cf_y", ]
zrow  <- params[params$parameter == "z_y", ]
mdiag <- diagnostics[diagnostics$parameter == "m_y_diagnostic", ]
capdiag <- diagnostics[diagnostics$parameter == "capital_import_y_bridge", ]
resdiag <- diagnostics[diagnostics$parameter == "bridge_residual_y", ]

doc <- c(
  "# Trade steady-state shares",
  "",
  "## Baseline sample",
  "",
  "1968-1972.",
  "",
  "## Source mapping",
  "",
  paste(
    "- `x_y` uses `Exportacao de bens e servicos / Produto Interno Bruto`",
    "from IBGE historical national accounts, sheet I.2.1."
  ),
  paste(
    "- `M/Y` uses `Menos: Importacao de bens e servicos / Produto Interno Bruto`",
    "from the same table and valuation system."
  ),
  "- `cf_y = (M/Y) x consumer-goods import share` from Doellinger et al. (1974).",
  "- `z_y = (M/Y) x intermediate-goods import share` from Doellinger et al. (1974).",
  "",
  "## Candidate values",
  "",
  paste0("- x_y = ", fmt(xrow$value)),
  paste0("- cf_y = ", fmt(cfrow$value)),
  paste0("- z_y = ", fmt(zrow$value)),
  paste0("- total M/Y diagnostic = ", fmt(mdiag$value)),
  paste0("- capital-import bridge diagnostic = ", fmt(capdiag$value)),
  paste0("- bridge residual = ", fmt(resdiag$value)),
  "",
  "## Statistical treatment",
  "",
  paste(
    "These are accounting calibrations rather than dynamic-process estimates.",
    "The baseline is the arithmetic mean of annual ratios. Median, annual",
    "standard deviation, min/max, and inclusion of 1967 are reported as",
    "descriptive robustness checks."
  ),
  "",
  "## Caveat",
  "",
  paste(
    "Doellinger reports merchandise-import composition, while national-accounts",
    "imports are a broader aggregate. Therefore cf_y and z_y are calibration",
    "bridges, not exact accounting identities."
  ),
  paste(
    "Capital-goods imports remain explicit diagnostics and are not silently",
    "reclassified as imported intermediate inputs."
  ),
  "",
  if (is.finite(cH_y)) {
    paste0(
      "## Derived cH_y\n\n",
      "cH_y = 1 - x_y - g_y - ig_y = ", fmt(cH_y), "."
    )
  } else {
    paste(
      "## Derived cH_y\n\n",
      "cH_y was not derived because g_y and/or ig_y were not found in the existing outputs."
    )
  },
  ""
)

writeLines(
  doc,
  file.path(docs_dir, "parameter_trade_steady_state_shares.md"),
  useBytes = TRUE
)

# =============================================================================
# 15. FIGURE
# =============================================================================

png(
  file.path(figures_dir, "trade_steady_state_shares_1967_1972.png"),
  width = 1400,
  height = 900,
  res = 150
)

matplot(
  bridge$year,
  100 * cbind(
    bridge$x_y,
    bridge$m_y,
    bridge$cf_y,
    bridge$z_y
  ),
  type = "o",
  pch = c(16, 17, 1, 2),
  lty = c(1, 1, 2, 2),
  xlab = "Year",
  ylab = "Share of GDP (%)",
  main = "Brazil: Trade Steady-State Share Inputs, 1967-1972"
)

legend(
  "topleft",
  legend = c(
    "Exports/GDP",
    "Imports/GDP",
    "Consumer imports/GDP",
    "Intermediate imports/GDP"
  ),
  pch = c(16, 17, 1, 2),
  lty = c(1, 1, 2, 2),
  bty = "n"
)

dev.off()

# =============================================================================
# 16. CONSOLE REPORT
# =============================================================================

cat("\n============================================================\n")
cat("TRADE STEADY-STATE CALIBRATION COMPLETE\n")
cat("============================================================\n")
cat("Baseline: 1968-1972\n")
cat("Core data source: IBGE national accounts I.2.1 + Doellinger Table VI.2\n")
cat("FOB external-sector workbook: optional diagnostic only\n")

cat("\nANNUAL CALIBRATION TABLE\n\n")

print(
  base[, c(
    "year",
    "x_y",
    "m_y",
    "consumer_goods_total_pct",
    "intermediate_goods_pct",
    "cf_y",
    "z_y",
    "capital_import_y_bridge",
    "bridge_residual_y",
    "modeled_import_share_pct"
  )],
  row.names = FALSE
)

cat("\nPARAMETER CANDIDATES\n\n")

print(
  params[, c(
    "parameter",
    "value",
    "median",
    "annual_sd",
    "min",
    "max",
    "sample"
  )],
  row.names = FALSE
)

cat("\nIMPORT-COVERAGE DIAGNOSTICS\n\n")

print(
  diagnostics[, c(
    "parameter",
    "value",
    "median",
    "annual_sd",
    "min",
    "max"
  )],
  row.names = FALSE
)

cat("\nROBUSTNESS\n\n")
print(robustness, row.names = FALSE)

if (!is.null(fob)) {
  cat("\nOPTIONAL FOB DIAGNOSTIC, 1967-1972 (US$ million)\n\n")
  print(fob, row.names = FALSE)
} else {
  cat(
    "\nOPTIONAL FOB DIAGNOSTIC was skipped. ",
    "This does not affect the calibrated parameters.\n",
    sep = ""
  )
}

if (is.finite(cH_y)) {
  cat("\nDERIVED cH_y\n\n")
  cat(
    "cH_y = 1 - ", fmt(x_y),
    " - ", fmt(g_y),
    " - ", fmt(ig_y),
    " = ", fmt(cH_y), "\n",
    sep = ""
  )
}

cat("\nFILES CREATED/UPDATED\n")
cat(" - data/processed/brazil_national_accounts_trade_shares_1967_1972.csv\n")
cat(" - data/processed/trade_steady_state_annual_1967_1972.csv\n")
cat(" - output/estimates/trade_steady_state_share_parameters.csv\n")
cat(" - output/estimates/trade_steady_state_share_diagnostics.csv\n")
cat(" - output/estimates/trade_steady_state_share_robustness.csv\n")
cat(" - output/figures/trade_steady_state_shares_1967_1972.png\n")
cat(" - docs/parameter_trade_steady_state_shares.md\n")
cat(" - docs/parameter_registry.csv (if compatible)\n")

if (!is.null(fob)) {
  cat(" - data/processed/brazil_imports_by_use_fob_1967_1972.csv\n")
}

cat("\nIMPORTANT\n")
cat(
  "x_y is a direct same-unit national-accounts ratio. cf_y and z_y are\n",
  "calibration bridges. Review the capital-import and bridge-residual diagnostics\n",
  "before marking cf_y and z_y final.\n",
  sep = ""
)

# =============================================================================
# 07_alpha_mu_lambda_s.R
# =============================================================================
#
# PURPOSE
# -------
# Calibrate / derive:
#
#   alpha     = imported-consumption weight in the household CES aggregator
#   mu        = imported-intermediate-input cost share in production
#   lambda_s  = total terms-of-trade loading in marginal cost
#
# MODEL MAPPING
# -------------
#
#   alpha_t = cf_y_t / c_y_t
#
# where:
#   cf_y_t = imported consumer goods / GDP   (from Script 06)
#   c_y_t  = private household consumption / GDP
#
# and:
#
#   mu_t = z_y_t / (labor_share_t + z_y_t)
#
# where:
#   z_y_t = imported intermediate goods / GDP   (from Script 06)
#
# Finally:
#
#   lambda_s = mu + (1-mu)*alpha
#
# BASELINE SAMPLE
# ---------------
# 1968-1972
#
# IMPORTANT FIX IN THIS VERSION
# -----------------------------
# Script 07 no longer tries to identify GDP again inside the IBGE workbook.
# Script 06 already extracted and validated GDP, so this script reuses
# gdp_current from:
#
#   data/processed/trade_steady_state_annual_1967_1972.csv
#
# The IBGE workbook is opened only to identify household/private consumption.
# This avoids the previous ambiguity from multiple rows containing the phrase
# "Produto Interno Bruto".
# =============================================================================

options(stringsAsFactors = FALSE)

# =============================================================================
# 0. PACKAGE + DIRECTORIES
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
# 1. INPUTS
# =============================================================================

file_trade <- file.path(
  processed_dir,
  "trade_steady_state_annual_1967_1972.csv"
)

file_na <- file.path(
  raw_dir,
  "1_2_scn_consolidado.xls"
)

if (!file.exists(file_trade)) {
  stop(
    "Missing Script 06 output:\n",
    file_trade,
    "\nRun R/06_trade_steady_state_shares.R first."
  )
}

if (!file.exists(file_na)) {
  stop(
    "Missing IBGE national-accounts workbook:\n",
    file_na
  )
}

# =============================================================================
# 2. HELPERS
# =============================================================================

norm_text <- function(x) {
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  y <- suppressWarnings(
    iconv(
      x,
      from = "",
      to = "ASCII//TRANSLIT"
    )
  )
  
  y[is.na(y)] <- x[is.na(y)]
  
  y <- tolower(y)
  y <- gsub("[\r\n\t]+", " ", y)
  y <- gsub("[^a-z0-9]+", " ", y)
  y <- trimws(
    gsub(
      "[[:space:]]+",
      " ",
      y
    )
  )
  
  y
}


as_num <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  z <- trimws(as.character(x))
  
  z[
    z %in% c(
      "",
      "NA",
      "N/A",
      "-",
      "--"
    )
  ] <- NA_character_
  
  ans <- suppressWarnings(
    as.numeric(z)
  )
  
  miss <- is.na(ans) & !is.na(z)
  
  if (any(miss)) {
    
    z2 <- z[miss]
    
    z2 <- gsub(
      "R\\$|US\\$",
      "",
      z2
    )
    
    z2 <- gsub(
      "[[:space:]]",
      "",
      z2
    )
    
    has_comma <- grepl(
      ",",
      z2,
      fixed = TRUE
    )
    
    z2[has_comma] <- gsub(
      "\\.",
      "",
      z2[has_comma]
    )
    
    z2[has_comma] <- sub(
      ",",
      ".",
      z2[has_comma],
      fixed = TRUE
    )
    
    ans[miss] <- suppressWarnings(
      as.numeric(z2)
    )
  }
  
  ans
}


as_year_flexible <- function(
    x,
    year_min = 1800,
    year_max = 2100
) {
  
  s <- as.character(x)
  s[is.na(x)] <- NA_character_
  s <- trimws(s)
  
  out <- rep(
    NA_integer_,
    length(s)
  )
  
  direct <- suppressWarnings(
    as.numeric(s)
  )
  
  good_direct <- (
    !is.na(direct) &
      abs(direct - round(direct)) < 1e-8 &
      direct >= year_min &
      direct <= year_max
  )
  
  out[good_direct] <- as.integer(
    round(
      direct[good_direct]
    )
  )
  
  left <- which(
    is.na(out) &
      !is.na(s) &
      nzchar(s)
  )
  
  if (length(left)) {
    
    for (ii in left) {
      
      cleaned <- gsub(
        "[^0-9]+",
        " ",
        s[ii]
      )
      
      tokens <- strsplit(
        trimws(cleaned),
        "[[:space:]]+"
      )[[1]]
      
      tokens <- tokens[
        nzchar(tokens) &
          nchar(tokens) == 4
      ]
      
      if (!length(tokens)) {
        next
      }
      
      candidates <- suppressWarnings(
        as.integer(tokens)
      )
      
      candidates <- candidates[
        !is.na(candidates) &
          candidates >= year_min &
          candidates <= year_max
      ]
      
      if (length(candidates)) {
        out[ii] <- candidates[1]
      }
    }
  }
  
  out
}


stats_row <- function(x) {
  
  x <- x[
    is.finite(x)
  ]
  
  data.frame(
    n = length(x),
    mean = mean(x),
    median = median(x),
    sd = if (length(x) > 1) sd(x) else NA_real_,
    min = min(x),
    max = max(x)
  )
}


fmt <- function(
    x,
    digits = 6
) {
  
  ifelse(
    is.finite(x),
    sprintf(
      paste0(
        "%.",
        digits,
        "f"
      ),
      x
    ),
    "NA"
  )
}

# =============================================================================
# 3. READ SCRIPT 06 OUTPUT
# =============================================================================

trade <- read.csv(
  file_trade,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

required_trade_cols <- c(
  "year",
  "gdp_current",
  "cf_y",
  "z_y"
)

if (
  !all(
    required_trade_cols %in%
    names(trade)
  )
) {
  
  stop(
    "Script 06 output is missing required column(s): ",
    paste(
      setdiff(
        required_trade_cols,
        names(trade)
      ),
      collapse = ", "
    )
  )
}

trade$year <- as.integer(
  trade$year
)

trade <- trade[
  trade$year >= 1967 &
    trade$year <= 1972,
  ,
  drop = FALSE
]

if (
  !all(
    1967:1972 %in%
    trade$year
  )
) {
  
  stop(
    "Expected Script 06 observations for every year 1967-1972."
  )
}

# =============================================================================
# 4. EXTRACT HOUSEHOLD / PRIVATE CONSUMPTION FROM IBGE I.2.1
# =============================================================================
#
# IMPORTANT:
# We DO NOT search for GDP here anymore.
#
# GDP comes from Script 06.
#
# We only need the household-consumption numerator so that:
#
#   c_y = household consumption / GDP
#
# Then:
#
#   alpha = cf_y / c_y

sheet_na <- "I.2.1"

if (
  !(sheet_na %in%
    readxl::excel_sheets(file_na))
) {
  
  stop(
    "Sheet I.2.1 not found in ",
    basename(file_na)
  )
}

na_raw <- as.data.frame(
  readxl::read_excel(
    file_na,
    sheet = sheet_na,
    col_names = FALSE,
    .name_repair = "minimal"
  ),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Detect year row
# ---------------------------------------------------------------------------

year_count_by_row <- vapply(
  seq_len(nrow(na_raw)),
  function(i) {
    
    yy <- as_year_flexible(
      unlist(
        na_raw[
          i,
          ,
          drop = FALSE
        ],
        use.names = FALSE
      ),
      year_min = 1947,
      year_max = 2000
    )
    
    sum(!is.na(yy))
  },
  numeric(1)
)

year_row <- which.max(
  year_count_by_row
)

if (
  !length(year_row) ||
  year_count_by_row[year_row] < 10
) {
  
  stop(
    "Could not identify the year-header row in I.2.1."
  )
}

year_vector <- as_year_flexible(
  unlist(
    na_raw[
      year_row,
      ,
      drop = FALSE
    ],
    use.names = FALSE
  ),
  year_min = 1947,
  year_max = 2000
)

year_cols <- which(
  !is.na(year_vector)
)

if (!length(year_cols)) {
  
  stop(
    "Year row was found but no usable year columns were detected."
  )
}

first_year_col <- min(
  year_cols
)

# ---------------------------------------------------------------------------
# Create one normalized label per row
# ---------------------------------------------------------------------------

row_labels <- vapply(
  seq_len(nrow(na_raw)),
  function(i) {
    
    if (first_year_col <= 1) {
      return("")
    }
    
    txt <- norm_text(
      unlist(
        na_raw[
          i,
          seq_len(first_year_col - 1),
          drop = FALSE
        ],
        use.names = FALSE
      )
    )
    
    txt <- txt[
      nzchar(txt)
    ]
    
    paste(
      unique(txt),
      collapse = " | "
    )
  },
  character(1)
)

# ---------------------------------------------------------------------------
# Candidate household-consumption rows
# ---------------------------------------------------------------------------
#
# We deliberately search only household/private concepts.
# Government consumption is excluded.
#
# Preference order:
#   1. consumo final das familias
#   2. consumo pessoal
#   3. despesa de consumo das familias
#   4. consumo das familias
#
# If more than one row matches, the script chooses the row whose 1967-1972
# consumption/GDP ratios are complete and economically plausible.

consumption_patterns <- c(
  "^consumo final das familias$",
  "consumo final das familias",
  "^consumo pessoal$",
  "consumo pessoal",
  "despesa de consumo das familias",
  "consumo das familias"
)

candidate_rows <- integer(0)

for (p in consumption_patterns) {
  
  candidate_rows <- union(
    candidate_rows,
    which(
      grepl(
        p,
        row_labels
      )
    )
  )
}

# Explicitly remove government/public-consumption rows.
candidate_rows <- candidate_rows[
  !grepl(
    "governo|administracoes publicas|publica",
    row_labels[candidate_rows]
  )
]

if (!length(candidate_rows)) {
  
  cat(
    "\nRows containing the word 'consumo' in I.2.1:\n"
  )
  
  consumo_rows <- which(
    grepl(
      "consumo",
      row_labels
    )
  )
  
  for (j in consumo_rows) {
    
    cat(
      sprintf(
        "[%02d] %s\n",
        j,
        row_labels[j]
      )
    )
  }
  
  stop(
    "Could not identify a household/private consumption row in I.2.1."
  )
}

# ---------------------------------------------------------------------------
# Evaluate every candidate using GDP already extracted by Script 06
# ---------------------------------------------------------------------------

extract_values_for_row <- function(
    row_id,
    years_needed
) {
  
  out <- rep(
    NA_real_,
    length(years_needed)
  )
  
  for (k in seq_along(years_needed)) {
    
    y <- years_needed[k]
    
    cols <- which(
      year_vector == y
    )
    
    if (length(cols) != 1) {
      
      stop(
        "Expected exactly one column for year ",
        y,
        " in I.2.1."
      )
    }
    
    out[k] <- as_num(
      na_raw[
        row_id,
        cols
      ][[1]]
    )
  }
  
  out
}


gdp_reference <- trade[
  match(
    1967:1972,
    trade$year
  ),
  "gdp_current"
]


candidate_diagnostics <- data.frame(
  row = candidate_rows,
  label = row_labels[candidate_rows],
  complete_n = NA_integer_,
  mean_c_y = NA_real_,
  min_c_y = NA_real_,
  max_c_y = NA_real_,
  plausible = FALSE,
  stringsAsFactors = FALSE
)


candidate_values <- list()


for (j in seq_along(candidate_rows)) {
  
  r <- candidate_rows[j]
  
  values <- extract_values_for_row(
    r,
    1967:1972
  )
  
  c_y_candidate <- values /
    gdp_reference
  
  candidate_values[[j]] <-
    values
  
  candidate_diagnostics$complete_n[j] <-
    sum(
      is.finite(
        c_y_candidate
      )
    )
  
  candidate_diagnostics$mean_c_y[j] <-
    if (
      any(
        is.finite(
          c_y_candidate
        )
      )
    ) {
      mean(
        c_y_candidate,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  
  candidate_diagnostics$min_c_y[j] <-
    if (
      any(
        is.finite(
          c_y_candidate
        )
      )
    ) {
      min(
        c_y_candidate,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  
  candidate_diagnostics$max_c_y[j] <-
    if (
      any(
        is.finite(
          c_y_candidate
        )
      )
    ) {
      max(
        c_y_candidate,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  
  # Broad plausibility filter.
  # Historical private household consumption should be positive and a
  # substantial, but not impossible, share of GDP.
  candidate_diagnostics$plausible[j] <-
    candidate_diagnostics$complete_n[j] == 6 &&
    is.finite(
      candidate_diagnostics$mean_c_y[j]
    ) &&
    candidate_diagnostics$min_c_y[j] > 0.30 &&
    candidate_diagnostics$max_c_y[j] < 0.95
}


plausible_idx <- which(
  candidate_diagnostics$plausible
)


if (!length(plausible_idx)) {
  
  cat(
    "\nHousehold-consumption candidate diagnostics:\n\n"
  )
  
  print(
    candidate_diagnostics,
    row.names = FALSE
  )
  
  stop(
    paste(
      "Consumption rows were found, but none produced",
      "plausible complete consumption/GDP ratios."
    )
  )
}


# ---------------------------------------------------------------------------
# If several plausible rows remain, use a deterministic priority ranking
# based on the economic label.
# ---------------------------------------------------------------------------

label_priority <- function(label) {
  
  if (
    grepl(
      "^consumo final das familias$",
      label
    )
  ) {
    return(1)
  }
  
  if (
    grepl(
      "consumo final das familias",
      label
    )
  ) {
    return(2)
  }
  
  if (
    grepl(
      "^consumo pessoal$",
      label
    )
  ) {
    return(3)
  }
  
  if (
    grepl(
      "consumo pessoal",
      label
    )
  ) {
    return(4)
  }
  
  if (
    grepl(
      "despesa de consumo das familias",
      label
    )
  ) {
    return(5)
  }
  
  if (
    grepl(
      "consumo das familias",
      label
    )
  ) {
    return(6)
  }
  
  99
}


candidate_diagnostics$priority <- vapply(
  candidate_diagnostics$label,
  label_priority,
  numeric(1)
)


plausible_table <- candidate_diagnostics[
  plausible_idx,
  ,
  drop = FALSE
]


best_priority <- min(
  plausible_table$priority
)


best <- plausible_table[
  plausible_table$priority ==
    best_priority,
  ,
  drop = FALSE
]


if (
  nrow(best) != 1
) {
  
  cat(
    "\nMore than one plausible household-consumption row remains:\n\n"
  )
  
  print(
    plausible_table,
    row.names = FALSE
  )
  
  stop(
    "Could not choose a unique household-consumption row."
  )
}


consumption_row <- best$row[1]


personal_consumption <- extract_values_for_row(
  consumption_row,
  1967:1972
)


na_consumption <- data.frame(
  
  year = 1967:1972,
  
  gdp_current =
    gdp_reference,
  
  personal_consumption_current =
    personal_consumption,
  
  stringsAsFactors = FALSE
)


na_consumption$c_y <-
  na_consumption$personal_consumption_current /
  na_consumption$gdp_current


write.csv(
  na_consumption,
  file.path(
    processed_dir,
    "brazil_private_consumption_share_1967_1972.csv"
  ),
  row.names = FALSE
)


cat(
  "\n============================================================\n"
)

cat(
  "NATIONAL-ACCOUNTS CONSUMPTION EXTRACTION\n"
)

cat(
  "============================================================\n"
)

cat(
  "GDP source: reused validated gdp_current from Script 06\n"
)

cat(
  "Matched consumption row: ",
  consumption_row,
  " -> ",
  row_labels[consumption_row],
  "\n\n",
  sep = ""
)


if (
  nrow(candidate_diagnostics) > 1
) {
  
  cat(
    "Candidate-row diagnostics used for selection:\n\n"
  )
  
  print(
    candidate_diagnostics,
    row.names = FALSE
  )
  
  cat("\n")
}


print(
  na_consumption,
  row.names = FALSE
)

# =============================================================================
# 5. LABOR-SHARE DATA FOR mu
# =============================================================================
#
# Source:
# Miebach & Marquetti (2022), Nova Economia 32(3).
#
# The transcription is stored as a versioned raw CSV rather than embedded in
# R vectors so the historical transcription can be audited independently:
#
#   data/raw/miebach_marquetti_2022_labor_share_1967_1972_v1.csv
#
# employee_comp_share_pc:
#   Table 10, "Parc. dos salários (pc)"
#
# labor_share_adjusted:
#   Table 12, "Parcela salarial"
#
# Baseline uses the adjusted labor share because the model labor input N is
# intended to represent total labor services, including the labor component
# of mixed/self-employment income.

labor_source_file <- file.path(
  raw_dir,
  "miebach_marquetti_2022_labor_share_1967_1972_v1.csv"
)

if (!file.exists(labor_source_file)) {
  stop(
    "Missing versioned Miebach-Marquetti labor-share transcription:\n",
    labor_source_file
  )
}

labor_source <- read.csv(
  labor_source_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_labor_columns <- c(
  "year",
  "employee_comp_share_pc",
  "labor_share_adjusted"
)

missing_labor_columns <- setdiff(
  required_labor_columns,
  names(labor_source)
)

if (length(missing_labor_columns) > 0) {
  stop(
    "Labor-share raw CSV is missing required column(s): ",
    paste(missing_labor_columns, collapse = ", ")
  )
}

labor_source$year <- as.integer(labor_source$year)
labor_source$employee_comp_share_pc <- suppressWarnings(
  as.numeric(labor_source$employee_comp_share_pc)
)
labor_source$labor_share_adjusted <- suppressWarnings(
  as.numeric(labor_source$labor_share_adjusted)
)

if (!identical(sort(labor_source$year), 1967:1972)) {
  stop(
    "Miebach-Marquetti raw CSV must contain exactly years 1967-1972."
  )
}

if (
  any(!is.finite(labor_source$employee_comp_share_pc)) ||
  any(!is.finite(labor_source$labor_share_adjusted))
) {
  stop("Non-finite labor-share observations found in versioned raw CSV.")
}

write.csv(
  labor_source,
  file.path(
    processed_dir,
    "brazil_labor_share_miebach_marquetti_1967_1972.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 6. MERGE INPUTS
# =============================================================================

annual <- merge(
  trade,
  na_consumption[
    ,
    c(
      "year",
      "c_y"
    )
  ],
  by = "year",
  all = FALSE
)

annual <- merge(
  annual,
  labor_source,
  by = "year",
  all = FALSE
)

annual <- annual[
  order(
    annual$year
  ),
]

if (
  !all(
    1967:1972 %in%
    annual$year
  )
) {
  
  stop(
    "Merged data do not contain every year 1967-1972."
  )
}

# =============================================================================
# 7. CALIBRATE alpha
# =============================================================================

annual$alpha <-
  annual$cf_y /
  annual$c_y


if (
  any(
    annual$alpha <= 0 |
    annual$alpha >= 1
  )
) {
  
  stop(
    "Calculated alpha is outside (0,1). Check the consumption mapping."
  )
}

# =============================================================================
# 8. CALIBRATE mu
# =============================================================================
#
# Production:
#
#   Y = A*N^(1-mu)*Z^mu
#
# Cost minimization:
#
#   mu = P_Z Z / (W N + P_Z Z)
#
# Because both z_y and labor share use GDP as denominator:
#
#   mu = z_y / (labor_share + z_y)

annual$mu_adjusted <- with(
  annual,
  z_y /
    (
      labor_share_adjusted +
        z_y
    )
)


annual$mu_employee_only <- with(
  annual,
  z_y /
    (
      employee_comp_share_pc +
        z_y
    )
)


if (
  any(
    annual$mu_adjusted <= 0 |
    annual$mu_adjusted >= 1
  )
) {
  
  stop(
    "Calculated baseline mu is outside (0,1)."
  )
}

# =============================================================================
# 9. DERIVE lambda_s
# =============================================================================

annual$lambda_s_annual <- with(
  annual,
  mu_adjusted +
    (
      1 -
        mu_adjusted
    ) *
    alpha
)


write.csv(
  annual,
  file.path(
    processed_dir,
    "alpha_mu_lambda_s_annual_1967_1972.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 10. BASELINE 1968-1972
# =============================================================================

base <- annual[
  annual$year >= 1968 &
    annual$year <= 1972,
  ,
  drop = FALSE
]

if (
  nrow(base) != 5
) {
  
  stop(
    "Expected five baseline observations for 1968-1972."
  )
}


alpha_stats <- stats_row(
  base$alpha
)

mu_stats <- stats_row(
  base$mu_adjusted
)


alpha_final <-
  alpha_stats$mean

mu_final <-
  mu_stats$mean


# Exact model identity using chosen baseline alpha and mu.
lambda_s_final <-
  mu_final +
  (
    1 -
      mu_final
  ) *
  alpha_final


parameter_candidates <- data.frame(
  
  parameter = c(
    "alpha",
    "mu",
    "lambda_s"
  ),
  
  value = c(
    alpha_final,
    mu_final,
    lambda_s_final
  ),
  
  classification = c(
    "Calibrated",
    "Calibrated",
    "Derived"
  ),
  
  sample = c(
    "1968-1972",
    "1968-1972",
    "Derived from baseline alpha and mu"
  ),
  
  method = c(
    
    paste(
      "Mean annual imported-consumption share:",
      "alpha_t = cf_y_t / c_y_t"
    ),
    
    paste(
      "Mean annual imported-input variable-cost share:",
      "mu_t = z_y_t / (adjusted labor share_t + z_y_t)"
    ),
    
    "lambda_s = mu + (1-mu)*alpha"
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  parameter_candidates,
  file.path(
    estimates_dir,
    "alpha_mu_lambda_s_parameters.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 11. ROBUSTNESS
# =============================================================================

all_67_72 <- annual[
  annual$year >= 1967 &
    annual$year <= 1972,
  ,
  drop = FALSE
]


alpha_67_72 <-
  mean(
    all_67_72$alpha
  )

mu_67_72 <-
  mean(
    all_67_72$mu_adjusted
  )

lambda_67_72 <-
  mu_67_72 +
  (
    1 -
      mu_67_72
  ) *
  alpha_67_72


alpha_median <-
  median(
    base$alpha
  )

mu_median <-
  median(
    base$mu_adjusted
  )

lambda_median <-
  mu_median +
  (
    1 -
      mu_median
  ) *
  alpha_median


mu_employee_baseline <-
  mean(
    base$mu_employee_only
  )

lambda_employee_mu <-
  mu_employee_baseline +
  (
    1 -
      mu_employee_baseline
  ) *
  alpha_final


mu_ratio_of_means <-
  mean(
    base$z_y
  ) /
  (
    mean(
      base$z_y
    ) +
      mean(
        base$labor_share_adjusted
      )
  )


robustness <- data.frame(
  
  specification = c(
    "Baseline 1968-1972 mean annual ratios",
    "Include 1967",
    "1968-1972 medians",
    "Employee compensation only for mu",
    "mu ratio-of-means diagnostic"
  ),
  
  alpha = c(
    alpha_final,
    alpha_67_72,
    alpha_median,
    alpha_final,
    alpha_final
  ),
  
  mu = c(
    mu_final,
    mu_67_72,
    mu_median,
    mu_employee_baseline,
    mu_ratio_of_means
  ),
  
  lambda_s = c(
    lambda_s_final,
    lambda_67_72,
    lambda_median,
    lambda_employee_mu,
    mu_ratio_of_means +
      (
        1 -
          mu_ratio_of_means
      ) *
      alpha_final
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  robustness,
  file.path(
    estimates_dir,
    "alpha_mu_lambda_s_robustness.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 12. DESCRIPTIVE STATISTICS
# =============================================================================

descriptive <- rbind(
  
  cbind(
    parameter = "alpha",
    stats_row(
      base$alpha
    )
  ),
  
  cbind(
    parameter = "mu_adjusted",
    stats_row(
      base$mu_adjusted
    )
  ),
  
  cbind(
    parameter = "mu_employee_only",
    stats_row(
      base$mu_employee_only
    )
  ),
  
  cbind(
    parameter = "lambda_s_annual_diagnostic",
    stats_row(
      base$lambda_s_annual
    )
  )
)


write.csv(
  descriptive,
  file.path(
    estimates_dir,
    "alpha_mu_lambda_s_descriptive_statistics.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 13. DYNARE CANDIDATE FILE
# =============================================================================

dynare_lines <- c(
  
  paste0(
    "alpha = ",
    sprintf(
      "%.6f",
      alpha_final
    ),
    ";"
  ),
  
  paste0(
    "mu = ",
    sprintf(
      "%.6f",
      mu_final
    ),
    ";"
  ),
  
  paste0(
    "lambda_s = ",
    sprintf(
      "%.6f",
      lambda_s_final
    ),
    ";"
  )
)


writeLines(
  dynare_lines,
  file.path(
    estimates_dir,
    "alpha_mu_lambda_s_dynare.txt"
  )
)

# =============================================================================
# 14. PARAMETER-OUTPUT ARCHITECTURE
# =============================================================================
#
# Script 07 does NOT write docs/parameter_registry.csv.
# Its machine-readable parameter outputs remain in output/estimates/.
# Script 99 is the sole registry writer.

# =============================================================================
# 15. DOCUMENTATION
# =============================================================================

doc <- c(
  
  "# Structural openness parameters: alpha, mu and lambda_s",
  
  "",
  
  "## Baseline sample",
  
  "1968-1972.",
  
  "",
  
  "## alpha",
  
  "`alpha_t = cf_y_t / c_y_t`",
  
  paste0(
    "Baseline alpha = ",
    fmt(alpha_final),
    "."
  ),
  
  "",
  
  "## mu",
  
  "`mu_t = z_y_t / (labor_share_t + z_y_t)`",
  
  paste0(
    "Baseline mu = ",
    fmt(mu_final),
    "."
  ),
  
  paste0(
    "Employee-compensation-only robustness mu = ",
    fmt(mu_employee_baseline),
    "."
  ),
  
  "",
  
  "## lambda_s",
  
  "`lambda_s = mu + (1-mu)*alpha`",
  
  paste0(
    "Baseline lambda_s = ",
    fmt(lambda_s_final),
    "."
  ),
  
  "",
  
  paste(
    "GDP is reused from Script 06 rather than re-extracted in Script 07.",
    "This avoids ambiguity from multiple GDP-labelled rows in the historical",
    "IBGE workbook."
  ),
  
  ""
)


writeLines(
  doc,
  file.path(
    docs_dir,
    "parameter_alpha_mu_lambda_s.md"
  ),
  useBytes = TRUE
)

# =============================================================================
# 16. FIGURE
# =============================================================================

png(
  file.path(
    figures_dir,
    "alpha_mu_1967_1972.png"
  ),
  width = 1400,
  height = 900,
  res = 150
)


matplot(
  annual$year,
  100 *
    cbind(
      annual$alpha,
      annual$mu_adjusted,
      annual$mu_employee_only
    ),
  type = "o",
  pch = c(
    16,
    17,
    1
  ),
  lty = c(
    1,
    1,
    2
  ),
  xlab = "Year",
  ylab = "Parameter value (%)",
  main =
    "Brazil: alpha and mu, 1967-1972"
)


legend(
  "topright",
  legend = c(
    "alpha",
    "mu: adjusted labor share",
    "mu: employee compensation only"
  ),
  pch = c(
    16,
    17,
    1
  ),
  lty = c(
    1,
    1,
    2
  ),
  bty = "n"
)


dev.off()

# =============================================================================
# 17. CONSOLE REPORT
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "SCRIPT 07: alpha, mu AND lambda_s\n"
)

cat(
  "============================================================\n"
)

cat(
  "Baseline sample: 1968-1972\n"
)

cat(
  "GDP: reused from validated Script 06 output\n"
)

cat(
  "Consumption: IBGE I.2.1 household/private consumption\n"
)

cat(
  "Labor share: Miebach & Marquetti (2022), versioned raw CSV v1\n"
)

cat(
  "lambda_s: exact model identity\n"
)


cat(
  "\n============================================================\n"
)

cat(
  "ANNUAL CALIBRATION TABLE\n"
)

cat(
  "============================================================\n"
)


print(
  annual[
    ,
    c(
      "year",
      "cf_y",
      "c_y",
      "alpha",
      "z_y",
      "labor_share_adjusted",
      "employee_comp_share_pc",
      "mu_adjusted",
      "mu_employee_only",
      "lambda_s_annual"
    )
  ],
  row.names = FALSE
)


cat(
  "\n============================================================\n"
)

cat(
  "BASELINE PARAMETER CANDIDATES\n"
)

cat(
  "============================================================\n"
)


print(
  parameter_candidates,
  row.names = FALSE
)


cat(
  "\nDescriptive statistics, 1968-1972:\n\n"
)

print(
  descriptive,
  row.names = FALSE
)


cat(
  "\n============================================================\n"
)

cat(
  "ROBUSTNESS\n"
)

cat(
  "============================================================\n"
)


print(
  robustness,
  row.names = FALSE
)


cat(
  "\nDynare candidate values:\n\n"
)

cat(
  paste(
    dynare_lines,
    collapse = "\n"
  ),
  "\n"
)


cat(
  "\nIMPORTANT\n"
)

cat(
  paste(
    "Do not update the Dynare .mod file yet.",
    "Review the annual table and robustness results first.\n"
  )
)

# =============================================================================
# 03_brazil_inflation_steady_state.R
#
# Purpose:
#   Construct quarterly Brazilian IGPC inflation and calibrate the steady-state
#   quarterly gross inflation factor Pi_ss.
#
# Model parameters:
#   Pi_ss = steady-state quarterly gross inflation factor
#   invPi = 1 / Pi_ss
#
# Baseline:
#   1968Q1-1972Q4
#
# Robustness:
#   1967Q1-1972Q4
#   1964Q1-1972Q4
#
# Important pipeline rule:
#   This script DOES NOT write docs/parameter_registry.csv.
#   Script 99 is the sole registry writer.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Paths
# -----------------------------------------------------------------------------

raw_file <- "data/raw/IGPC_ipeadata.csv"

processed_dir <- "data/processed"
estimate_dir <- "output/estimates"
figure_dir <- "output/figures"
docs_dir <- "docs"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(estimate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

processed_file <- file.path(
  processed_dir,
  "brazil_igpc_quarterly.csv"
)

estimate_file <- file.path(
  estimate_dir,
  "Pi_ss_calibration.csv"
)

figure_file <- file.path(
  figure_dir,
  "brazil_igpc_quarterly_inflation.png"
)

parameter_note_file <- file.path(
  docs_dir,
  "parameter_Pi_ss.md"
)


# -----------------------------------------------------------------------------
# 1. Check raw input
# -----------------------------------------------------------------------------

if (!file.exists(raw_file)) {
  stop(
    paste(
      "Required raw file not found:",
      raw_file
    )
  )
}


# -----------------------------------------------------------------------------
# 2. Read raw IGPC data
# -----------------------------------------------------------------------------

raw <- read.csv(
  raw_file,
  stringsAsFactors = FALSE
)

names(raw) <- trimws(
  names(raw)
)

cat("\nRaw IGPC column names detected:\n")
print(
  names(raw)
)


# -----------------------------------------------------------------------------
# 3. Identify and standardize IPEA columns
#
# IMPORTANT:
# The original IPEA file uses:
#
#   Data
#
# for the date column, while the IGPC column has a very long descriptive
# header that read.csv() sanitizes into a long name containing "IGPC" and
# the IPEA code GAMMA12_IGPCMTB12.
#
# We therefore identify both columns robustly and rename them internally.
# -----------------------------------------------------------------------------


# ---- 3.1 Date column ----------------------------------------------------------

date_candidates <- c(
  "date_raw",
  "date",
  "Date",
  "DATE",
  "Data",
  "DATA"
)

date_matches <- which(
  names(raw) %in%
    date_candidates
)

if (length(date_matches) == 0) {
  
  date_matches <- grep(
    "^data$|^date$|date_raw",
    names(raw),
    ignore.case = TRUE
  )
}

if (length(date_matches) == 0) {
  stop(
    paste(
      "Could not identify the date column in",
      raw_file,
      "\nColumns found:",
      paste(
        names(raw),
        collapse = " | "
      )
    )
  )
}

date_col_index <- date_matches[1]


# ---- 3.2 IGPC column ----------------------------------------------------------

# First try simple names.

igpc_candidates <- c(
  "igpc",
  "IGPC",
  "Igpc",
  "value",
  "Value",
  "VALUE"
)

igpc_matches <- which(
  names(raw) %in%
    igpc_candidates
)

# If not found, use the old IPEA-header solution:
# locate the long descriptive column by IGPC series name/code.

if (length(igpc_matches) == 0) {
  
  igpc_matches <- grep(
    "GAMMA12_IGPCMTB12|IGPC",
    names(raw),
    ignore.case = TRUE
  )
}

if (length(igpc_matches) == 0) {
  stop(
    paste(
      "Could not identify the IGPC column in",
      raw_file,
      "\nColumns found:",
      paste(
        names(raw),
        collapse = " | "
      )
    )
  )
}

igpc_col_index <- igpc_matches[1]


# ---- 3.3 Standardize names ----------------------------------------------------

names(raw)[date_col_index] <- "date_raw"
names(raw)[igpc_col_index] <- "igpc"

cat("\nStandardized columns:\n")
cat(
  "Date column -> date_raw\n"
)
cat(
  "IGPC column -> igpc\n"
)


# -----------------------------------------------------------------------------
# 4. Parse raw values
# -----------------------------------------------------------------------------

raw$date_raw_original <- raw$date_raw

# IMPORTANT:
# IGPC is an INDEX LEVEL.
#
# Examples from the original file include values such as:
#
#   984.300
#   1,021.400
#
# The comma is a thousands separator.
# The period is the decimal separator.
#
# Therefore:
#
#   "1,021.400" -> 1021.400
#
# We REMOVE commas only.
#
# Do NOT replace "." with "".
# Do NOT replace "," with ".".

raw$igpc <- as.numeric(
  gsub(
    ",",
    "",
    as.character(raw$igpc)
  )
)

if (anyNA(raw$igpc)) {
  
  bad_values <- unique(
    raw$igpc[
      is.na(raw$igpc)
    ]
  )
  
  stop(
    paste(
      "IGPC conversion produced missing values.",
      "Check the raw IGPC column."
    )
  )
}


# -----------------------------------------------------------------------------
# 5. Parse IPEA YYYY.MM date format
# -----------------------------------------------------------------------------

date_numeric <- as.numeric(
  as.character(
    raw$date_raw
  )
)

if (anyNA(date_numeric)) {
  stop(
    "Date conversion produced missing values."
  )
}

date_text <- sprintf(
  "%.2f",
  date_numeric
)

year <- as.integer(
  substr(
    date_text,
    1,
    4
  )
)

month <- as.integer(
  substr(
    date_text,
    6,
    7
  )
)

if (
  any(
    month < 1 |
    month > 12
  )
) {
  stop(
    "Invalid month detected after parsing YYYY.MM dates."
  )
}

raw$date <- as.Date(
  sprintf(
    "%04d-%02d-01",
    year,
    month
  )
)

raw$year <- year
raw$month <- month

raw$quarter_number <-
  ((month - 1) %/% 3) + 1

raw$quarter <- paste0(
  raw$year,
  "Q",
  raw$quarter_number
)

raw <- raw[
  order(raw$date),
]


# -----------------------------------------------------------------------------
# 6. Basic raw-data validation
# -----------------------------------------------------------------------------

if (
  anyDuplicated(
    raw$date
  )
) {
  stop(
    "Duplicate monthly dates found in IGPC raw data."
  )
}

if (
  any(
    !is.finite(
      raw$igpc
    )
  )
) {
  stop(
    "Non-finite IGPC index values detected."
  )
}

if (
  any(
    raw$igpc <= 0
  )
) {
  stop(
    "IGPC index must be strictly positive."
  )
}


# -----------------------------------------------------------------------------
# 7. Monthly gross inflation
# -----------------------------------------------------------------------------

raw$gross_monthly <- NA_real_

if (
  nrow(raw) >= 2
) {
  
  raw$gross_monthly[
    2:nrow(raw)
  ] <-
    raw$igpc[
      2:nrow(raw)
    ] /
    raw$igpc[
      1:(nrow(raw) - 1)
    ]
}


# -----------------------------------------------------------------------------
# 8. Construct quarterly inflation
# -----------------------------------------------------------------------------

quarters <- unique(
  raw$quarter
)

quarterly_list <- vector(
  "list",
  length(quarters)
)

for (
  i in seq_along(quarters)
) {
  
  q <- quarters[i]
  
  qdata <- raw[
    raw$quarter == q,
  ]
  
  qdata <- qdata[
    order(qdata$date),
  ]
  
  q_year <-
    qdata$year[1]
  
  q_number <-
    qdata$quarter_number[1]
  
  first_row_index <- which(
    raw$date ==
      min(qdata$date)
  )[1]
  
  last_row_index <- which(
    raw$date ==
      max(qdata$date)
  )[1]
  
  # For quarters after the first observation:
  #
  # quarterly gross inflation =
  #
  # P(last month of quarter) /
  # P(last month of previous quarter)
  
  if (
    first_row_index > 1
  ) {
    
    previous_index <-
      raw$igpc[
        first_row_index - 1
      ]
    
    gross_quarterly <-
      raw$igpc[
        last_row_index
      ] /
      previous_index
    
  } else {
    
    # First available quarter fallback.
    
    valid_monthly <-
      qdata$gross_monthly[
        is.finite(
          qdata$gross_monthly
        )
      ]
    
    if (
      length(valid_monthly) == 0
    ) {
      
      gross_quarterly <-
        qdata$igpc[
          nrow(qdata)
        ] /
        qdata$igpc[1]
      
    } else {
      
      gross_quarterly <-
        prod(
          valid_monthly
        )
    }
  }
  
  quarterly_list[[i]] <- data.frame(
    quarter = q,
    year = q_year,
    quarter_number = q_number,
    igpc_end = qdata$igpc[
      nrow(qdata)
    ],
    gross_quarterly = gross_quarterly,
    inflation_quarterly =
      gross_quarterly - 1,
    n_months = nrow(qdata),
    stringsAsFactors = FALSE
  )
}


quarterly <- do.call(
  rbind,
  quarterly_list
)

quarterly <- quarterly[
  order(
    quarterly$year,
    quarterly$quarter_number
  ),
]


# -----------------------------------------------------------------------------
# 9. Validate quarterly data
# -----------------------------------------------------------------------------

if (
  anyDuplicated(
    quarterly$quarter
  )
) {
  stop(
    "Duplicate quarter identifiers found."
  )
}

if (
  anyNA(
    quarterly$inflation_quarterly
  )
) {
  stop(
    "Quarterly inflation contains missing values."
  )
}

if (
  any(
    quarterly$gross_quarterly <= 0
  )
) {
  stop(
    "Quarterly gross inflation must be positive."
  )
}


# -----------------------------------------------------------------------------
# 10. Known data validation point
#
# 1977Q2:
#
#   June 1977 IGPC / March 1977 IGPC
#
#   1086.1 / 984.3 - 1
# -----------------------------------------------------------------------------

validation_row <- quarterly[
  quarterly$quarter ==
    "1977Q2",
]

if (
  nrow(validation_row) == 1
) {
  
  expected_1977Q2 <-
    1086.1 /
    984.3 -
    1
  
  difference <-
    abs(
      validation_row$inflation_quarterly -
        expected_1977Q2
    )
  
  if (
    difference > 1e-8
  ) {
    
    warning(
      paste(
        "1977Q2 validation differs from expected value.",
        "Calculated:",
        validation_row$inflation_quarterly,
        "Expected:",
        expected_1977Q2
      )
    )
  }
}


# -----------------------------------------------------------------------------
# 11. Save processed quarterly dataset
# -----------------------------------------------------------------------------

write.csv(
  quarterly,
  processed_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 12. Function to calibrate Pi_ss
# -----------------------------------------------------------------------------

calibrate_pi_ss <- function(
    data,
    start_year,
    start_quarter,
    end_year,
    end_quarter,
    sample_label,
    recommended_baseline = "NO"
) {
  
  start_index <-
    start_year * 4 +
    start_quarter
  
  end_index <-
    end_year * 4 +
    end_quarter
  
  data_index <-
    data$year * 4 +
    data$quarter_number
  
  sample_data <- data[
    data_index >= start_index &
      data_index <= end_index,
  ]
  
  if (
    nrow(sample_data) == 0
  ) {
    stop(
      paste(
        "No observations found for sample:",
        sample_label
      )
    )
  }
  
  if (
    any(
      !is.finite(
        sample_data$gross_quarterly
      )
    )
  ) {
    stop(
      paste(
        "Non-finite gross inflation in sample:",
        sample_label
      )
    )
  }
  
  if (
    any(
      sample_data$gross_quarterly <= 0
    )
  ) {
    stop(
      paste(
        "Non-positive gross inflation in sample:",
        sample_label
      )
    )
  }
  
  # Geometric mean of quarterly gross inflation.
  
  Pi_ss <- exp(
    mean(
      log(
        sample_data$gross_quarterly
      )
    )
  )
  
  invPi <- 1 / Pi_ss
  
  quarterly_inflation <-
    Pi_ss - 1
  
  annual_inflation <-
    Pi_ss^4 - 1
  
  data.frame(
    sample = sample_label,
    n_quarters = nrow(sample_data),
    Pi_ss = Pi_ss,
    quarterly_inflation =
      quarterly_inflation,
    annual_inflation =
      annual_inflation,
    invPi = invPi,
    recommended_baseline =
      recommended_baseline,
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# 13. Baseline and robustness calibration
# -----------------------------------------------------------------------------

baseline <- calibrate_pi_ss(
  data = quarterly,
  start_year = 1968,
  start_quarter = 1,
  end_year = 1972,
  end_quarter = 4,
  sample_label =
    "1968Q1-1972Q4",
  recommended_baseline =
    "YES"
)


robustness_1967 <- calibrate_pi_ss(
  data = quarterly,
  start_year = 1967,
  start_quarter = 1,
  end_year = 1972,
  end_quarter = 4,
  sample_label =
    "1967Q1-1972Q4",
  recommended_baseline =
    "NO"
)


robustness_1964 <- calibrate_pi_ss(
  data = quarterly,
  start_year = 1964,
  start_quarter = 1,
  end_year = 1972,
  end_quarter = 4,
  sample_label =
    "1964Q1-1972Q4",
  recommended_baseline =
    "NO"
)


descriptive_long <- calibrate_pi_ss(
  data = quarterly,
  start_year = 1948,
  start_quarter = 1,
  end_year = 1972,
  end_quarter = 4,
  sample_label =
    "1948Q1-1972Q4",
  recommended_baseline =
    "NO"
)


calibration_results <- rbind(
  baseline,
  robustness_1967,
  robustness_1964,
  descriptive_long
)

write.csv(
  calibration_results,
  estimate_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 14. Extract baseline model parameters
# -----------------------------------------------------------------------------

Pi_ss <-
  baseline$Pi_ss[1]

invPi <-
  baseline$invPi[1]


# -----------------------------------------------------------------------------
# 15. Internal consistency checks
# -----------------------------------------------------------------------------

if (
  !is.finite(Pi_ss)
) {
  stop(
    "Pi_ss is not finite."
  )
}

if (
  Pi_ss <= 0
) {
  stop(
    "Pi_ss must be positive."
  )
}

if (
  abs(
    invPi -
    1 / Pi_ss
  ) > 1e-12
) {
  stop(
    "Identity check failed: invPi != 1/Pi_ss."
  )
}

if (
  sum(
    calibration_results$recommended_baseline ==
    "YES"
  ) != 1
) {
  stop(
    "Exactly one Pi_ss baseline must be marked YES."
  )
}


# -----------------------------------------------------------------------------
# 16. Inflation figure
# -----------------------------------------------------------------------------

png(
  filename = figure_file,
  width = 1400,
  height = 800,
  res = 130
)

plot(
  seq_len(
    nrow(quarterly)
  ),
  100 *
    quarterly$inflation_quarterly,
  type = "l",
  xlab = "Quarter",
  ylab = "Quarterly inflation (%)",
  main = "Brazil IGPC Quarterly Inflation"
)

axis_positions <- seq(
  1,
  nrow(quarterly),
  by = 8
)

axis(
  side = 1,
  at = axis_positions,
  labels =
    quarterly$quarter[
      axis_positions
    ],
  las = 2,
  cex.axis = 0.7
)

abline(
  h =
    100 *
    (Pi_ss - 1),
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Observed quarterly inflation",
    "1968Q1-1972Q4 geometric mean"
  ),
  lty = c(
    1,
    2
  ),
  bty = "n"
)

dev.off()


# -----------------------------------------------------------------------------
# 17. Parameter documentation note
# -----------------------------------------------------------------------------

note <- c(
  "# Pi_ss calibration",
  "",
  "## Parameter",
  "",
  "- Dynare/model name: `Pi_ss`",
  "- Derived inverse: `invPi = 1 / Pi_ss`",
  "",
  "## Data",
  "",
  "- Source series: IPEA/FGV IGPC",
  "- IPEA series code: `GAMMA12_IGPCMTB12`",
  paste0(
    "- Raw file: `",
    raw_file,
    "`"
  ),
  paste0(
    "- Processed quarterly file: `",
    processed_file,
    "`"
  ),
  "",
  "## Baseline",
  "",
  "- Sample: 1968Q1-1972Q4",
  "- Pi_ss is the geometric mean of quarterly gross IGPC inflation.",
  "",
  sprintf(
    "- Pi_ss = %.12f",
    Pi_ss
  ),
  sprintf(
    "- invPi = %.12f",
    invPi
  ),
  sprintf(
    "- Quarterly inflation = %.6f%%",
    100 *
      (Pi_ss - 1)
  ),
  sprintf(
    "- Annualized compound inflation = %.6f%%",
    100 *
      (Pi_ss^4 - 1)
  ),
  "",
  "## Robustness",
  "",
  "- 1967Q1-1972Q4",
  "- 1964Q1-1972Q4",
  "",
  "## Pipeline rule",
  "",
  "This script does not write `docs/parameter_registry.csv`.",
  "Script 99 is the sole registry writer."
)

writeLines(
  note,
  parameter_note_file
)


# -----------------------------------------------------------------------------
# 18. Final console output
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("SCRIPT 03 COMPLETE\n")
cat("============================================================\n")

cat(
  "Baseline sample: 1968Q1-1972Q4\n"
)

cat(
  sprintf(
    "Pi_ss  = %.12f\n",
    Pi_ss
  )
)

cat(
  sprintf(
    "invPi  = %.12f\n",
    invPi
  )
)

cat(
  sprintf(
    "Quarterly inflation = %.6f%%\n",
    100 *
      (Pi_ss - 1)
  )
)

cat(
  sprintf(
    "Annual compound inflation = %.6f%%\n",
    100 *
      (Pi_ss^4 - 1)
  )
)

cat("\n")

cat("Outputs:\n")

cat(
  " - ",
  processed_file,
  "\n",
  sep = ""
)

cat(
  " - ",
  estimate_file,
  "\n",
  sep = ""
)

cat(
  " - ",
  figure_file,
  "\n",
  sep = ""
)

cat(
  " - ",
  parameter_note_file,
  "\n",
  sep = ""
)

cat("\n")

cat(
  "Registry NOT modified. Script 99 remains the sole registry writer.\n"
)

cat(
  "============================================================\n"
)
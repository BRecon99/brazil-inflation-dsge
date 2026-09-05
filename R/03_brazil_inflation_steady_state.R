# =============================================================================
# 03_brazil_inflation_steady_state.R
#
# Purpose:
#   Calibrate steady-state quarterly gross inflation Pi_ss
#
# Baseline window:
#   1968Q1-1972Q4
#
# Robustness windows:
#   1967Q1-1972Q4
#   1964Q1-1972Q4
#
# Raw data:
#   data/raw/IPC_FGV.csv
#
# Model parameter:
#   Pi_ss
#
# Derived parameter:
#   invPi = 1/Pi_ss
# =============================================================================

rm(list = ls())

# ---- 0. Verify project root --------------------------------------------------
required_file <- file.path("data", "raw", "IGPC_ipeadata.csv")
if (!file.exists(required_file)) {
  stop(
    "Cannot find data/raw/IGPC_ipeadata.csv. ",
    "Open the project by double-clicking brazil-inflation-dsge.Rproj ",
    "or set the working directory to the repository root."
  )
}
# ---- 1. Read raw monthly data ------------------------------------------------

raw <- file.path("data", "raw", "IGPC_ipeadata.csv")

if (!file.exists(raw)) {
  stop("Cannot find data/raw/IGPC_ipeadata.csv")
}

raw <- read.csv(raw, stringsAsFactors = FALSE)

names(raw)[names(raw) == "Data"] <- "date_raw"
names(raw)[2] <- "igpc"

date_text <- sprintf("%.2f", as.numeric(raw$date_raw))

raw$year <- as.integer(substr(date_text, 1, 4))

raw$month <- as.integer(substr(date_text, 6, 7))

raw$date <- as.Date(
  sprintf(
    "%04d-%02d-01",
    raw$year,
    raw$month
  )
)

raw$qtr <- ((raw$month - 1) %/% 3) + 1

raw <- raw[order(raw$date), ]

# ---- 1.1 Validation ----------------------------------------------------------

if (any(is.na(raw$date))) {
  stop("Date parsing produced missing dates.")
}

if (anyDuplicated(raw$date) > 0) {
  stop("Date parsing produced duplicate dates.")
}

if (any(raw$month < 1 | raw$month > 12)) {
  stop("Invalid month detected.")
}

any(raw$igpc <= 0)

head(raw)

#---- 2. Converting from Index level to monthly Level --------------------------
#
#
# Gross Monthly Inflation = Price in t/Price in t-1

#First we convert to "american numbers" since the Brazilian ones 
# use comma for decimal and dot for thousands.
# For example: 1,200.50 is 1.200,50 in Brazil.

raw$igpc <- as.numeric(
  gsub(",", "", raw$igpc)
)

raw$gross_monthly <- raw$igpc / c(NA, raw$igpc[-nrow(raw)])

raw$inflation_monthly <- raw$gross_monthly - 1

raw$quarter <- paste0(
  raw$year,
  "Q",
  raw$qtr
)

quarterly_gross <- aggregate(
  raw$gross_monthly,
  by = list(quarter = raw$quarter),
  FUN = prod,
  na.rm = TRUE
)

names(quarterly_gross)[2] <- "gross_quarterly"

quarterly_gross$inflation_quarterly <-
  quarterly_gross$gross_quarterly - 1

quarter_counts <- aggregate(
  raw$gross_monthly,
  by = list(quarter = raw$quarter),
  FUN = function(x) sum(!is.na(x))
)

quarterly <- quarterly_gross

quarterly$year <- as.integer(
  substr(quarterly$quarter, 1, 4)
)

quarterly$qtr <- as.integer(
  substr(quarterly$quarter, 6, 6)
)

quarterly <- quarterly[
  order(quarterly$year, quarterly$qtr),
]

write.csv(
  quarterly,
  file.path(
    "data",
    "processed",
    "brazil_igpc_quarterly.csv"
  ),
  row.names = FALSE
)


#
# ---- 3. Steady-state quarterly inflation -------------------------------
#
#The Steady-state inflation will be extracted through a geometric mean instead
# of an arithmetic average, since inflation compounds multiplicatively,
# thus: Pi_ss = (product of Pi_t)^1/T
#
# Equivalently:
# Pi_ss = exp(mean(log(Pi_t)))

# For calibration and rubustness testing we will calculate the steady state
# inflation for different time windows, as the function below:

calibrate_inflation <- function(
    data,
    start_year,
    end_year,
    label
) {
  
  sample <- data[
    data$year >= start_year &
      data$year <= end_year,
  ]
  
  Pi_ss <- exp(
    mean(log(sample$gross_quarterly))
  )
  
  quarterly_inflation <- Pi_ss - 1
  
  annual_inflation <- Pi_ss^4 - 1
  
  invPi <- 1 / Pi_ss
  
  data.frame(
    sample = label,
    n_quarters = nrow(sample),
    Pi_ss = Pi_ss,
    quarterly_inflation = quarterly_inflation,
    annual_inflation = annual_inflation,
    invPi = invPi
  )
}

baseline <- calibrate_inflation(
  quarterly,
  1968,
  1972,
  "1968Q1-1972Q4"
)

robust_1967 <- calibrate_inflation(
  quarterly,
  1967,
  1972,
  "1967Q1-1972Q4"
)

robust_1964 <- calibrate_inflation(
  quarterly,
  1964,
  1972,
  "1964Q1-1972Q4"
)

results <- rbind(
  baseline,
  robust_1967,
  robust_1964
)

# DESCRIPTIVE ONLY — NOT STEADY-STATE CALIBRATION
historical <- calibrate_inflation(
  quarterly,
  1948,
  1972,
  "1948Q1-1972Q4"
)

print(results)

results$recommended_baseline <- c(
  "YES",
  "NO",
  "NO"
)

write.csv(
  results,
  file.path(
    "output",
    "estimates",
    "Pi_ss_calibration.csv"
  ),
  row.names = FALSE
)

# ============================================================================
# UPDATE DATA DICTIONARY
# ============================================================================

dictionary_file <- file.path(
  "docs",
  "data_dictionary.csv"
)

data_dictionary <- read.csv(
  dictionary_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

idx <- which(
  data_dictionary$dataset_name == "brazil_cpi_inflation"
)

if (length(idx) != 1) {
  stop("Could not uniquely identify brazil_cpi_inflation")
}

data_dictionary$series_code[idx] <- "GAMMA12_IGPCMTB12"
data_dictionary$raw_frequency[idx] <- "Monthly"
data_dictionary$raw_units[idx] <- "Index level"
data_dictionary$local_file[idx] <- "data/raw/IGPC_ipeadata.csv"
data_dictionary$status[idx] <- "RAW DATA ACQUIRED"

write.csv(
  data_dictionary,
  dictionary_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================================
# 4. FINALIZE DATA DICTIONARY
# ============================================================================

data_dictionary$description[idx] <-
  paste(
    "IGPC-Mtb - Índice Geral de Preços ao Consumidor.",
    "Historical monthly consumer price index.",
    "Index base: January 1967 = 100."
  )

data_dictionary$transformation[idx] <-
  paste(
    "Monthly gross inflation = P_t / P_(t-1);",
    "quarterly gross inflation obtained by compounding monthly gross inflation;",
    "Pi_ss calibrated using the geometric mean of quarterly gross inflation."
  )

data_dictionary$target_sample[idx] <-
  paste(
    "Full historical series retained;",
    "baseline Pi_ss calibration: 1968Q1-1972Q4;",
    "robustness: 1967Q1-1972Q4 and 1964Q1-1972Q4."
  )

data_dictionary$status[idx] <-
  "PROCESSED - Pi_ss CALIBRATION COMPLETE"

data_dictionary$purpose[idx] <-
  paste(
    "Calibrate steady-state gross quarterly inflation Pi_ss;",
    "derive invPi;",
    "construct Brazilian quarterly inflation series for later DSGE estimation."
  )

write.csv(
  data_dictionary,
  dictionary_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat(
  "\nData dictionary updated successfully.\n"
)


# ============================================================================
# 5. EXTRACT FINAL BASELINE PARAMETER VALUES
# ============================================================================

Pi_ss <- baseline$Pi_ss[1]

invPi <- baseline$invPi[1]

quarterly_inflation_ss <-
  baseline$quarterly_inflation[1]

annual_inflation_ss <-
  baseline$annual_inflation[1]


# ---- 5.1 Validate ------------------------------------------------------------

if (!is.numeric(Pi_ss) || length(Pi_ss) != 1) {
  stop("Pi_ss was not correctly extracted.")
}

if (!is.numeric(invPi) || length(invPi) != 1) {
  stop("invPi was not correctly extracted.")
}

if (abs(invPi - 1 / Pi_ss) > 1e-10) {
  stop("invPi is inconsistent with Pi_ss.")
}

if (Pi_ss <= 0) {
  stop("Pi_ss must be positive.")
}

# ============================================================================
# 6. UPDATE PARAMETER REGISTRY
# ============================================================================

registry_file <- file.path(
  "docs",
  "parameter_registry.csv"
)

registry <- read.csv(
  registry_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Locate parameters exactly as they are named in Dynare
idx_pi <- which(
  registry$dynare_name == "Pi_ss"
)

idx_invpi <- which(
  registry$dynare_name == "invPi"
)

if (length(idx_pi) != 1) {
  stop("Pi_ss was not found uniquely in parameter_registry.csv")
}

if (length(idx_invpi) != 1) {
  stop("invPi was not found uniquely in parameter_registry.csv")
}


# ============================================================================
# 7. UPDATE Pi_ss
# ============================================================================

registry$classification[idx_pi] <- "Calibrated"

registry$research_value[idx_pi] <- Pi_ss

registry$status[idx_pi] <- "CALIBRATION COMPLETE"

registry$method[idx_pi] <-
  "Geometric mean of quarterly gross IGPC-Mtb inflation"

registry$sample[idx_pi] <-
  "1968Q1-1972Q4"

registry$raw_or_primary_data[idx_pi] <-
  "data/raw/IGPC_ipeadata.csv"

registry$source_or_literature[idx_pi] <-
  "IPEAdata / IGPC-Mtb series GAMMA12_IGPCMTB12"

registry$standard_error[idx_pi] <- NA

registry$shock_sd[idx_pi] <- NA

registry$notes[idx_pi] <-
  paste(
    "Recommended pre-oil-shock baseline.",
    "Robustness windows: 1967Q1-1972Q4 and 1964Q1-1972Q4.",
    "1948Q1-1972Q4 retained for descriptive comparison only.",
    "See output/estimates/Pi_ss_calibration.csv."
  )


# ============================================================================
# 8. UPDATE invPi
# ============================================================================

registry$classification[idx_invpi] <-
  "Fixed / derived"

registry$research_value[idx_invpi] <-
  invPi

registry$status[idx_invpi] <-
  "DERIVED FROM Pi_ss"

registry$method[idx_invpi] <-
  "invPi = 1 / Pi_ss"

registry$sample[idx_invpi] <-
  "Derived from Pi_ss"

registry$raw_or_primary_data[idx_invpi] <-
  "Derived from Pi_ss"

registry$source_or_literature[idx_invpi] <-
  "Model identity"

registry$standard_error[idx_invpi] <- NA

registry$shock_sd[idx_invpi] <- NA

registry$notes[idx_invpi] <-
  paste(
    "Mechanically derived from calibrated Pi_ss.",
    "Not independently estimated or calibrated."
  )


# ============================================================================
# 9. SAVE UPDATED PARAMETER REGISTRY
# ============================================================================

write.csv(
  registry,
  registry_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nParameter registry updated successfully.\n")
# ============================================================================
# 10. CREATE PARAMETER DECISION DOCUMENT
# ============================================================================

parameter_doc_file <- file.path(
  "docs",
  "parameter_Pi_ss.md"
)

parameter_document <- c(
  
  "# Parameter Decision: Pi_ss",
  
  "",
  
  "## Parameter definition",
  
  "",
  
  "`Pi_ss` is gross quarterly steady-state inflation.",
  
  "",
  
  "The related parameter `invPi` is defined mechanically as:",
  
  "",
  
  "```text",
  "invPi = 1 / Pi_ss",
  "```",
  
  "",
  
  "## Classification",
  
  "",
  
  "`Pi_ss`: Calibrated.",
  
  "",
  
  "`invPi`: Fixed / derived.",
  
  "",
  
  "## Data source",
  
  "",
  
  "Historical IGPC-Mtb consumer price index.",
  
  "",
  
  "Series code: `GAMMA12_IGPCMTB12`.",
  
  "",
  
  "Raw file used by the repository:",
  
  "",
  
  "`data/raw/IGPC_ipeadata.csv`",
  
  "",
  
  "The raw variable is a monthly price-index level, not an inflation rate.",
  
  "",
  
  "## Transformation",
  
  "",
  
  "Monthly gross inflation is constructed as:",
  
  "",
  
  "```text",
  "Pi_m,t = P_t / P_(t-1)",
  "```",
  
  "",
  
  "Quarterly gross inflation is obtained by compounding monthly gross inflation:",
  
  "",
  
  "```text",
  "Pi_q = Pi_m,1 * Pi_m,2 * Pi_m,3",
  "```",
  
  "",
  
  "The steady-state value is calibrated using the geometric mean:",
  
  "",
  
  "```text",
  "Pi_ss = exp(mean(log(Pi_q)))",
  "```",
  
  "",
  
  "## Baseline window",
  
  "",
  
  "1968Q1-1972Q4.",
  
  "",
  
  "This window represents the post-PAEG, pre-first-oil-shock inflation regime.",
  
  "",
  
  "1973 is excluded because it marks the beginning of the external shock period",
  "that the model is intended to explain.",
  
  "",
  
  "## Robustness windows",
  
  "",
  
  "- 1967Q1-1972Q4",
  "- 1964Q1-1972Q4",
  
  "",
  
  "The 1948Q1-1972Q4 calculation is retained only as a descriptive historical",
  "comparison and is not the preferred steady-state calibration.",
  
  "",
  
  "## Final calibration",
  
  "",
  
  paste0(
    "`Pi_ss = ",
    format(Pi_ss, digits = 12),
    "`"
  ),
  
  "",
  
  paste0(
    "`invPi = ",
    format(invPi, digits = 12),
    "`"
  ),
  
  "",
  
  paste0(
    "Implied quarterly steady-state inflation: ",
    round(100 * quarterly_inflation_ss, 4),
    "%."
  ),
  
  "",
  
  paste0(
    "Implied annual compounded steady-state inflation: ",
    round(100 * annual_inflation_ss, 4),
    "%."
  ),
  
  "",
  
  "## Dynare implementation",
  
  "",
  
  "```text",
  
  paste0(
    "Pi_ss = ",
    format(Pi_ss, digits = 12),
    ";"
  ),
  
  "invPi = 1/Pi_ss;",
  
  "```",
  
  "",
  
  "## Reproducibility",
  
  "",
  
  "The complete transformation and calibration are reproduced by:",
  
  "",
  
  "`R/03_brazil_inflation_steady_state.R`",
  
  "",
  
  "Processed quarterly data:",
  
  "",
  
  "`data/processed/brazil_igpc_quarterly.csv`",
  
  "",
  
  "Calibration results:",
  
  "",
  
  "`output/estimates/Pi_ss_calibration.csv`"
)

writeLines(
  parameter_document,
  parameter_doc_file,
  useBytes = TRUE
)

cat(
  "\nParameter documentation created:\n",
  parameter_doc_file,
  "\n"
)


# ============================================================================
# 11. SAVE DIAGNOSTIC FIGURE
# ============================================================================

figure_file <- file.path(
  "output",
  "figures",
  "brazil_igpc_quarterly_inflation.png"
)

png(
  filename = figure_file,
  width = 1200,
  height = 700,
  res = 120
)

plot(
  quarterly$inflation_quarterly * 100,
  type = "l",
  ylab = "Quarterly inflation (%)",
  xlab = "Quarter",
  main = "Brazil: IGPC-Mtb Quarterly Inflation",
  xaxt = "n"
)

# Create a readable x-axis every few years
tick_positions <- seq(
  1,
  nrow(quarterly),
  by = 20
)

axis(
  1,
  at = tick_positions,
  labels = quarterly$quarter[tick_positions],
  las = 2,
  cex.axis = 0.7
)

# Baseline window markers
baseline_start <- which(
  quarterly$quarter == "1968Q1"
)

oil_shock_start <- which(
  quarterly$quarter == "1973Q1"
)

if (length(baseline_start) == 1) {
  abline(
    v = baseline_start,
    lty = 2
  )
}

if (length(oil_shock_start) == 1) {
  abline(
    v = oil_shock_start,
    lty = 2
  )
}

dev.off()

cat(
  "\nDiagnostic figure saved:\n",
  figure_file,
  "\n"
)


# ============================================================================
# 12. FINAL OUTPUT SUMMARY
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("STEADY-STATE INFLATION CALIBRATION COMPLETE\n")
cat("============================================================\n")

cat(
  "\nBaseline sample: 1968Q1-1972Q4\n"
)

cat(
  "\nPi_ss = ",
  format(Pi_ss, digits = 12),
  "\n",
  sep = ""
)

cat(
  "invPi = ",
  format(invPi, digits = 12),
  "\n",
  sep = ""
)

cat(
  "Quarterly steady-state inflation = ",
  round(100 * quarterly_inflation_ss, 4),
  "%\n",
  sep = ""
)

cat(
  "Annual compounded steady-state inflation = ",
  round(100 * annual_inflation_ss, 4),
  "%\n",
  sep = ""
)

cat("\nDynare calibration:\n\n")

cat(
  "Pi_ss = ",
  format(Pi_ss, digits = 12),
  ";\n",
  sep = ""
)

cat(
  "invPi = 1/Pi_ss;\n"
)

cat("\nFiles created or updated:\n")

cat(
  "  data/processed/brazil_igpc_quarterly.csv\n"
)

cat(
  "  output/estimates/Pi_ss_calibration.csv\n"
)

cat(
  "  output/figures/brazil_igpc_quarterly_inflation.png\n"
)

cat(
  "  docs/data_dictionary.csv\n"
)

cat(
  "  docs/parameter_registry.csv\n"
)

cat(
  "  docs/parameter_Pi_ss.md\n"
)

cat("\n============================================================\n")
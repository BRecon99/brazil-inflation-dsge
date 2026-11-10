# =============================================================================
# 04i_steady_state_fiscal_shares.R
#
# Purpose:
#   Calibrate steady-state government-consumption and government-investment
#   shares of GDP.
#
# Parameters:
#   g_y  = G / Y
#   ig_y = I_G / Y
#
# FINAL baseline:
#   1968-1972
#
# Robustness:
#   1968-1973
#
# Important pipeline rule:
#   This script DOES NOT write docs/parameter_registry.csv.
#   Script 99 is the sole registry writer.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Paths
# -----------------------------------------------------------------------------

raw_file <- "data/raw/fiscal_raw_1947_1980.csv"

estimate_dir <- "output/estimates"
diagnostic_dir <- "outputs/fiscal_shares"

dir.create(
  estimate_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  diagnostic_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

sample_output_file <- file.path(
  estimate_dir,
  "steady_state_fiscal_share_sample.csv"
)

parameter_output_file <- file.path(
  estimate_dir,
  "steady_state_fiscal_share_parameters.csv"
)

robustness_output_file <- file.path(
  estimate_dir,
  "steady_state_fiscal_share_robustness.csv"
)

dynare_output_file <- file.path(
  diagnostic_dir,
  "steady_state_fiscal_shares_dynare.txt"
)


# -----------------------------------------------------------------------------
# 1. Check input
# -----------------------------------------------------------------------------

if (!file.exists(raw_file)) {
  stop(
    paste(
      "Required fiscal file not found:",
      raw_file
    )
  )
}


# -----------------------------------------------------------------------------
# 2. Read fiscal data
# -----------------------------------------------------------------------------

fiscal <- read.csv(
  raw_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "year",
  "government_consumption_pct_gdp",
  "government_investment_pct_gdp"
)

missing_columns <- required_columns[
  !required_columns %in% names(fiscal)
]

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

fiscal <- fiscal[
  order(fiscal$year),
]


# -----------------------------------------------------------------------------
# 3. Helper function
# -----------------------------------------------------------------------------

calculate_fiscal_shares <- function(
    data,
    start_year,
    end_year,
    sample_label
) {
  
  sample_data <- data[
    data$year >= start_year &
      data$year <= end_year,
  ]
  
  if (nrow(sample_data) == 0) {
    stop(
      paste(
        "No observations found for",
        sample_label
      )
    )
  }
  
  if (
    anyNA(
      sample_data$government_consumption_pct_gdp
    )
  ) {
    stop(
      paste(
        "Missing government consumption observations in",
        sample_label
      )
    )
  }
  
  if (
    anyNA(
      sample_data$government_investment_pct_gdp
    )
  ) {
    stop(
      paste(
        "Missing government investment observations in",
        sample_label
      )
    )
  }
  
  g_mean_pct <- mean(
    sample_data$government_consumption_pct_gdp
  )
  
  ig_mean_pct <- mean(
    sample_data$government_investment_pct_gdp
  )
  
  g_sd_pct <- sd(
    sample_data$government_consumption_pct_gdp
  )
  
  ig_sd_pct <- sd(
    sample_data$government_investment_pct_gdp
  )
  
  g_se_pct <-
    g_sd_pct /
    sqrt(
      nrow(sample_data)
    )
  
  ig_se_pct <-
    ig_sd_pct /
    sqrt(
      nrow(sample_data)
    )
  
  list(
    sample_data = sample_data,
    sample_label = sample_label,
    n = nrow(sample_data),
    g_y = g_mean_pct / 100,
    ig_y = ig_mean_pct / 100,
    g_mean_pct = g_mean_pct,
    ig_mean_pct = ig_mean_pct,
    g_sd_pct = g_sd_pct,
    ig_sd_pct = ig_sd_pct,
    g_se_pct = g_se_pct,
    ig_se_pct = ig_se_pct,
    g_min_pct = min(
      sample_data$government_consumption_pct_gdp
    ),
    g_max_pct = max(
      sample_data$government_consumption_pct_gdp
    ),
    ig_min_pct = min(
      sample_data$government_investment_pct_gdp
    ),
    ig_max_pct = max(
      sample_data$government_investment_pct_gdp
    )
  )
}


# -----------------------------------------------------------------------------
# 4. FINAL BASELINE: 1968-1972
# -----------------------------------------------------------------------------

baseline <- calculate_fiscal_shares(
  data = fiscal,
  start_year = 1968,
  end_year = 1972,
  sample_label = "1968-1972"
)

g_y <- baseline$g_y
ig_y <- baseline$ig_y


# -----------------------------------------------------------------------------
# 5. ROBUSTNESS: 1968-1973
# -----------------------------------------------------------------------------

robustness <- calculate_fiscal_shares(
  data = fiscal,
  start_year = 1968,
  end_year = 1973,
  sample_label = "1968-1973"
)


# -----------------------------------------------------------------------------
# 6. Save preferred sample observations
# -----------------------------------------------------------------------------

preferred_sample <- baseline$sample_data

preferred_sample$government_consumption_share <-
  preferred_sample$government_consumption_pct_gdp /
  100

preferred_sample$government_investment_share <-
  preferred_sample$government_investment_pct_gdp /
  100

write.csv(
  preferred_sample,
  sample_output_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 7. Machine-readable preferred parameters
# -----------------------------------------------------------------------------

parameter_results <- data.frame(
  dynare_name = c(
    "g_y",
    "ig_y"
  ),
  symbol = c(
    "G^C/Y",
    "I^G/Y"
  ),
  estimate = c(
    baseline$g_y,
    baseline$ig_y
  ),
  mean_percent_gdp = c(
    baseline$g_mean_pct,
    baseline$ig_mean_pct
  ),
  sample_sd_percent_gdp = c(
    baseline$g_sd_pct,
    baseline$ig_sd_pct
  ),
  mean_se_percent_gdp = c(
    baseline$g_se_pct,
    baseline$ig_se_pct
  ),
  minimum_percent_gdp = c(
    baseline$g_min_pct,
    baseline$ig_min_pct
  ),
  maximum_percent_gdp = c(
    baseline$g_max_pct,
    baseline$ig_max_pct
  ),
  n = c(
    baseline$n,
    baseline$n
  ),
  sample = c(
    baseline$sample_label,
    baseline$sample_label
  ),
  method = c(
    "Arithmetic mean of annual government consumption/GDP ratio",
    "Arithmetic mean of annual government investment/GDP ratio"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameter_results,
  parameter_output_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 8. Robustness table
# -----------------------------------------------------------------------------

robustness_results <- data.frame(
  dynare_name = c(
    "g_y",
    "ig_y"
  ),
  preferred_1968_1972 = c(
    baseline$g_y,
    baseline$ig_y
  ),
  robustness_1968_1973 = c(
    robustness$g_y,
    robustness$ig_y
  ),
  difference = c(
    robustness$g_y - baseline$g_y,
    robustness$ig_y - baseline$ig_y
  ),
  stringsAsFactors = FALSE
)

write.csv(
  robustness_results,
  robustness_output_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 9. Dynare-ready output
# -----------------------------------------------------------------------------

dynare_lines <- c(
  sprintf(
    "g_y = %.15f;",
    g_y
  ),
  sprintf(
    "ig_y = %.15f;",
    ig_y
  )
)

writeLines(
  dynare_lines,
  dynare_output_file
)


# -----------------------------------------------------------------------------
# 10. Validation against accepted baseline
# -----------------------------------------------------------------------------

expected_g_y <- 0.110279410789871
expected_ig_y <- 0.044702876856991

tolerance <- 1e-12

if (
  abs(
    g_y -
    expected_g_y
  ) > tolerance
) {
  stop(
    paste(
      "g_y differs from accepted 1968-1972 value.",
      "Calculated:",
      g_y,
      "Expected:",
      expected_g_y
    )
  )
}

if (
  abs(
    ig_y -
    expected_ig_y
  ) > tolerance
) {
  stop(
    paste(
      "ig_y differs from accepted 1968-1972 value.",
      "Calculated:",
      ig_y,
      "Expected:",
      expected_ig_y
    )
  )
}

if (
  g_y <= 0 |
  g_y >= 1
) {
  stop(
    "g_y must lie between 0 and 1."
  )
}

if (
  ig_y <= 0 |
  ig_y >= 1
) {
  stop(
    "ig_y must lie between 0 and 1."
  )
}


# -----------------------------------------------------------------------------
# 11. Final console output
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("SCRIPT 04i COMPLETE\n")
cat("============================================================\n")
cat("\n")
cat("FINAL BASELINE: 1968-1972\n")
cat(sprintf("g_y  = %.15f\n", g_y))
cat(sprintf("ig_y = %.15f\n", ig_y))
cat("\n")
cat("ROBUSTNESS: 1968-1973\n")
cat(sprintf("g_y  = %.15f\n", robustness$g_y))
cat(sprintf("ig_y = %.15f\n", robustness$ig_y))
cat("\n")
cat("Difference: robustness - baseline\n")
cat(sprintf("g_y  = %.15f\n", robustness$g_y - g_y))
cat(sprintf("ig_y = %.15f\n", robustness$ig_y - ig_y))
cat("\n")
cat("Outputs:\n")
cat(" - ", sample_output_file, "\n", sep = "")
cat(" - ", parameter_output_file, "\n", sep = "")
cat(" - ", robustness_output_file, "\n", sep = "")
cat(" - ", dynare_output_file, "\n", sep = "")
cat("\n")
cat("Registry NOT modified. Script 99 remains the sole registry writer.\n")
cat("============================================================\n")
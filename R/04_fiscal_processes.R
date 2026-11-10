# =============================================================================
# 04_fiscal_processes.R
#
# Purpose:
#   Estimate persistence of Brazilian fiscal processes using annual data
#   from 1947-1980.
#
# Processes:
#   rhotau = tax-revenue persistence
#   rhoG   = government-consumption persistence
#   rhoIG  = government-investment persistence
#
# Specification:
#   1. Remove regime-specific means.
#   2. Estimate AR(1) with NO intercept:
#
#        x_t = rho_annual * x_(t-1) + epsilon_t
#
#   3. Convert annual persistence to quarterly persistence:
#
#        rho_quarterly = rho_annual^(1/4)
#
# Accepted samples:
#
#   Taxes:
#     1947-1963 and 1968-1980
#     Exclude 1964-1967
#
#   Government consumption:
#     1947-1973 and 1974-1980
#
#   Government investment:
#     1947-1978
#     Exclude 1979-1980
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

diagnostic_root <- "outputs/fiscal_ar1"
diagnostic_table_dir <- file.path(diagnostic_root, "tables")
diagnostic_figure_dir <- file.path(diagnostic_root, "figures")

dir.create(estimate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_root, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_figure_dir, recursive = TRUE, showWarnings = FALSE)

ar1_output_file <- file.path(
  estimate_dir,
  "fiscal_persistence_ar1.csv"
)

parameter_output_file <- file.path(
  estimate_dir,
  "fiscal_persistence_parameters.csv"
)

dynare_output_file <- file.path(
  diagnostic_root,
  "fiscal_persistence_dynare.txt"
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
  "tax_burden_global_pct_gdp",
  "government_consumption_pct_gdp",
  "government_investment_pct_gdp"
)

missing_columns <- required_columns[
  !required_columns %in% names(fiscal)
]

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required fiscal columns:",
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
# 3. Helper: fit regime-adjusted AR(1)
# -----------------------------------------------------------------------------

fit_regime_ar <- function(
    data,
    value_column,
    regimes,
    parameter_name,
    sample_description,
    treatment_description
) {
  
  pieces <- list()
  
  piece_counter <- 1
  
  for (regime_index in seq_along(regimes)) {
    
    start_year <- regimes[[regime_index]][1]
    end_year <- regimes[[regime_index]][2]
    
    regime_data <- data[
      data$year >= start_year &
        data$year <= end_year,
      c(
        "year",
        value_column
      )
    ]
    
    names(regime_data)[2] <- "value"
    
    if (nrow(regime_data) < 2) {
      stop(
        paste(
          "Insufficient observations for",
          parameter_name,
          "regime",
          start_year,
          "-",
          end_year
        )
      )
    }
    
    regime_data <- regime_data[
      is.finite(regime_data$value),
    ]
    
    regime_mean <- mean(
      regime_data$value
    )
    
    regime_data$regime <- regime_index
    regime_data$regime_start <- start_year
    regime_data$regime_end <- end_year
    regime_data$regime_mean <- regime_mean
    regime_data$deviation <- regime_data$value - regime_mean
    regime_data$lag_deviation <- c(
      NA,
      regime_data$deviation[
        1:(nrow(regime_data) - 1)
      ]
    )
    
    pieces[[piece_counter]] <- regime_data
    
    piece_counter <- piece_counter + 1
  }
  
  regression_data <- do.call(
    rbind,
    pieces
  )
  
  regression_data <- regression_data[
    is.finite(regression_data$deviation) &
      is.finite(regression_data$lag_deviation),
  ]
  
  if (nrow(regression_data) < 3) {
    stop(
      paste(
        "Insufficient AR observations for",
        parameter_name
      )
    )
  }
  
  # No intercept by construction because the series is already centered
  # around regime-specific means.
  
  fit <- lm(
    deviation ~ 0 + lag_deviation,
    data = regression_data
  )
  
  fit_summary <- summary(fit)
  
  rho_annual <- unname(
    coef(fit)[
      "lag_deviation"
    ]
  )
  
  rho_annual_se <- fit_summary$coefficients[
    "lag_deviation",
    "Std. Error"
  ]
  
  rho_annual_p_value <- fit_summary$coefficients[
    "lag_deviation",
    "Pr(>|t|)"
  ]
  
  rho_annual_ci_low <-
    rho_annual -
    1.96 * rho_annual_se
  
  rho_annual_ci_high <-
    rho_annual +
    1.96 * rho_annual_se
  
  if (rho_annual <= 0) {
    stop(
      paste(
        "Annual rho is non-positive for",
        parameter_name,
        "and cannot be converted using rho^(1/4)."
      )
    )
  }
  
  rho_quarterly <- rho_annual^(1 / 4)
  
  # Delta-method SE for quarterly transformation.
  derivative <-
    (1 / 4) *
    rho_annual^(-3 / 4)
  
  rho_quarterly_se <-
    abs(derivative) *
    rho_annual_se
  
  annual_innovation_sd_pct_points <- sigma(fit)
  
  annual_innovation_sd_share <-
    annual_innovation_sd_pct_points /
    100
  
  regression_data$fitted <- fitted(fit)
  regression_data$residual <- residuals(fit)
  
  list(
    parameter = parameter_name,
    rho_annual = rho_annual,
    rho_annual_se = rho_annual_se,
    rho_annual_ci_low = rho_annual_ci_low,
    rho_annual_ci_high = rho_annual_ci_high,
    rho_annual_p_value = rho_annual_p_value,
    rho_quarterly = rho_quarterly,
    rho_quarterly_se = rho_quarterly_se,
    annual_innovation_sd_pct_points = annual_innovation_sd_pct_points,
    annual_innovation_sd_share = annual_innovation_sd_share,
    n_ar_observations = nrow(regression_data),
    sample = sample_description,
    treatment = treatment_description,
    data = regression_data,
    fit = fit
  )
}


# -----------------------------------------------------------------------------
# 4. TAX PROCESS
# -----------------------------------------------------------------------------

tax_result <- fit_regime_ar(
  data = fiscal,
  value_column = "tax_burden_global_pct_gdp",
  regimes = list(
    c(
      1947,
      1963
    ),
    c(
      1968,
      1980
    )
  ),
  parameter_name = "rhotau",
  sample_description = "1947-1963 and 1968-1980",
  treatment_description = paste(
    "Tax burden as percent of GDP;",
    "1964-1967 excluded;",
    "regime means removed;",
    "AR(1) without intercept"
  )
)


# -----------------------------------------------------------------------------
# 5. GOVERNMENT CONSUMPTION PROCESS
# -----------------------------------------------------------------------------

g_result <- fit_regime_ar(
  data = fiscal,
  value_column = "government_consumption_pct_gdp",
  regimes = list(
    c(
      1947,
      1973
    ),
    c(
      1974,
      1980
    )
  ),
  parameter_name = "rhoG",
  sample_description = "1947-1973 and 1974-1980",
  treatment_description = paste(
    "Government consumption as percent of GDP;",
    "pre/post-1974 regime means removed;",
    "AR(1) without intercept"
  )
)


# -----------------------------------------------------------------------------
# 6. GOVERNMENT INVESTMENT PROCESS
# -----------------------------------------------------------------------------

ig_result <- fit_regime_ar(
  data = fiscal,
  value_column = "government_investment_pct_gdp",
  regimes = list(
    c(
      1947,
      1978
    )
  ),
  parameter_name = "rhoIG",
  sample_description = "1947-1978",
  treatment_description = paste(
    "Government investment as percent of GDP;",
    "1979-1980 excluded;",
    "sample mean removed;",
    "AR(1) without intercept"
  )
)


# -----------------------------------------------------------------------------
# 7. Standardized AR(1) output
# -----------------------------------------------------------------------------

results <- list(
  tax_result,
  g_result,
  ig_result
)

ar1_results <- do.call(
  rbind,
  lapply(
    results,
    function(x) {
      
      data.frame(
        parameter = x$parameter,
        rho_annual = x$rho_annual,
        rho_annual_se = x$rho_annual_se,
        rho_annual_ci_low = x$rho_annual_ci_low,
        rho_annual_ci_high = x$rho_annual_ci_high,
        rho_annual_p_value = x$rho_annual_p_value,
        rho_quarterly_equivalent = x$rho_quarterly,
        rho_quarterly_se = x$rho_quarterly_se,
        annual_innovation_sd_pct_points =
          x$annual_innovation_sd_pct_points,
        annual_innovation_sd_share =
          x$annual_innovation_sd_share,
        n_ar_observations =
          x$n_ar_observations,
        stationary_abs_rho_lt_1 =
          abs(x$rho_quarterly) < 1,
        sample =
          x$sample,
        treatment =
          x$treatment,
        stringsAsFactors = FALSE
      )
    }
  )
)

write.csv(
  ar1_results,
  ar1_output_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 8. Machine-readable parameter output
# -----------------------------------------------------------------------------

parameter_results <- data.frame(
  dynare_name = c(
    "rhotau",
    "rhoG",
    "rhoIG"
  ),
  estimate = c(
    tax_result$rho_quarterly,
    g_result$rho_quarterly,
    ig_result$rho_quarterly
  ),
  standard_error = c(
    tax_result$rho_quarterly_se,
    g_result$rho_quarterly_se,
    ig_result$rho_quarterly_se
  ),
  shock_sd = c(
    NA_real_,
    NA_real_,
    NA_real_
  ),
  method = rep(
    paste(
      "Annual regime-adjusted AR(1),",
      "no intercept;",
      "converted to quarterly using rho_annual^(1/4)"
    ),
    3
  ),
  sample = c(
    tax_result$sample,
    g_result$sample,
    ig_result$sample
  ),
  source_file = rep(
    raw_file,
    3
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameter_results,
  parameter_output_file,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 9. Save detailed regression data
# -----------------------------------------------------------------------------

write.csv(
  tax_result$data,
  file.path(
    diagnostic_table_dir,
    "tax_ar1_regression_data.csv"
  ),
  row.names = FALSE
)

write.csv(
  g_result$data,
  file.path(
    diagnostic_table_dir,
    "government_consumption_ar1_regression_data.csv"
  ),
  row.names = FALSE
)

write.csv(
  ig_result$data,
  file.path(
    diagnostic_table_dir,
    "government_investment_ar1_regression_data.csv"
  ),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 10. Diagnostic plots
# -----------------------------------------------------------------------------

plot_ar_result <- function(
    result,
    title,
    output_file
) {
  
  d <- result$data
  
  png(
    output_file,
    width = 1200,
    height = 800,
    res = 130
  )
  
  plot(
    d$year,
    d$deviation,
    type = "b",
    xlab = "Year",
    ylab = "Deviation from regime mean (percentage points)",
    main = title
  )
  
  abline(
    h = 0,
    lty = 2
  )
  
  dev.off()
}

plot_ar_result(
  tax_result,
  "Tax burden: regime-adjusted deviations",
  file.path(
    diagnostic_figure_dir,
    "tax_regime_adjusted_series.png"
  )
)

plot_ar_result(
  g_result,
  "Government consumption: regime-adjusted deviations",
  file.path(
    diagnostic_figure_dir,
    "government_consumption_regime_adjusted_series.png"
  )
)

plot_ar_result(
  ig_result,
  "Government investment: demeaned deviations",
  file.path(
    diagnostic_figure_dir,
    "government_investment_regime_adjusted_series.png"
  )
)


# -----------------------------------------------------------------------------
# 11. Dynare-ready text output
# -----------------------------------------------------------------------------

dynare_lines <- c(
  sprintf(
    "rhotau = %.15f;",
    tax_result$rho_quarterly
  ),
  sprintf(
    "rhoG = %.15f;",
    g_result$rho_quarterly
  ),
  sprintf(
    "rhoIG = %.15f;",
    ig_result$rho_quarterly
  )
)

writeLines(
  dynare_lines,
  dynare_output_file
)


# -----------------------------------------------------------------------------
# 12. Validation against accepted specification
# -----------------------------------------------------------------------------

expected_rhotau <- 0.902332736768704
expected_rhoG <- 0.697503334451888
expected_rhoIG <- 0.864304193118051

tolerance <- 1e-10

if (
  abs(
    tax_result$rho_quarterly -
    expected_rhotau
  ) > tolerance
) {
  stop(
    paste(
      "rhotau differs from accepted value.",
      "Calculated:",
      tax_result$rho_quarterly,
      "Expected:",
      expected_rhotau
    )
  )
}

if (
  abs(
    g_result$rho_quarterly -
    expected_rhoG
  ) > tolerance
) {
  stop(
    paste(
      "rhoG differs from accepted value.",
      "Calculated:",
      g_result$rho_quarterly,
      "Expected:",
      expected_rhoG
    )
  )
}

if (
  abs(
    ig_result$rho_quarterly -
    expected_rhoIG
  ) > tolerance
) {
  stop(
    paste(
      "rhoIG differs from accepted value.",
      "Calculated:",
      ig_result$rho_quarterly,
      "Expected:",
      expected_rhoIG
    )
  )
}

if (
  any(
    abs(
      parameter_results$estimate
    ) >= 1
  )
) {
  stop(
    "At least one quarterly fiscal persistence parameter is non-stationary."
  )
}


# -----------------------------------------------------------------------------
# 13. Final console output
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("SCRIPT 04 COMPLETE\n")
cat("============================================================\n")
cat("\n")
cat("Annual persistence estimates:\n")
cat(sprintf("tax:  %.10f\n", tax_result$rho_annual))
cat(sprintf("G:    %.10f\n", g_result$rho_annual))
cat(sprintf("IG:   %.10f\n", ig_result$rho_annual))
cat("\n")
cat("Quarterly Dynare persistence parameters:\n")
cat(sprintf("rhotau = %.15f\n", tax_result$rho_quarterly))
cat(sprintf("rhoG   = %.15f\n", g_result$rho_quarterly))
cat(sprintf("rhoIG  = %.15f\n", ig_result$rho_quarterly))
cat("\n")
cat("Expected accepted values:\n")
cat(sprintf("rhotau = %.15f\n", expected_rhotau))
cat(sprintf("rhoG   = %.15f\n", expected_rhoG))
cat(sprintf("rhoIG  = %.15f\n", expected_rhoIG))
cat("\n")
cat("Outputs:\n")
cat(" - ", ar1_output_file, "\n", sep = "")
cat(" - ", parameter_output_file, "\n", sep = "")
cat(" - ", diagnostic_root, "/\n", sep = "")
cat("\n")
cat("Registry NOT modified. Script 99 remains the sole registry writer.\n")
cat("============================================================\n")
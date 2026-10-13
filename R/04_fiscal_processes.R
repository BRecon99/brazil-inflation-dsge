# =============================================================================
# 04_fiscal_processes.R
#
# Fiscal persistence estimation: Brazil, 1947-1980
#
# Variables:
#   T/Y   = global tax burden / GDP
#   G/Y   = government consumption / GDP
#   IG/Y  = government investment / GDP
#
# PURPOSE
# -------
# Estimate fiscal AR(1) persistence parameters while preventing major
# historical regime changes from being incorrectly interpreted as
# ordinary stochastic persistence.
#
# Selected treatment:
#
# TAX:
#   - Pre-PAEG regime: 1947-1963
#   - PAEG transition: 1964-1967 EXCLUDED from persistence estimation
#   - Post-PAEG regime: 1968-1980
#
# GOVERNMENT CONSUMPTION:
#   - Regime 1: 1947-1973
#   - Regime 2: 1974-1980
#
# GOVERNMENT INVESTMENT:
#   - Persistence estimated over 1947-1978
#   - 1979-1980 treated as second-oil-shock / external-crisis boundary
#
# Selected AR specification:
#
#       x_tilde[t] = rho * x_tilde[t-1] + epsilon[t]
#
# where:
#
#       x_tilde[t] = x[t] - regime-specific mean
#
# The regression does NOT create lags across regime boundaries.
#
# IMPORTANT FREQUENCY NOTE
# ------------------------
# These historical fiscal data are annual.
#
# Annual rho is converted to a quarterly-equivalent persistence using:
#
#       rho_quarterly = rho_annual^(1/4)
#
# because the current DSGE is quarterly.
#
# Annual residual standard deviations are NOT automatically converted
# into quarterly shock standard deviations.
#
# OUTPUTS ADDED TO STANDARD PROJECT STRUCTURE
# -------------------------------------------
#
#   output/estimates/fiscal_persistence_ar1.csv
#   output/estimates/fiscal_persistence_parameters.csv
#   docs/parameter_registry.csv
#
# =============================================================================


rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# =============================================================================
# 1. FILE PATHS
# =============================================================================

candidate_paths <- c(
  "data/raw/fiscal_raw_1947_1980.csv",
  "fiscal_raw_1947_1980.csv",
  "/mnt/data/fiscal_raw_1947_1980.csv"
)

existing_paths <- candidate_paths[file.exists(candidate_paths)]

if (length(existing_paths) == 0) {
  stop(
    paste0(
      "\nCould not find fiscal_raw_1947_1980.csv.\n\n",
      "Place the file at:\n",
      "data/raw/fiscal_raw_1947_1980.csv\n"
    )
  )
}

data_path <- existing_paths[1]

cat("\n============================================================\n")
cat("FISCAL AR(1) ESTIMATION\n")
cat("============================================================\n")
cat("Reading data from:\n")
cat(data_path, "\n\n")


# =============================================================================
# 2. OUTPUT FOLDERS
# =============================================================================

output_dir <- file.path(
  "outputs",
  "fiscal_ar1"
)

figure_dir <- file.path(
  output_dir,
  "figures"
)

table_dir <- file.path(
  output_dir,
  "tables"
)

# Standard project estimate folder
standard_estimate_dir <- file.path(
  "output",
  "estimates"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  standard_estimate_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 3. LOAD DATA
# =============================================================================

fiscal <- read.csv(
  data_path,
  check.names = FALSE
)

required_columns <- c(
  "year",
  "tax_burden_global_pct_gdp",
  "government_consumption_pct_gdp",
  "government_investment_pct_gdp"
)

missing_columns <- setdiff(
  required_columns,
  names(fiscal)
)

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

row.names(fiscal) <- NULL


# =============================================================================
# 4. BASIC DATA CHECKS
# =============================================================================

if (anyDuplicated(fiscal$year)) {
  stop("Duplicate years found in fiscal dataset.")
}

expected_years <- 1947:1980

missing_years <- setdiff(
  expected_years,
  fiscal$year
)

if (length(missing_years) > 0) {
  
  stop(
    paste(
      "Missing years:",
      paste(
        missing_years,
        collapse = ", "
      )
    )
  )
  
}

if (
  any(
    is.na(
      fiscal[
        ,
        required_columns
      ]
    )
  )
) {
  
  stop(
    "Missing values found in required fiscal variables."
  )
  
}


cat("Data check passed.\n")
cat(
  "Years:",
  min(fiscal$year),
  "to",
  max(fiscal$year),
  "\n"
)

cat(
  "Observations:",
  nrow(fiscal),
  "\n\n"
)


# =============================================================================
# 5. SHORTER VARIABLE NAMES
# =============================================================================

fiscal$tax <- fiscal$tax_burden_global_pct_gdp

fiscal$g <- fiscal$government_consumption_pct_gdp

fiscal$ig <- fiscal$government_investment_pct_gdp


# =============================================================================
# 6. HISTORICAL PERIOD CLASSIFICATION
# =============================================================================

fiscal$historical_period <- NA_character_

fiscal$historical_period[
  fiscal$year >= 1947 &
    fiscal$year <= 1955
] <- "1947-1955: Postwar / pre-Plano de Metas"

fiscal$historical_period[
  fiscal$year >= 1956 &
    fiscal$year <= 1963
] <- "1956-1963: Plano de Metas / pre-PAEG"

fiscal$historical_period[
  fiscal$year >= 1964 &
    fiscal$year <= 1967
] <- "1964-1967: PAEG transition"

fiscal$historical_period[
  fiscal$year >= 1968 &
    fiscal$year <= 1973
] <- "1968-1973: PED / Miracle / pre-oil baseline"

fiscal$historical_period[
  fiscal$year >= 1974 &
    fiscal$year <= 1978
] <- "1974-1978: First oil shock / II PND"

fiscal$historical_period[
  fiscal$year >= 1979 &
    fiscal$year <= 1980
] <- "1979-1980: Second oil shock / external tightening"


# =============================================================================
# 7. HISTORICAL PERIOD SUMMARY
# =============================================================================

period_order <- unique(
  fiscal$historical_period
)

period_summary <- do.call(
  rbind,
  lapply(
    period_order,
    function(period_name) {
      
      d <- fiscal[
        fiscal$historical_period == period_name,
      ]
      
      data.frame(
        period = period_name,
        
        start_year = min(d$year),
        end_year = max(d$year),
        n = nrow(d),
        
        tax_mean = mean(d$tax),
        tax_sd = sd(d$tax),
        
        g_mean = mean(d$g),
        g_sd = sd(d$g),
        
        ig_mean = mean(d$ig),
        ig_sd = sd(d$ig),
        
        stringsAsFactors = FALSE
      )
    }
  )
)

write.csv(
  period_summary,
  file.path(
    table_dir,
    "historical_period_summary.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 8. GRAPH THE THREE FISCAL RATIOS
# =============================================================================

historical_breaks <- c(
  1964,
  1968,
  1974,
  1979
)

png(
  file.path(
    figure_dir,
    "fiscal_ratios_1947_1980.png"
  ),
  width = 1800,
  height = 1800,
  res = 200
)

par(
  mfrow = c(3, 1),
  mar = c(4, 5, 3, 1)
)


# -----------------------------------------------------------------------------
# TAX
# -----------------------------------------------------------------------------

plot(
  fiscal$year,
  fiscal$tax,
  type = "o",
  lwd = 2,
  pch = 16,
  xlab = "Year",
  ylab = "Percent of GDP",
  main = "Global Tax Burden / GDP"
)

grid()

abline(
  v = historical_breaks,
  lty = 3
)


# -----------------------------------------------------------------------------
# GOVERNMENT CONSUMPTION
# -----------------------------------------------------------------------------

plot(
  fiscal$year,
  fiscal$g,
  type = "o",
  lwd = 2,
  pch = 16,
  xlab = "Year",
  ylab = "Percent of GDP",
  main = "Government Consumption / GDP"
)

grid()

abline(
  v = historical_breaks,
  lty = 3
)


# -----------------------------------------------------------------------------
# GOVERNMENT INVESTMENT
# -----------------------------------------------------------------------------

plot(
  fiscal$year,
  fiscal$ig,
  type = "o",
  lwd = 2,
  pch = 16,
  xlab = "Year",
  ylab = "Percent of GDP",
  main = "Government Investment / GDP"
)

grid()

abline(
  v = historical_breaks,
  lty = 3
)


dev.off()


# =============================================================================
# 9. HELPER: CREATE ORDINARY LAGGED AR DATA
# =============================================================================

make_lagged_data <- function(
    data,
    variable,
    start_year = min(data$year),
    end_year = max(data$year)
) {
  
  d <- data[
    data$year >= start_year &
      data$year <= end_year,
    c(
      "year",
      variable
    )
  ]
  
  d <- d[
    order(d$year),
  ]
  
  d <- d[
    complete.cases(d),
  ]
  
  if (nrow(d) < 4) {
    stop("Not enough observations for AR estimation.")
  }
  
  data.frame(
    year = d$year[-1],
    y = d[[variable]][-1],
    lag = d[[variable]][-nrow(d)]
  )
  
}


# =============================================================================
# 10. HELPER: STANDARD AR(1), WITH OPTIONAL STEP DUMMY
# =============================================================================

fit_step_ar <- function(
    data,
    variable,
    break_year = NULL,
    start_year = min(data$year),
    end_year = max(data$year)
) {
  
  ar_data <- make_lagged_data(
    data = data,
    variable = variable,
    start_year = start_year,
    end_year = end_year
  )
  
  if (is.null(break_year)) {
    
    model <- lm(
      y ~ lag,
      data = ar_data
    )
    
  } else {
    
    ar_data$post_break <- as.integer(
      ar_data$year >= break_year
    )
    
    model <- lm(
      y ~ lag + post_break,
      data = ar_data
    )
    
  }
  
  list(
    model = model,
    data = ar_data
  )
  
}


# =============================================================================
# 11. NAIVE AR(1) BENCHMARKS
# =============================================================================

tax_naive <- fit_step_ar(
  fiscal,
  "tax"
)

g_naive <- fit_step_ar(
  fiscal,
  "g"
)

ig_naive <- fit_step_ar(
  fiscal,
  "ig"
)


extract_naive <- function(
    fit_object,
    parameter_name
) {
  
  model <- fit_object$model
  
  coefficient_table <- summary(model)$coefficients
  
  data.frame(
    parameter = parameter_name,
    rho = coefficient_table[
      "lag",
      "Estimate"
    ],
    standard_error = coefficient_table[
      "lag",
      "Std. Error"
    ],
    p_value = coefficient_table[
      "lag",
      "Pr(>|t|)"
    ],
    n = nobs(model),
    AIC = AIC(model),
    BIC = BIC(model),
    stringsAsFactors = FALSE
  )
  
}


naive_results <- rbind(
  extract_naive(
    tax_naive,
    "tax"
  ),
  extract_naive(
    g_naive,
    "government_consumption"
  ),
  extract_naive(
    ig_naive,
    "government_investment"
  )
)

write.csv(
  naive_results,
  file.path(
    table_dir,
    "naive_ar1_results.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 12. CANDIDATE HISTORICAL BREAK DIAGNOSTICS
# =============================================================================

candidate_break_years <- c(
  1964,
  1968,
  1974,
  1979
)


candidate_break_table <- data.frame()


for (
  series_name in c(
    "tax",
    "g",
    "ig"
  )
) {
  
  base_fit <- fit_step_ar(
    fiscal,
    series_name
  )
  
  base_model <- base_fit$model
  
  base_row <- data.frame(
    series = series_name,
    specification = "No break",
    break_year = NA,
    rho = coef(base_model)["lag"],
    break_coefficient = NA,
    break_p_value = NA,
    AIC = AIC(base_model),
    BIC = BIC(base_model),
    n = nobs(base_model),
    stringsAsFactors = FALSE
  )
  
  candidate_break_table <- rbind(
    candidate_break_table,
    base_row
  )
  
  
  for (
    break_year in candidate_break_years
  ) {
    
    break_fit <- fit_step_ar(
      fiscal,
      series_name,
      break_year = break_year
    )
    
    break_model <- break_fit$model
    
    coefficient_table <- summary(
      break_model
    )$coefficients
    
    row <- data.frame(
      series = series_name,
      
      specification = paste0(
        "Step break ",
        break_year
      ),
      
      break_year = break_year,
      
      rho = coefficient_table[
        "lag",
        "Estimate"
      ],
      
      break_coefficient = coefficient_table[
        "post_break",
        "Estimate"
      ],
      
      break_p_value = coefficient_table[
        "post_break",
        "Pr(>|t|)"
      ],
      
      AIC = AIC(
        break_model
      ),
      
      BIC = BIC(
        break_model
      ),
      
      n = nobs(
        break_model
      ),
      
      stringsAsFactors = FALSE
    )
    
    candidate_break_table <- rbind(
      candidate_break_table,
      row
    )
    
  }
  
}


write.csv(
  candidate_break_table,
  file.path(
    table_dir,
    "candidate_break_models.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 13. FORMAL CHECKS FOR THE THREE HISTORICALLY SELECTED BREAKS
# =============================================================================

test_step_break <- function(
    data,
    variable,
    break_year
) {
  
  no_break <- fit_step_ar(
    data,
    variable
  )$model
  
  with_break <- fit_step_ar(
    data,
    variable,
    break_year
  )$model
  
  comparison <- anova(
    no_break,
    with_break
  )
  
  coefficient_table <- summary(
    with_break
  )$coefficients
  
  data.frame(
    series = variable,
    break_year = break_year,
    
    rho_with_break = coefficient_table[
      "lag",
      "Estimate"
    ],
    
    break_coefficient = coefficient_table[
      "post_break",
      "Estimate"
    ],
    
    break_t_p_value = coefficient_table[
      "post_break",
      "Pr(>|t|)"
    ],
    
    nested_F_p_value = comparison$`Pr(>F)`[2],
    
    BIC_no_break = BIC(
      no_break
    ),
    
    BIC_with_break = BIC(
      with_break
    ),
    
    stringsAsFactors = FALSE
  )
  
}


historical_break_tests <- rbind(
  
  test_step_break(
    fiscal,
    "tax",
    1964
  ),
  
  test_step_break(
    fiscal,
    "g",
    1974
  ),
  
  test_step_break(
    fiscal,
    "ig",
    1979
  )
  
)


write.csv(
  historical_break_tests,
  file.path(
    table_dir,
    "historical_break_tests.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 14. REGIME-DEVIATION AR(1) FUNCTION
# =============================================================================

fit_regime_ar <- function(
    data,
    variable,
    regimes
) {
  
  ar_rows <- list()
  
  mean_rows <- list()
  
  counter <- 1
  
  
  for (
    regime_name in names(regimes)
  ) {
    
    years <- regimes[[regime_name]]
    
    start_year <- years[1]
    end_year <- years[2]
    
    
    d <- data[
      data$year >= start_year &
        data$year <= end_year,
      c(
        "year",
        variable
      )
    ]
    
    
    d <- d[
      order(d$year),
    ]
    
    
    d <- d[
      complete.cases(d),
    ]
    
    
    if (
      nrow(d) < 2
    ) {
      
      stop(
        paste(
          "Not enough observations in regime:",
          regime_name
        )
      )
      
    }
    
    
    regime_mean <- mean(
      d[[variable]]
    )
    
    
    mean_rows[[counter]] <- data.frame(
      
      regime = regime_name,
      
      start_year = start_year,
      
      end_year = end_year,
      
      n = nrow(d),
      
      mean_pct_gdp = regime_mean,
      
      mean_share_gdp = regime_mean / 100,
      
      stringsAsFactors = FALSE
    )
    
    
    deviations <- (
      d[[variable]] -
        regime_mean
    )
    
    
    regime_ar <- data.frame(
      
      year = d$year[-1],
      
      y = deviations[-1],
      
      lag = deviations[
        -length(
          deviations
        )
      ],
      
      regime = regime_name,
      
      stringsAsFactors = FALSE
    )
    
    
    ar_rows[[counter]] <- regime_ar
    
    
    counter <- counter + 1
    
  }
  
  
  ar_data <- do.call(
    rbind,
    ar_rows
  )
  
  
  regime_means <- do.call(
    rbind,
    mean_rows
  )
  
  
  row.names(
    ar_data
  ) <- NULL
  
  
  row.names(
    regime_means
  ) <- NULL
  
  
  model <- lm(
    y ~ 0 + lag,
    data = ar_data
  )
  
  
  list(
    model = model,
    ar_data = ar_data,
    regime_means = regime_means
  )
  
}


# =============================================================================
# 15. SELECTED TAX ESTIMATION
# =============================================================================

tax_selected <- fit_regime_ar(
  
  data = fiscal,
  
  variable = "tax",
  
  regimes = list(
    
    pre_PAEG = c(
      1947,
      1963
    ),
    
    post_PAEG = c(
      1968,
      1980
    )
    
  )
  
)


# =============================================================================
# 16. SELECTED GOVERNMENT CONSUMPTION ESTIMATION
# =============================================================================

g_selected <- fit_regime_ar(
  
  data = fiscal,
  
  variable = "g",
  
  regimes = list(
    
    pre_1974 = c(
      1947,
      1973
    ),
    
    post_1974 = c(
      1974,
      1980
    )
    
  )
  
)


# =============================================================================
# 17. SELECTED GOVERNMENT INVESTMENT ESTIMATION
# =============================================================================

ig_selected <- fit_regime_ar(
  
  data = fiscal,
  
  variable = "ig",
  
  regimes = list(
    
    pre_1979_crisis = c(
      1947,
      1978
    )
    
  )
  
)


# =============================================================================
# 18. EXTRACT SELECTED AR PARAMETERS
# =============================================================================

extract_selected_rho <- function(
    fit_object,
    parameter_name,
    sample_description,
    treatment_description
) {
  
  model <- fit_object$model
  
  coefficient_table <- summary(
    model
  )$coefficients
  
  rho_annual <- coefficient_table[
    "lag",
    "Estimate"
  ]
  
  rho_se <- coefficient_table[
    "lag",
    "Std. Error"
  ]
  
  rho_p <- coefficient_table[
    "lag",
    "Pr(>|t|)"
  ]
  
  rho_ci <- confint(
    model,
    "lag",
    level = 0.95
  )
  
  
  if (
    rho_annual >= 0
  ) {
    
    rho_quarterly <- rho_annual^(1 / 4)
    
  } else {
    
    rho_quarterly <- NA_real_
    
  }
  
  
  # Delta-method SE for quarterly persistence
  if (
    rho_annual > 0
  ) {
    
    rho_quarterly_se <-
      abs(
        0.25 *
          rho_annual^(-0.75)
      ) *
      rho_se
    
  } else {
    
    rho_quarterly_se <- NA_real_
    
  }
  
  
  data.frame(
    
    parameter = parameter_name,
    
    rho_annual = rho_annual,
    
    rho_annual_se = rho_se,
    
    rho_annual_ci_low = rho_ci[1],
    
    rho_annual_ci_high = rho_ci[2],
    
    rho_annual_p_value = rho_p,
    
    rho_quarterly_equivalent = rho_quarterly,
    
    rho_quarterly_se = rho_quarterly_se,
    
    annual_innovation_sd_pct_points = summary(
      model
    )$sigma,
    
    annual_innovation_sd_share = summary(
      model
    )$sigma / 100,
    
    n_ar_observations = nobs(
      model
    ),
    
    stationary_abs_rho_lt_1 = (
      abs(
        rho_annual
      ) < 1
    ),
    
    sample = sample_description,
    
    treatment = treatment_description,
    
    stringsAsFactors = FALSE
  )
  
}


selected_rhos <- rbind(
  
  extract_selected_rho(
    
    tax_selected,
    
    parameter_name = "rho_tau",
    
    sample_description =
      "1947-1963 and 1968-1980",
    
    treatment_description =
      "Regime-specific means; PAEG transition 1964-1967 excluded"
    
  ),
  
  
  extract_selected_rho(
    
    g_selected,
    
    parameter_name =
      "rho_g",
    
    sample_description =
      "1947-1973 and 1974-1980",
    
    treatment_description =
      "Separate pre/post-1974 regime means"
    
  ),
  
  
  extract_selected_rho(
    
    ig_selected,
    
    parameter_name =
      "rho_ig",
    
    sample_description =
      "1947-1978",
    
    treatment_description =
      "1979-1980 second-oil-shock/external-crisis boundary excluded"
    
  )
  
)


# Original detailed output
write.csv(
  selected_rhos,
  file.path(
    table_dir,
    "selected_fiscal_ar1_parameters.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 18.1 NEW — STANDARD PROJECT ESTIMATE OUTPUTS
# =============================================================================

write.csv(
  selected_rhos,
  file.path(
    standard_estimate_dir,
    "fiscal_persistence_ar1.csv"
  ),
  row.names = FALSE
)


fiscal_parameter_output <- data.frame(
  
  dynare_name =
    selected_rhos$parameter,
  
  estimate =
    selected_rhos$rho_quarterly_equivalent,
  
  standard_error =
    selected_rhos$rho_quarterly_se,
  
  shock_sd =
    NA_real_,
  
  annual_estimate =
    selected_rhos$rho_annual,
  
  annual_standard_error =
    selected_rhos$rho_annual_se,
  
  sample =
    selected_rhos$sample,
  
  method =
    paste0(
      "Regime-adjusted annual AR(1); ",
      "quarterly persistence = annual rho^(1/4)"
    ),
  
  stringsAsFactors = FALSE
  
)


write.csv(
  fiscal_parameter_output,
  file.path(
    standard_estimate_dir,
    "fiscal_persistence_parameters.csv"
  ),
  row.names = FALSE,
  na = ""
)


# =============================================================================
# 18.2 NEW — UPDATE PARAMETER REGISTRY
# =============================================================================

registry_path <- "docs/parameter_registry.csv"


if (
  !file.exists(
    registry_path
  )
) {
  
  stop(
    "docs/parameter_registry.csv was not found."
  )
  
}


registry <- read.csv(
  registry_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


required_registry_columns <- c(
  "dynare_name",
  "symbol",
  "block",
  "classification",
  "baseline_value",
  "research_value",
  "status",
  "method",
  "sample",
  "raw_or_primary_data",
  "source_or_literature",
  "standard_error",
  "shock_sd",
  "notes"
)


missing_registry_columns <- setdiff(
  required_registry_columns,
  names(registry)
)


if (
  length(
    missing_registry_columns
  ) > 0
) {
  
  stop(
    paste(
      "Parameter registry is missing columns:",
      paste(
        missing_registry_columns,
        collapse = ", "
      )
    )
  )
  
}


# Save all existing baseline values before updating anything
old_baselines <- registry$baseline_value
names(old_baselines) <- registry$dynare_name


# -----------------------------------------------------------------------------
# Helper: add parameter only when missing
# -----------------------------------------------------------------------------

add_parameter_if_missing <- function(
    registry,
    dynare_name,
    symbol,
    baseline_value
) {
  
  existing <- which(
    registry$dynare_name ==
      dynare_name
  )
  
  
  if (
    length(
      existing
    ) > 1
  ) {
    
    stop(
      paste(
        "Duplicate registry rows for",
        dynare_name
      )
    )
    
  }
  
  
  if (
    length(
      existing
    ) == 0
  ) {
    
    new_row <- registry[
      1,
      ,
      drop = FALSE
    ]
    
    new_row[1, ] <- NA
    
    new_row$dynare_name <-
      dynare_name
    
    new_row$symbol <-
      symbol
    
    new_row$block <-
      "Fiscal processes"
    
    new_row$classification <-
      "Estimated"
    
    new_row$baseline_value <-
      baseline_value
    
    new_row$status <-
      "TODO"
    
    
    registry <- rbind(
      registry,
      new_row
    )
    
  }
  
  
  registry
}


# rho_tau and rho_g had preliminary model values of 0.80.
# rho_ig was not previously implemented as a separate Dynare process.

registry <- add_parameter_if_missing(
  registry,
  "rho_tau",
  "rho_tau",
  0.80
)

registry <- add_parameter_if_missing(
  registry,
  "rho_g",
  "rho_g",
  0.80
)

registry <- add_parameter_if_missing(
  registry,
  "rho_ig",
  "rho_ig",
  NA_real_
)


# -----------------------------------------------------------------------------
# Update the three research estimates
# -----------------------------------------------------------------------------

for (
  parameter_name in fiscal_parameter_output$dynare_name
) {
  
  registry_row <- which(
    registry$dynare_name ==
      parameter_name
  )
  
  output_row <- fiscal_parameter_output[
    fiscal_parameter_output$dynare_name ==
      parameter_name,
  ]
  
  full_row <- selected_rhos[
    selected_rhos$parameter ==
      parameter_name,
  ]
  
  
  if (
    length(
      registry_row
    ) != 1
  ) {
    
    stop(
      paste(
        "Expected exactly one registry row for",
        parameter_name
      )
    )
    
  }
  
  
  registry$classification[
    registry_row
  ] <- "Estimated"
  
  
  registry$research_value[
    registry_row
  ] <- output_row$estimate
  
  
  registry$status[
    registry_row
  ] <- "FIRST-STAGE ESTIMATE COMPLETE"
  
  
  registry$method[
    registry_row
  ] <- paste0(
    "Regime-adjusted OLS AR(1) estimated from annual fiscal ratio; ",
    "quarterly persistence obtained as rho_q = rho_a^(1/4)."
  )
  
  
  registry$sample[
    registry_row
  ] <- output_row$sample
  
  
  registry$raw_or_primary_data[
    registry_row
  ] <- "data/raw/fiscal_raw_1947_1980.csv"
  
  
  registry$standard_error[
    registry_row
  ] <- output_row$standard_error
  
  
  # Annual fiscal shock SD is NOT mechanically converted to quarterly.
  registry$shock_sd[
    registry_row
  ] <- NA_real_
  
  
  if (
    parameter_name == "rho_tau"
  ) {
    
    registry$source_or_literature[
      registry_row
    ] <- paste0(
      "Brazil historical public-sector accounts; ",
      "global tax burden / GDP (IV.2)"
    )
    
  }
  
  
  if (
    parameter_name == "rho_g"
  ) {
    
    registry$source_or_literature[
      registry_row
    ] <- paste0(
      "Brazil historical public-sector accounts; ",
      "government consumption / GDP (IV.11)"
    )
    
  }
  
  
  if (
    parameter_name == "rho_ig"
  ) {
    
    registry$source_or_literature[
      registry_row
    ] <- paste0(
      "Brazil historical public-sector accounts; ",
      "government gross fixed capital formation / GDP (IV.14)"
    )
    
  }
  
  
  registry$notes[
    registry_row
  ] <- paste0(
    "Annual rho = ",
    round(
      full_row$rho_annual,
      6
    ),
    "; quarterly-equivalent rho = ",
    round(
      full_row$rho_quarterly_equivalent,
      6
    ),
    ". ",
    full_row$treatment,
    ". Detailed results: ",
    "output/estimates/fiscal_persistence_ar1.csv."
  )
  
}


# Add special implementation note to rho_ig
rho_ig_registry_row <- which(
  registry$dynare_name ==
    "rho_ig"
)

registry$notes[
  rho_ig_registry_row
] <- paste0(
  registry$notes[
    rho_ig_registry_row
  ],
  " Estimated now for later use; current Dynare model does not yet ",
  "contain a separate government-investment stochastic process."
)


# -----------------------------------------------------------------------------
# Verify previously existing baseline values were not changed
# -----------------------------------------------------------------------------

for (
  parameter_name in names(
    old_baselines
  )
) {
  
  old_value <- old_baselines[
    parameter_name
  ]
  
  new_value <- registry$baseline_value[
    registry$dynare_name ==
      parameter_name
  ]
  
  
  if (
    length(
      new_value
    ) == 1
  ) {
    
    if (
      is.na(
        old_value
      ) &&
      is.na(
        new_value
      )
    ) {
      
      next
      
    }
    
    
    if (
      !isTRUE(
        all.equal(
          as.numeric(
            old_value
          ),
          as.numeric(
            new_value
          ),
          check.attributes = FALSE
        )
      )
    ) {
      
      stop(
        paste(
          "Existing baseline_value was changed for",
          parameter_name
        )
      )
      
    }
    
  }
  
}


if (
  anyDuplicated(
    registry$dynare_name
  )
) {
  
  stop(
    "Duplicate Dynare parameter names detected in registry."
  )
  
}


write.csv(
  registry,
  registry_path,
  row.names = FALSE,
  na = ""
)


# =============================================================================
# 18.3 NEW — VERIFY STANDARD OUTPUTS AND REGISTRY
# =============================================================================

registry_check <- read.csv(
  registry_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


for (
  parameter_name in c(
    "rho_tau",
    "rho_g",
    "rho_ig"
  )
) {
  
  parameter_registry <- registry_check[
    registry_check$dynare_name ==
      parameter_name,
  ]
  
  parameter_output <- fiscal_parameter_output[
    fiscal_parameter_output$dynare_name ==
      parameter_name,
  ]
  
  
  if (
    nrow(
      parameter_registry
    ) != 1
  ) {
    
    stop(
      paste(
        "Registry verification failed for",
        parameter_name
      )
    )
    
  }
  
  
  if (
    !isTRUE(
      all.equal(
        as.numeric(
          parameter_registry$research_value
        ),
        as.numeric(
          parameter_output$estimate
        ),
        tolerance = 1e-10,
        check.attributes = FALSE
      )
    )
  ) {
    
    stop(
      paste(
        "Registry research_value does not match estimate for",
        parameter_name
      )
    )
    
  }
  
}


# =============================================================================
# 19. SAVE REGIME MEANS
# =============================================================================

tax_means <- tax_selected$regime_means
tax_means$series <- "tax"

g_means <- g_selected$regime_means
g_means$series <- "government_consumption"

ig_means <- ig_selected$regime_means
ig_means$series <- "government_investment"


regime_means <- rbind(
  tax_means,
  g_means,
  ig_means
)


regime_means <- regime_means[
  ,
  c(
    "series",
    "regime",
    "start_year",
    "end_year",
    "n",
    "mean_pct_gdp",
    "mean_share_gdp"
  )
]


write.csv(
  regime_means,
  file.path(
    table_dir,
    "fiscal_regime_means.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 20. PRE-OIL BASELINE: 1968-1973
# =============================================================================

pre_oil <- fiscal[
  fiscal$year >= 1968 &
    fiscal$year <= 1973,
]


pre_oil_baseline <- data.frame(
  
  parameter = c(
    "tax_ratio",
    "government_consumption_ratio",
    "government_investment_ratio"
  ),
  
  period = "1968-1973",
  
  mean_pct_gdp = c(
    
    mean(
      pre_oil$tax
    ),
    
    mean(
      pre_oil$g
    ),
    
    mean(
      pre_oil$ig
    )
    
  )
  
)


pre_oil_baseline$mean_share_gdp <-
  pre_oil_baseline$mean_pct_gdp / 100


write.csv(
  pre_oil_baseline,
  file.path(
    table_dir,
    "pre_oil_1968_1973_fiscal_means.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 21. SAVE SELECTED MODEL RESIDUALS
# =============================================================================

make_residual_table <- function(
    fit_object,
    series_name
) {
  
  d <- fit_object$ar_data
  
  d$series <- series_name
  
  d$fitted <- fitted(
    fit_object$model
  )
  
  d$residual <- residuals(
    fit_object$model
  )
  
  d
  
}


selected_residuals <- rbind(
  
  make_residual_table(
    tax_selected,
    "tax"
  ),
  
  make_residual_table(
    g_selected,
    "government_consumption"
  ),
  
  make_residual_table(
    ig_selected,
    "government_investment"
  )
  
)


write.csv(
  selected_residuals,
  file.path(
    table_dir,
    "selected_model_residuals.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 22. COMPARISON: NAIVE VS SELECTED
# =============================================================================

comparison_table <- data.frame(
  
  parameter = c(
    "tax",
    "government_consumption",
    "government_investment"
  ),
  
  naive_rho_annual = c(
    
    coef(
      tax_naive$model
    )["lag"],
    
    coef(
      g_naive$model
    )["lag"],
    
    coef(
      ig_naive$model
    )["lag"]
    
  ),
  
  selected_rho_annual =
    selected_rhos$rho_annual,
  
  selected_rho_quarterly =
    selected_rhos$rho_quarterly_equivalent,
  
  stringsAsFactors = FALSE
)


comparison_table$naive_minus_selected <-
  comparison_table$naive_rho_annual -
  comparison_table$selected_rho_annual


write.csv(
  comparison_table,
  file.path(
    table_dir,
    "naive_vs_selected_rho.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 23. DYNARE-READY PERSISTENCE FILE
# =============================================================================

rho_tax_a <-
  selected_rhos$rho_annual[
    selected_rhos$parameter ==
      "rho_tau"
  ]

rho_tax_q <-
  selected_rhos$rho_quarterly_equivalent[
    selected_rhos$parameter ==
      "rho_tau"
  ]


rho_g_a <-
  selected_rhos$rho_annual[
    selected_rhos$parameter ==
      "rho_g"
  ]

rho_g_q <-
  selected_rhos$rho_quarterly_equivalent[
    selected_rhos$parameter ==
      "rho_g"
  ]


rho_ig_a <-
  selected_rhos$rho_annual[
    selected_rhos$parameter ==
      "rho_ig"
  ]

rho_ig_q <-
  selected_rhos$rho_quarterly_equivalent[
    selected_rhos$parameter ==
      "rho_ig"
  ]


dynare_lines <- c(
  
  "// ============================================================",
  "// Fiscal persistence estimates",
  "// Estimated from annual Brazilian fiscal data, 1947-1980",
  "// ============================================================",
  "",
  "// Original annual persistence estimates:",
  sprintf(
    "// rho_tau_annual = %.6f",
    rho_tax_a
  ),
  sprintf(
    "// rho_g_annual   = %.6f",
    rho_g_a
  ),
  sprintf(
    "// rho_ig_annual  = %.6f",
    rho_ig_a
  ),
  "",
  "// Quarterly-equivalent persistence for quarterly Dynare model:",
  sprintf(
    "rho_tau = %.6f;",
    rho_tax_q
  ),
  sprintf(
    "rho_g   = %.6f;",
    rho_g_q
  ),
  sprintf(
    "rho_ig  = %.6f;",
    rho_ig_q
  ),
  "",
  "// Conversion:",
  "// rho_quarterly = rho_annual^(1/4)",
  "",
  "// IMPORTANT:",
  "// Annual innovation standard deviations are intentionally",
  "// NOT converted automatically into quarterly shock std devs."
)


writeLines(
  dynare_lines,
  file.path(
    output_dir,
    "fiscal_persistence_dynare.txt"
  )
)


# =============================================================================
# 24. SAVE SESSION INFORMATION
# =============================================================================

capture.output(
  sessionInfo(),
  file = file.path(
    output_dir,
    "session_info.txt"
  )
)


# =============================================================================
# 25. PRINT FINAL RESULTS
# =============================================================================

cat("\n\n")
cat("============================================================\n")
cat("SELECTED FISCAL AR(1) RESULTS\n")
cat("============================================================\n\n")


print(
  selected_rhos[
    ,
    c(
      "parameter",
      "rho_annual",
      "rho_annual_se",
      "rho_annual_ci_low",
      "rho_annual_ci_high",
      "rho_annual_p_value",
      "rho_quarterly_equivalent",
      "rho_quarterly_se",
      "annual_innovation_sd_pct_points"
    )
  ],
  row.names = FALSE
)


cat("\n\n")
cat("============================================================\n")
cat("NAIVE VS SELECTED\n")
cat("============================================================\n\n")


print(
  comparison_table,
  row.names = FALSE
)


cat("\n\n")
cat("============================================================\n")
cat("REGIME MEANS\n")
cat("============================================================\n\n")


print(
  regime_means,
  row.names = FALSE
)


cat("\n\n")
cat("============================================================\n")
cat("1968-1973 PRE-OIL BASELINE MEANS\n")
cat("============================================================\n\n")


print(
  pre_oil_baseline,
  row.names = FALSE
)


cat("\n\n")
cat("============================================================\n")
cat("HISTORICAL BREAK CHECKS\n")
cat("============================================================\n\n")


print(
  historical_break_tests,
  row.names = FALSE
)


cat("\n\n")
cat("============================================================\n")
cat("STANDARD PARAMETER OUTPUT\n")
cat("============================================================\n\n")


print(
  fiscal_parameter_output,
  row.names = FALSE
)


cat("\n\n")
cat("============================================================\n")
cat("FILES CREATED\n")
cat("============================================================\n\n")


cat(
  "Standard AR results:\n",
  "output/estimates/fiscal_persistence_ar1.csv\n\n"
)

cat(
  "Standard parameter output:\n",
  "output/estimates/fiscal_persistence_parameters.csv\n\n"
)

cat(
  "Parameter registry updated:\n",
  "docs/parameter_registry.csv\n\n"
)

cat(
  "Detailed diagnostic directory:\n",
  output_dir,
  "\n\n"
)

cat(
  "Dynare persistence block:\n",
  file.path(
    output_dir,
    "fiscal_persistence_dynare.txt"
  ),
  "\n\n"
)

cat(
  "Graph:\n",
  file.path(
    figure_dir,
    "fiscal_ratios_1947_1980.png"
  ),
  "\n\n"
)


cat("============================================================\n")
cat("ESTIMATION COMPLETE\n")
cat("============================================================\n")
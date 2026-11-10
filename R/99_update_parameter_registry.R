# =============================================================================
# 99_update_parameter_registry.R
# =============================================================================
#
# PURPOSE
# -------
# Sole writer of:
#
#   docs/parameter_registry.csv
#
# This script consolidates machine-readable parameter outputs produced by
# Scripts 01-08. Parameter-estimation/calibration scripts must NOT write the
# registry directly.
#
# ARCHITECTURE
# ------------
# Inputs:
#   01 -> output/estimates/world_interest_rate_parameters.csv
#   02 -> output/estimates/foreign_output_parameters.csv
#   03 -> output/estimates/Pi_ss_calibration.csv
#   04 -> output/estimates/fiscal_persistence_parameters.csv
#   04i -> output/estimates/steady_state_fiscal_share_parameters.csv
#   05 -> output/estimates/foreign_price_process_parameters.csv
#   06 -> output/estimates/trade_steady_state_share_parameters.csv
#   07 -> output/estimates/alpha_mu_lambda_s_parameters.csv
#   08 -> output/estimates/external_financing_final_parameter_candidates.csv
#
# Registry cleanup:
#   - keep canonical fiscal Dynare names:
#       rhotau, rhoG, rhoIG
#   - remove obsolete aliases:
#       rho_tau, rho_g, rho_ig
#   - remove phiDD completely
#
# SAFETY RULES
# ------------
#   - baseline_value is never modified for retained parameters.
#   - exactly one registry row must exist for every target parameter.
#   - Script 04i must use the corrected 1968-1972 baseline.
#   - cH_y must be consistent with x_y, g_y and ig_y.
#   - Script 04 annual rho is optional in the compact output; when absent,
#     it is reconstructed for documentation as rho_quarterly^4.
#   - lambda_s must be consistent with alpha and mu.
#   - phiDD must not appear in Script 08 output.
#
# BASE R ONLY.
# =============================================================================

rm(list = ls())

options(stringsAsFactors = FALSE)

# =============================================================================
# 0. PATHS
# =============================================================================

registry_file <- file.path(
  "docs",
  "parameter_registry.csv"
)

estimate_dir <- file.path(
  "output",
  "estimates"
)

files <- list(
  world_rate = file.path(
    estimate_dir,
    "world_interest_rate_parameters.csv"
  ),
  foreign_output = file.path(
    estimate_dir,
    "foreign_output_parameters.csv"
  ),
  inflation = file.path(
    estimate_dir,
    "Pi_ss_calibration.csv"
  ),
  fiscal_persistence = file.path(
    estimate_dir,
    "fiscal_persistence_parameters.csv"
  ),
  fiscal_shares = file.path(
    estimate_dir,
    "steady_state_fiscal_share_parameters.csv"
  ),
  foreign_price = file.path(
    estimate_dir,
    "foreign_price_process_parameters.csv"
  ),
  trade_shares = file.path(
    estimate_dir,
    "trade_steady_state_share_parameters.csv"
  ),
  openness = file.path(
    estimate_dir,
    "alpha_mu_lambda_s_parameters.csv"
  ),
  external_financing = file.path(
    estimate_dir,
    "external_financing_final_parameter_candidates.csv"
  )
)

if (!file.exists(registry_file)) {
  stop(
    "Cannot find ",
    registry_file,
    ". Open the project from the repository root."
  )
}

missing_inputs <- names(files)[
  !vapply(
    files,
    file.exists,
    logical(1)
  )
]

if (length(missing_inputs) > 0) {
  stop(
    "Missing parameter-output file(s):\n - ",
    paste(
      unlist(files[missing_inputs]),
      collapse = "\n - "
    ),
    "\nRun the corresponding scripts before Script 99."
  )
}

# =============================================================================
# 1. HELPERS
# =============================================================================

read_project_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

one_row <- function(
    df,
    key_col,
    key_value,
    source_label
) {
  
  if (!(key_col %in% names(df))) {
    stop(
      source_label,
      " does not contain required key column '",
      key_col,
      "'."
    )
  }
  
  idx <- which(
    trimws(
      as.character(
        df[[key_col]]
      )
    ) == key_value
  )
  
  if (length(idx) != 1) {
    stop(
      "Expected exactly one ",
      key_value,
      " row in ",
      source_label,
      "; found ",
      length(idx),
      "."
    )
  }
  
  df[idx, , drop = FALSE]
}

numeric_scalar <- function(
    x,
    label
) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  if (
    length(value) != 1 ||
    !is.finite(value)
  ) {
    stop(
      label,
      " is not one finite numeric value."
    )
  }
  
  value
}

first_existing_value <- function(
    row,
    candidates,
    default = NULL
) {
  
  found <- candidates[
    candidates %in% names(row)
  ]
  
  if (length(found) == 0) {
    return(default)
  }
  
  value <- row[[found[1]]][1]
  
  if (
    length(value) == 0 ||
    is.na(value)
  ) {
    return(default)
  }
  
  value
}

set_if_requested <- function(
    df,
    idx,
    column,
    value
) {
  
  if (
    !is.null(value) &&
    column %in% names(df)
  ) {
    df[idx, column] <- value
  }
  
  df
}

# =============================================================================
# 2. READ REGISTRY + CLEAN OBSOLETE ROWS
# =============================================================================

registry <- read_project_csv(
  registry_file
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

if (length(missing_registry_columns) > 0) {
  stop(
    "Parameter registry is missing required column(s): ",
    paste(
      missing_registry_columns,
      collapse = ", "
    )
  )
}

if (anyDuplicated(registry$dynare_name)) {
  duplicated_names <- unique(
    registry$dynare_name[
      duplicated(registry$dynare_name)
    ]
  )
  
  stop(
    "Registry contains duplicate exact dynare_name values before cleanup: ",
    paste(
      duplicated_names,
      collapse = ", "
    )
  )
}

baseline_before <- setNames(
  registry$baseline_value,
  registry$dynare_name
)

obsolete_names <- c(
  "rho_tau",
  "rho_g",
  "rho_ig",
  "phiDD"
)

removed_rows <- registry[
  registry$dynare_name %in% obsolete_names,
  ,
  drop = FALSE
]

registry <- registry[
  !(registry$dynare_name %in% obsolete_names),
  ,
  drop = FALSE
]

row.names(registry) <- NULL

# Canonical rows that must survive cleanup.
canonical_fiscal <- c(
  "rhotau",
  "rhoG",
  "rhoIG"
)

for (p in canonical_fiscal) {
  if (sum(registry$dynare_name == p) != 1) {
    stop(
      "Expected exactly one canonical fiscal registry row for ",
      p,
      "."
    )
  }
}

if ("phiDD" %in% registry$dynare_name) {
  stop("phiDD cleanup failed.")
}

# =============================================================================
# 3. REGISTRY UPDATE HELPER
# =============================================================================

updated_names <- character(0)

update_parameter <- function(
    registry,
    name,
    value,
    classification = NULL,
    status = NULL,
    method = NULL,
    sample = NULL,
    raw_or_primary_data = NULL,
    source_or_literature = NULL,
    standard_error = NULL,
    shock_sd = NULL,
    notes = NULL
) {
  
  idx <- which(
    registry$dynare_name == name
  )
  
  if (length(idx) != 1) {
    stop(
      "Expected exactly one registry row for ",
      name,
      "; found ",
      length(idx),
      "."
    )
  }
  
  value <- numeric_scalar(
    value,
    paste0(name, " research value")
  )
  
  registry$research_value[idx] <- value
  
  registry <- set_if_requested(
    registry,
    idx,
    "classification",
    classification
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "status",
    status
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "method",
    method
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "sample",
    sample
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "raw_or_primary_data",
    raw_or_primary_data
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "source_or_literature",
    source_or_literature
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "standard_error",
    standard_error
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "shock_sd",
    shock_sd
  )
  
  registry <- set_if_requested(
    registry,
    idx,
    "notes",
    notes
  )
  
  updated_names <<- unique(
    c(
      updated_names,
      name
    )
  )
  
  registry
}

# =============================================================================
# 4. SCRIPT 01 — WORLD INTEREST RATE
# =============================================================================

world_rate <- read_project_csv(
  files$world_rate
)

rhoiw <- one_row(
  world_rate,
  "dynare_name",
  "rhoiw",
  basename(files$world_rate)
)

rstar_ss <- one_row(
  world_rate,
  "dynare_name",
  "rstar_ss",
  basename(files$world_rate)
)

registry <- update_parameter(
  registry,
  "rhoiw",
  rhoiw$estimate,
  classification = "Estimated",
  status = rhoiw$status,
  method = rhoiw$method,
  sample = rhoiw$sample,
  raw_or_primary_data = "data/raw/TB3MS.csv",
  source_or_literature = "FRED series TB3MS",
  standard_error = rhoiw$standard_error,
  shock_sd = rhoiw$shock_sd,
  notes = paste(
    rhoiw$notes,
    "Robustness estimates: output/estimates/world_interest_rate_ar1.csv."
  )
)

registry <- update_parameter(
  registry,
  "rstar_ss",
  rstar_ss$estimate,
  classification = "Fixed / derived",
  status = rstar_ss$status,
  method = rstar_ss$method,
  sample = rstar_ss$sample,
  raw_or_primary_data = "data/raw/TB3MS.csv",
  source_or_literature = "FRED series TB3MS",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = paste(
    rstar_ss$notes,
    "Derived from the same recommended pre-shock AR(1) used for rhoiw."
  )
)

# =============================================================================
# 5. SCRIPT 02 — FOREIGN OUTPUT
# =============================================================================

foreign_output <- read_project_csv(
  files$foreign_output
)

rhoystar <- one_row(
  foreign_output,
  "dynare_name",
  "rhoystar",
  basename(files$foreign_output)
)

registry <- update_parameter(
  registry,
  "rhoystar",
  rhoystar$estimate,
  classification = "Estimated",
  status = rhoystar$status,
  method = rhoystar$method,
  sample = rhoystar$sample,
  raw_or_primary_data = "data/raw/GDPC1.csv",
  source_or_literature = "FRED GDPC1 / U.S. Bureau of Economic Analysis",
  standard_error = rhoystar$standard_error,
  shock_sd = rhoystar$shock_sd,
  notes = paste(
    rhoystar$notes,
    paste0(
      "Detrending window: ",
      rhoystar$detrending_sample,
      ". Robustness estimates: output/estimates/foreign_output_ar1.csv."
    )
  )
)

# =============================================================================
# 6. SCRIPT 03 — STEADY-STATE INFLATION
# =============================================================================

inflation <- read_project_csv(
  files$inflation
)

if ("recommended_baseline" %in% names(inflation)) {
  
  baseline_inflation <- inflation[
    toupper(
      trimws(
        as.character(
          inflation$recommended_baseline
        )
      )
    ) == "YES",
    ,
    drop = FALSE
  ]
  
} else {
  
  baseline_inflation <- inflation[
    inflation$sample == "1968Q1-1972Q4",
    ,
    drop = FALSE
  ]
}

if (nrow(baseline_inflation) != 1) {
  stop(
    "Expected exactly one Script 03 baseline row for 1968Q1-1972Q4."
  )
}

Pi_ss_value <- numeric_scalar(
  baseline_inflation$Pi_ss,
  "Pi_ss"
)

invPi_value <- numeric_scalar(
  baseline_inflation$invPi,
  "invPi"
)

if (
  abs(
    invPi_value -
    1 / Pi_ss_value
  ) > 1e-12
) {
  stop(
    "Script 03 output fails invPi = 1/Pi_ss."
  )
}

registry <- update_parameter(
  registry,
  "Pi_ss",
  Pi_ss_value,
  classification = "Calibrated",
  status = "CALIBRATION COMPLETE",
  method = "Geometric mean of quarterly gross IGPC-Mtb inflation",
  sample = "1968Q1-1972Q4",
  raw_or_primary_data = "data/raw/IGPC_ipeadata.csv",
  source_or_literature = "IPEAdata / IGPC-Mtb series GAMMA12_IGPCMTB12",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = paste(
    "Recommended pre-oil-shock baseline.",
    "Robustness windows: 1967Q1-1972Q4 and 1964Q1-1972Q4.",
    "See output/estimates/Pi_ss_calibration.csv."
  )
)

registry <- update_parameter(
  registry,
  "invPi",
  invPi_value,
  classification = "Fixed / derived",
  status = "DERIVED FROM Pi_ss",
  method = "invPi = 1 / Pi_ss",
  sample = "Derived from Pi_ss",
  raw_or_primary_data = "Derived from Pi_ss",
  source_or_literature = "Model identity",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = "Mechanically derived from calibrated Pi_ss; not independently estimated."
)

# =============================================================================
# 7. SCRIPT 04 — FISCAL PERSISTENCE
# =============================================================================

fiscal_persistence <- read_project_csv(
  files$fiscal_persistence
)

expected_fiscal_names <- c(
  "rhotau",
  "rhoG",
  "rhoIG"
)

if (
  !setequal(
    trimws(
      as.character(
        fiscal_persistence$dynare_name
      )
    ),
    expected_fiscal_names
  )
) {
  stop(
    paste(
      "Script 04 output must use exactly canonical Dynare names:",
      paste(expected_fiscal_names, collapse = ", "),
      "and must not use rho_tau/rho_g/rho_ig."
    )
  )
}

fiscal_source_map <- list(
  rhotau = list(
    raw = "data/raw/fiscal_raw_1947_1980.csv",
    source = "Brazil historical public-sector accounts; global tax burden / GDP",
    note = "Regime-specific means; PAEG transition 1964-1967 excluded."
  ),
  rhoG = list(
    raw = "data/raw/fiscal_raw_1947_1980.csv",
    source = "Brazil historical public-sector accounts; government consumption / GDP",
    note = "Separate 1947-1973 and 1974-1980 regime means."
  ),
  rhoIG = list(
    raw = "data/raw/fiscal_raw_1947_1980.csv",
    source = "Brazil historical public-sector accounts; government gross fixed capital formation / GDP",
    note = "1979-1980 second-oil-shock/external-crisis boundary excluded."
  )
)

for (p in expected_fiscal_names) {
  
  row <- one_row(
    fiscal_persistence,
    "dynare_name",
    p,
    basename(files$fiscal_persistence)
  )
  
  quarterly_rho <- numeric_scalar(
    first_existing_value(
      row,
      c(
        "estimate",
        "rho_quarterly_equivalent",
        "rho_quarterly"
      ),
      NA_real_
    ),
    paste0(p, " quarterly estimate")
  )
  
  quarterly_se_raw <- first_existing_value(
    row,
    c(
      "standard_error",
      "rho_quarterly_se",
      "quarterly_standard_error"
    ),
    NA_real_
  )
  
  quarterly_se <- suppressWarnings(
    as.numeric(
      quarterly_se_raw
    )
  )
  
  if (
    length(quarterly_se) != 1 ||
    !is.finite(quarterly_se)
  ) {
    quarterly_se <- NA_real_
  }
  
  annual_rho_raw <- first_existing_value(
    row,
    c(
      "annual_estimate",
      "rho_annual",
      "annual_rho"
    ),
    NA_real_
  )
  
  annual_rho <- suppressWarnings(
    as.numeric(
      annual_rho_raw
    )
  )
  
  # Script 04 defines:
  #
  #   rho_quarterly = rho_annual^(1/4)
  #
  # Therefore, if the compact parameter-output file does not carry the
  # annual estimate explicitly, recover it exactly from the accepted
  # quarterly model-facing estimate. The annual value is used here only
  # for documentation; it is NOT a separate registry parameter.
  if (
    length(annual_rho) != 1 ||
    !is.finite(annual_rho)
  ) {
    annual_rho <- quarterly_rho^4
  }
  
  fiscal_method <- first_existing_value(
    row,
    c("method"),
    "Regime-adjusted annual AR(1); quarterly persistence = annual rho^(1/4)"
  )
  
  fiscal_sample <- first_existing_value(
    row,
    c("sample"),
    ""
  )
  
  registry <- update_parameter(
    registry,
    p,
    quarterly_rho,
    classification = "Estimated",
    status = "FIRST-STAGE ESTIMATE COMPLETE",
    method = fiscal_method,
    sample = fiscal_sample,
    raw_or_primary_data = fiscal_source_map[[p]]$raw,
    source_or_literature = fiscal_source_map[[p]]$source,
    standard_error = quarterly_se,
    shock_sd = NA_real_,
    notes = paste0(
      "Annual rho=",
      format(
        annual_rho,
        digits = 8
      ),
      "; quarterly rho=",
      format(
        quarterly_rho,
        digits = 12
      ),
      ". ",
      fiscal_source_map[[p]]$note,
      " Detailed results: output/estimates/fiscal_persistence_ar1.csv."
    )
  )
}

# =============================================================================
# 8. SCRIPT 04i — FISCAL STEADY-STATE SHARES
# =============================================================================

fiscal_shares <- read_project_csv(
  files$fiscal_shares
)

for (p in c("g_y", "ig_y")) {
  
  row <- one_row(
    fiscal_shares,
    "dynare_name",
    p,
    basename(files$fiscal_shares)
  )
  
  if (
    !("sample" %in% names(row)) ||
    trimws(
      as.character(
        row$sample[1]
      )
    ) != "1968-1972"
  ) {
    stop(
      "Script 04i output for ",
      p,
      " is not using the corrected 1968-1972 baseline."
    )
  }
}

g_y_row <- one_row(
  fiscal_shares,
  "dynare_name",
  "g_y",
  basename(files$fiscal_shares)
)

ig_y_row <- one_row(
  fiscal_shares,
  "dynare_name",
  "ig_y",
  basename(files$fiscal_shares)
)

g_y_value <- numeric_scalar(
  g_y_row$estimate,
  "g_y"
)

ig_y_value <- numeric_scalar(
  ig_y_row$estimate,
  "ig_y"
)

if (abs(g_y_value - 0.110279410789871) > 1e-10) {
  stop(
    "g_y differs from the accepted corrected 1968-1972 value."
  )
}

if (abs(ig_y_value - 0.044702876856991) > 1e-10) {
  stop(
    "ig_y differs from the accepted corrected 1968-1972 value."
  )
}

registry <- update_parameter(
  registry,
  "g_y",
  g_y_value,
  classification = "Calibrated",
  status = "CALIBRATION COMPLETE",
  method = g_y_row$method,
  sample = "1968-1972",
  raw_or_primary_data = "data/raw/fiscal_raw_1947_1980.csv",
  source_or_literature = "Brazil historical public-sector accounts; government consumption / GDP",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = paste0(
    "Preferred baseline uses 1968-1972. ",
    "Mean percent of GDP=",
    format(
      first_existing_value(
        g_y_row,
        c("mean_percent_gdp"),
        100 * g_y_value
      ),
      digits = 8
    ),
    ". 1968-1973 retained only as robustness."
  )
)

registry <- update_parameter(
  registry,
  "ig_y",
  ig_y_value,
  classification = "Calibrated",
  status = "CALIBRATION COMPLETE",
  method = ig_y_row$method,
  sample = "1968-1972",
  raw_or_primary_data = "data/raw/fiscal_raw_1947_1980.csv",
  source_or_literature = "Brazil historical public-sector accounts; government gross fixed capital formation / GDP",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = paste0(
    "Preferred baseline uses 1968-1972. ",
    "Mean percent of GDP=",
    format(
      first_existing_value(
        ig_y_row,
        c("mean_percent_gdp"),
        100 * ig_y_value
      ),
      digits = 8
    ),
    ". 1968-1973 retained only as robustness."
  )
)

# =============================================================================
# 9. SCRIPT 05 — FOREIGN PRICE PROCESS
# =============================================================================

foreign_price <- read_project_csv(
  files$foreign_price
)

rhoF <- one_row(
  foreign_price,
  "dynare_name",
  "rhoF",
  basename(files$foreign_price)
)

registry <- update_parameter(
  registry,
  "rhoF",
  rhoF$estimate,
  classification = "Estimated",
  status = rhoF$status,
  method = rhoF$method,
  sample = rhoF$sample,
  raw_or_primary_data = rhoF$raw_or_primary_data,
  source_or_literature = rhoF$source_or_literature,
  standard_error = rhoF$standard_error,
  shock_sd = rhoF$shock_sd,
  notes = rhoF$notes
)

# =============================================================================
# 10. SCRIPT 06 — TRADE / STEADY-STATE SHARES
# =============================================================================

trade_shares <- read_project_csv(
  files$trade_shares
)

for (p in c("x_y", "cf_y", "z_y", "cH_y")) {
  
  row <- one_row(
    trade_shares,
    "parameter",
    p,
    basename(files$trade_shares)
  )
  
  value <- numeric_scalar(
    row$value,
    p
  )
  
  classification <- if (p == "cH_y") {
    "Derived"
  } else {
    "Calibrated"
  }
  
  status <- if (p == "cH_y") {
    "DERIVED FROM CORRECTED STEADY-STATE SHARES"
  } else {
    "CALIBRATION COMPLETE"
  }
  
  source_text <- if (p == "x_y") {
    "IBGE historical national accounts I.2.1"
  } else if (p == "cH_y") {
    "Script 06 identity using x_y plus Script 04i g_y and ig_y"
  } else {
    "IBGE historical national accounts I.2.1; Doellinger et al. (1974), Table VI.2"
  }
  
  registry <- update_parameter(
    registry,
    p,
    value,
    classification = classification,
    status = status,
    method = row$method,
    sample = row$sample,
    raw_or_primary_data = source_text,
    source_or_literature = source_text,
    standard_error = NA_real_,
    shock_sd = NA_real_,
    notes = first_existing_value(
      row,
      c("interpretation"),
      ""
    )
  )
}

x_y_value <- numeric_scalar(
  one_row(
    trade_shares,
    "parameter",
    "x_y",
    basename(files$trade_shares)
  )$value,
  "x_y"
)

cH_y_value <- numeric_scalar(
  one_row(
    trade_shares,
    "parameter",
    "cH_y",
    basename(files$trade_shares)
  )$value,
  "cH_y"
)

cH_identity <- 1 -
  x_y_value -
  g_y_value -
  ig_y_value

if (
  abs(
    cH_y_value -
    cH_identity
  ) > 1e-10
) {
  stop(
    "cH_y fails the identity 1 - x_y - g_y - ig_y."
  )
}

if (
  abs(
    cH_y_value -
    0.7781620666
  ) > 1e-8
) {
  stop(
    "cH_y differs from the accepted corrected target near 0.7781620666."
  )
}

# =============================================================================
# 11. SCRIPT 07 — alpha, mu, lambda_s
# =============================================================================

openness <- read_project_csv(
  files$openness
)

for (p in c("alpha", "mu", "lambda_s")) {
  
  row <- one_row(
    openness,
    "parameter",
    p,
    basename(files$openness)
  )
  
  registry <- update_parameter(
    registry,
    p,
    row$value,
    classification = row$classification,
    status = if (p == "lambda_s") {
      "DERIVED FROM alpha AND mu"
    } else {
      "CALIBRATION COMPLETE"
    },
    method = row$method,
    sample = row$sample,
    raw_or_primary_data = if (p == "alpha") {
      "Script 06 trade shares + IBGE household/private consumption"
    } else if (p == "mu") {
      "Script 06 z_y + Miebach & Marquetti (2022) labor-share transcription"
    } else {
      "Derived from Script 07 alpha and mu"
    },
    source_or_literature = if (p == "mu") {
      "Doellinger et al. (1974); Miebach & Marquetti (2022)"
    } else {
      "IBGE historical national accounts; current model algebra"
    },
    standard_error = NA_real_,
    shock_sd = NA_real_,
    notes = if (p == "lambda_s") {
      "Exact model identity lambda_s = mu + (1-mu)*alpha."
    } else {
      "Baseline calibration sample: 1968-1972."
    }
  )
}

alpha_value <- numeric_scalar(
  one_row(
    openness,
    "parameter",
    "alpha",
    basename(files$openness)
  )$value,
  "alpha"
)

mu_value <- numeric_scalar(
  one_row(
    openness,
    "parameter",
    "mu",
    basename(files$openness)
  )$value,
  "mu"
)

lambda_s_value <- numeric_scalar(
  one_row(
    openness,
    "parameter",
    "lambda_s",
    basename(files$openness)
  )$value,
  "lambda_s"
)

lambda_identity <- mu_value +
  (1 - mu_value) *
  alpha_value

if (
  abs(
    lambda_s_value -
    lambda_identity
  ) > 1e-10
) {
  stop(
    "lambda_s fails the identity mu + (1-mu)*alpha."
  )
}

# =============================================================================
# 12. SCRIPT 08 — EXTERNAL DEBT / FINANCING WEDGE
# =============================================================================

external_financing <- read_project_csv(
  files$external_financing
)

if (
  "phiDD" %in%
  trimws(
    as.character(
      external_financing$parameter
    )
  )
) {
  stop(
    "phiDD still appears in Script 08 parameter output."
  )
}

dbar_row <- one_row(
  external_financing,
  "parameter",
  "dbar",
  basename(files$external_financing)
)

phiD_row <- one_row(
  external_financing,
  "parameter",
  "phiD",
  basename(files$external_financing)
)

dbar_value <- numeric_scalar(
  dbar_row$value,
  "dbar"
)

phiD_value <- numeric_scalar(
  phiD_row$value,
  "phiD"
)

if (
  abs(
    dbar_value -
    0.06203431
  ) > 1e-7
) {
  stop(
    "dbar differs from the accepted 1970-1972 net-debt/GDP baseline."
  )
}

if (
  abs(
    phiD_value -
    0.001
  ) > 1e-12
) {
  stop(
    "phiD differs from the accepted quarterly literature calibration 0.001."
  )
}

registry <- update_parameter(
  registry,
  "dbar",
  dbar_value,
  classification = "Calibrated",
  status = "CALIBRATION COMPLETE",
  method = "Arithmetic mean of net external debt/GDP over 1970-1972",
  sample = "1970-1972",
  raw_or_primary_data = "data/processed/brazil_external_debt_master_1964_1980.csv",
  source_or_literature = "Serra (1982); Bresser-Pereira (1983); BCB historical external-debt database",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = "Net external debt/GDP baseline; gross debt/GDP retained as robustness."
)

registry <- update_parameter(
  registry,
  "phiD",
  phiD_value,
  classification = "Calibrated",
  status = "LITERATURE CALIBRATION COMPLETE",
  method = "Quarterly small-open-economy debt-elastic premium calibration",
  sample = "Structural quarterly calibration",
  raw_or_primary_data = "Not estimated from the short 1970s Brazil spread sample",
  source_or_literature = "Schmitt-Grohe & Uribe-style small SOE debt-elastic premium calibration",
  standard_error = NA_real_,
  shock_sd = NA_real_,
  notes = paste(
    "Stationarity / long-run debt-discipline parameter.",
    "Sensitivity values: 0.0001 and 0.01.",
    "Historical financing variation is disciplined through eps_rp."
  )
)

# =============================================================================
# 13. FINAL SAFETY CHECKS
# =============================================================================

# No obsolete aliases or phiDD.
for (p in obsolete_names) {
  if (p %in% registry$dynare_name) {
    stop(
      "Obsolete registry row still present: ",
      p
    )
  }
}

# No exact duplicate Dynare names.
if (anyDuplicated(registry$dynare_name)) {
  stop(
    "Duplicate dynare_name values remain after consolidation."
  )
}

# Every updated parameter must be unique.
for (p in updated_names) {
  if (sum(registry$dynare_name == p) != 1) {
    stop(
      "Updated parameter is not unique in final registry: ",
      p
    )
  }
}

# baseline_value must not change for retained rows.
baseline_after <- setNames(
  registry$baseline_value,
  registry$dynare_name
)

retained_before_names <- intersect(
  names(baseline_before),
  names(baseline_after)
)

if (
  !isTRUE(
    all.equal(
      baseline_before[retained_before_names],
      baseline_after[retained_before_names],
      check.attributes = FALSE
    )
  )
) {
  stop(
    paste(
      "SAFETY FAILURE: baseline_value changed for one or more retained",
      "registry parameters. Registry was NOT saved."
    )
  )
}

# Hard target checks for the corrected fiscal/trade architecture.
final_value <- function(name) {
  idx <- which(
    registry$dynare_name == name
  )
  
  numeric_scalar(
    registry$research_value[idx],
    paste0(name, " final registry value")
  )
}

target_checks <- c(
  Pi_ss = 1.048995251092,
  invPi = 0.953293162156,
  rhotau = 0.902332736768704,
  rhoG = 0.697503334451888,
  rhoIG = 0.864304193118051,
  g_y = 0.110279410789871,
  ig_y = 0.044702876856991,
  cH_y = 0.7781620666,
  rhoF = 0.723969948147,
  alpha = 0.01513018,
  mu = 0.06827873,
  lambda_s = 0.08237584,
  phiD = 0.001
)

target_tolerance <- c(
  Pi_ss = 1e-10,
  invPi = 1e-10,
  rhotau = 1e-12,
  rhoG = 1e-12,
  rhoIG = 1e-12,
  g_y = 1e-12,
  ig_y = 1e-12,
  cH_y = 1e-8,
  rhoF = 1e-12,
  alpha = 1e-7,
  mu = 1e-7,
  lambda_s = 1e-7,
  phiD = 1e-12
)

for (p in names(target_checks)) {
  
  observed <- final_value(p)
  
  if (
    abs(
      observed -
      target_checks[p]
    ) >
    target_tolerance[p]
  ) {
    stop(
      "Final target check failed for ",
      p,
      ": observed=",
      format(observed, digits = 15),
      ", expected approximately=",
      format(target_checks[p], digits = 15),
      "."
    )
  }
}

# =============================================================================
# 14. SAVE REGISTRY + AUDIT OUTPUT
# =============================================================================

write.csv(
  registry,
  registry_file,
  row.names = FALSE,
  na = ""
)

audit_rows <- registry[
  registry$dynare_name %in% updated_names,
  c(
    "dynare_name",
    "classification",
    "baseline_value",
    "research_value",
    "status",
    "sample",
    "standard_error",
    "shock_sd"
  ),
  drop = FALSE
]

audit_rows <- audit_rows[
  match(
    updated_names,
    audit_rows$dynare_name
  ),
  ,
  drop = FALSE
]

write.csv(
  audit_rows,
  file.path(
    estimate_dir,
    "parameter_registry_update_audit.csv"
  ),
  row.names = FALSE,
  na = ""
)

# Remove obsolete legacy output if present.
obsolete_file <- file.path(
  estimate_dir,
  "rhoiw_for_dynare.csv"
)

if (file.exists(obsolete_file)) {
  removed <- file.remove(
    obsolete_file
  )
  
  if (!removed) {
    warning(
      "Could not remove obsolete file: ",
      obsolete_file
    )
  }
}

# =============================================================================
# 15. CONSOLE REPORT
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "SCRIPT 99: PARAMETER REGISTRY CONSOLIDATION COMPLETE\n"
)

cat(
  "============================================================\n\n"
)

cat(
  "Registry writer architecture:\n"
)

cat(
  " - Script 99 is the sole writer of docs/parameter_registry.csv\n\n"
)

cat(
  "Removed obsolete registry rows:\n"
)

if (nrow(removed_rows) > 0) {
  print(
    removed_rows[
      ,
      c(
        "dynare_name",
        "research_value"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
} else {
  cat(
    " - none were present\n"
  )
}

cat(
  "\nUpdated / consolidated research parameters:\n\n"
)

print(
  audit_rows,
  row.names = FALSE,
  digits = 12
)

cat(
  "\nKey corrected values:\n"
)

cat(
  sprintf(
    " Pi_ss  = %.12f\n",
    final_value("Pi_ss")
  )
)

cat(
  sprintf(
    " invPi  = %.12f\n",
    final_value("invPi")
  )
)

cat(
  sprintf(
    " rhotau = %.15f\n",
    final_value("rhotau")
  )
)

cat(
  sprintf(
    " rhoG   = %.15f\n",
    final_value("rhoG")
  )
)

cat(
  sprintf(
    " rhoIG  = %.15f\n",
    final_value("rhoIG")
  )
)

cat(
  sprintf(
    " g_y    = %.15f\n",
    final_value("g_y")
  )
)

cat(
  sprintf(
    " ig_y   = %.15f\n",
    final_value("ig_y")
  )
)

cat(
  sprintf(
    " cH_y   = %.12f\n",
    final_value("cH_y")
  )
)

cat(
  sprintf(
    " rhoF   = %.12f\n",
    final_value("rhoF")
  )
)

cat(
  sprintf(
    " alpha  = %.12f\n",
    final_value("alpha")
  )
)

cat(
  sprintf(
    " mu     = %.12f\n",
    final_value("mu")
  )
)

cat(
  sprintf(
    " lambda_s = %.12f\n",
    final_value("lambda_s")
  )
)

cat(
  sprintf(
    " dbar   = %.12f\n",
    final_value("dbar")
  )
)

cat(
  sprintf(
    " phiD   = %.12f\n",
    final_value("phiD")
  )
)

cat(
  "\nSafety checks passed:\n"
)

cat(
  " - baseline_value unchanged for every retained registry row\n"
)

cat(
  " - rho_tau / rho_g / rho_ig removed\n"
)

cat(
  " - rhotau / rhoG / rhoIG retained as canonical names\n"
)

cat(
  " - phiDD removed\n"
)

cat(
  " - corrected 1968-1972 fiscal-share baseline enforced\n"
)

cat(
  " - cH_y identity verified\n"
)

cat(
  " - lambda_s identity verified\n"
)

cat(
  "\nFiles written:\n"
)

cat(
  " - docs/parameter_registry.csv\n"
)

cat(
  " - output/estimates/parameter_registry_update_audit.csv\n"
)

cat(
  "\n============================================================\n"
)

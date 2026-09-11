# =============================================================================
# 02_update_parameter_registry.R
#
# Purpose:
#   Update docs/parameter_registry.csv with research estimates produced by
#   R/01_world_interest_rate.R.
#
# Parameters updated:
#   rhoiw     = rho_i*     foreign interest-rate persistence
#   rstar_ss  = r*_ss     steady-state quarterly foreign interest rate
#
# Input:
#   output/estimates/world_interest_rate_parameters.csv
#
# Registry:
#   docs/parameter_registry.csv
#
# IMPORTANT:
#   This script updates RESEARCH values only.
#   It does NOT overwrite baseline_value.
#
# This script intentionally uses BASE R ONLY.
# =============================================================================

rm(list = ls())


# ---- 0. File paths -----------------------------------------------------------

registry_file <- file.path(
  "docs",
  "parameter_registry.csv"
)

parameter_file <- file.path(
  "output",
  "estimates",
  "world_interest_rate_parameters.csv"
)


# ---- 1. Verify project root and required files -------------------------------

if (!file.exists(registry_file)) {
  stop(
    "Cannot find docs/parameter_registry.csv. ",
    "Open the project from the repository root."
  )
}

if (!file.exists(parameter_file)) {
  stop(
    "Cannot find output/estimates/world_interest_rate_parameters.csv. ",
    "Run R/01_world_interest_rate.R first."
  )
}


# ---- 2. Read registry and new parameter estimates ----------------------------

registry <- read.csv(
  registry_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

estimates <- read.csv(
  parameter_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ---- 3. Validate expected structure -----------------------------------------

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
    "Parameter registry is missing required columns: ",
    paste(missing_registry_columns, collapse = ", ")
  )
}


required_estimate_columns <- c(
  "dynare_name",
  "estimate",
  "standard_error",
  "shock_sd",
  "sample",
  "method",
  "raw_series",
  "status",
  "notes"
)

missing_estimate_columns <- setdiff(
  required_estimate_columns,
  names(estimates)
)

if (length(missing_estimate_columns) > 0) {
  stop(
    "world_interest_rate_parameters.csv is missing required columns: ",
    paste(missing_estimate_columns, collapse = ", ")
  )
}


# ---- 4. Verify the two expected parameter rows -------------------------------

target_parameters <- c(
  "rhoiw",
  "rstar_ss"
)

for (parameter_name in target_parameters) {
  
  n_estimate_rows <- sum(
    estimates$dynare_name == parameter_name
  )
  
  if (n_estimate_rows != 1) {
    stop(
      "Expected exactly one ",
      parameter_name,
      " row in world_interest_rate_parameters.csv, but found ",
      n_estimate_rows,
      "."
    )
  }
  
  n_registry_rows <- sum(
    registry$dynare_name == parameter_name
  )
  
  if (n_registry_rows != 1) {
    stop(
      "Expected exactly one ",
      parameter_name,
      " row in docs/parameter_registry.csv, but found ",
      n_registry_rows,
      "."
    )
  }
}


# ---- 5. Preserve baseline values before update -------------------------------

baseline_before <- registry$baseline_value
names(baseline_before) <- registry$dynare_name


# ---- 6. Update rhoiw ---------------------------------------------------------

rhoiw_estimate <- estimates[
  estimates$dynare_name == "rhoiw",
]

rhoiw_registry_row <- which(
  registry$dynare_name == "rhoiw"
)

registry$classification[
  rhoiw_registry_row
] <- "Estimated"

registry$research_value[
  rhoiw_registry_row
] <- rhoiw_estimate$estimate

registry$status[
  rhoiw_registry_row
] <- rhoiw_estimate$status

registry$method[
  rhoiw_registry_row
] <- rhoiw_estimate$method

registry$sample[
  rhoiw_registry_row
] <- rhoiw_estimate$sample

registry$raw_or_primary_data[
  rhoiw_registry_row
] <- "data/raw/TB3MS.csv"

registry$source_or_literature[
  rhoiw_registry_row
] <- "FRED series TB3MS"

registry$standard_error[
  rhoiw_registry_row
] <- rhoiw_estimate$standard_error

registry$shock_sd[
  rhoiw_registry_row
] <- rhoiw_estimate$shock_sd

registry$notes[
  rhoiw_registry_row
] <- paste(
  rhoiw_estimate$notes,
  paste0(
    "Robustness estimates: ",
    "output/estimates/world_interest_rate_ar1.csv."
  )
)


# ---- 7. Update rstar_ss ------------------------------------------------------

rstar_estimate <- estimates[
  estimates$dynare_name == "rstar_ss",
]

rstar_registry_row <- which(
  registry$dynare_name == "rstar_ss"
)

registry$classification[
  rstar_registry_row
] <- "Fixed / derived"

registry$research_value[
  rstar_registry_row
] <- rstar_estimate$estimate

registry$status[
  rstar_registry_row
] <- rstar_estimate$status

registry$method[
  rstar_registry_row
] <- rstar_estimate$method

registry$sample[
  rstar_registry_row
] <- rstar_estimate$sample

registry$raw_or_primary_data[
  rstar_registry_row
] <- "data/raw/TB3MS.csv"

registry$source_or_literature[
  rstar_registry_row
] <- "FRED series TB3MS"

registry$standard_error[
  rstar_registry_row
] <- NA_real_

registry$shock_sd[
  rstar_registry_row
] <- NA_real_

registry$notes[
  rstar_registry_row
] <- paste(
  rstar_estimate$notes,
  paste0(
    "Derived from the same recommended pre-shock AR(1) used to estimate rhoiw. ",
    "Not independently estimated."
  )
)


# ---- 8. Safety check: baseline values must remain unchanged ------------------

baseline_after <- registry$baseline_value
names(baseline_after) <- registry$dynare_name

if (!isTRUE(
  all.equal(
    baseline_before,
    baseline_after,
    check.attributes = FALSE
  )
)) {
  stop(
    "SAFETY FAILURE: baseline_value was modified. ",
    "Registry was NOT saved."
  )
}


# ---- 9. Save updated parameter registry -------------------------------------

write.csv(
  registry,
  registry_file,
  row.names = FALSE,
  na = ""
)


# ---- 10. Remove obsolete single-parameter output -----------------------------

obsolete_file <- file.path(
  "output",
  "estimates",
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


# ---- 11. Console report ------------------------------------------------------

cat("\n============================================================\n")
cat("PARAMETER REGISTRY UPDATE\n")
cat("============================================================\n\n")

cat("Updated research parameters:\n\n")

print(
  registry[
    registry$dynare_name %in% c(
      "rhoiw",
      "rstar_ss"
    ),
    c(
      "dynare_name",
      "classification",
      "baseline_value",
      "research_value",
      "status",
      "sample",
      "standard_error",
      "shock_sd"
    )
  ],
  row.names = FALSE,
  digits = 10
)

cat("\nRegistry saved to:\n")
cat("  docs/parameter_registry.csv\n")

cat("\nIMPORTANT:\n")
cat("  baseline_value was NOT changed.\n")
cat("  rhoiw is directly estimated.\n")
cat("  rstar_ss is derived from the estimated AR(1) steady state.\n")

if (!file.exists(obsolete_file)) {
  cat("  obsolete rhoiw_for_dynare.csv is absent.\n")
}

cat("\nRegistry update completed successfully.\n")
cat("============================================================\n")
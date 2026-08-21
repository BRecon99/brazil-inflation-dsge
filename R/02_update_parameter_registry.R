# =============================================================================
# 02_update_parameter_registry.R
#
# Updates docs/parameter_registry.csv with the first-stage rhoiw estimate.
# The CSV is the GitHub-friendly source of truth for parameter decisions.
# =============================================================================

registry_file <- file.path("docs", "parameter_registry.csv")
estimate_file <- file.path("output", "estimates", "rhoiw_for_dynare.csv")

if (!file.exists(registry_file)) stop("Missing docs/parameter_registry.csv")
if (!file.exists(estimate_file)) {
  stop("Missing rhoiw estimate. Run R/01_world_interest_rate.R first.")
}

registry <- read.csv(registry_file, stringsAsFactors = FALSE, check.names = FALSE)
est <- read.csv(estimate_file, stringsAsFactors = FALSE)

idx <- which(registry$dynare_name == "rhoiw")
if (length(idx) != 1) stop("Expected exactly one rhoiw row in parameter_registry.csv")

registry$research_value[idx] <- format(est$estimate[1], digits = 12)
registry$status[idx] <- "FIRST-STAGE ESTIMATE COMPLETE"
registry$method[idx] <- paste(
  "OLS AR(1): r_t = a + rho*r_(t-1) + u_t;",
  "monthly TB3MS averaged to quarter;",
  "annual % converted to effective quarterly decimal"
)
registry$sample[idx] <- est$sample[1]
registry$raw_or_primary_data[idx] <- "data/raw/TB3MS.csv"
registry$source_or_literature[idx] <- "FRED series TB3MS"
registry$standard_error[idx] <- format(est$standard_error[1], digits = 12)
registry$shock_sd[idx] <- format(est$shock_sd_eps_iw[1], digits = 12)
registry$notes[idx] <- paste(
  "Recommended pre-shock baseline.",
  "Robustness estimates: output/estimates/world_interest_rate_ar1.csv."
)

write.csv(registry, registry_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("Updated:", registry_file, "\n")
cat("rhoiw =", registry$research_value[idx], "\n")

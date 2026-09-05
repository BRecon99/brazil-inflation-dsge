# =============================================================================
# 01_world_interest_rate.R
# 
# parameter estimate: rho_i*
#
# Model equation:
#   i_t^* = rho_i* i_{t-1}^* + epsilon_{i*,t}
#
# Raw input:
#   data/raw/TB3MS.csv
#
# Outputs:
#   data/processed/world_interest_rate_quarterly.csv
#   output/estimates/world_interest_rate_ar1.csv
#   output/estimates/rhoiw_for_dynare.csv
#   output/figures/world_interest_rate_quarterly.png
#
# This script intentionally uses BASE R ONLY.
# =============================================================================

rm(list = ls())

# ---- 0. Verify project root --------------------------------------------------
required_file <- file.path("data", "raw", "TB3MS.csv")
if (!file.exists(required_file)) {
  stop(
    "Cannot find data/raw/TB3MS.csv. ",
    "Open the project by double-clicking brazil-inflation-dsge.Rproj ",
    "or set the working directory to the repository root."
  )
}

# ---- 1. Read raw monthly data ------------------------------------------------
raw <- read.csv(required_file, stringsAsFactors = FALSE)

if (!all(c("observation_date", "TB3MS") %in% names(raw))) {
  stop("TB3MS.csv must contain columns observation_date and TB3MS.")
}

raw$date <- as.Date(raw$observation_date, format = "%m/%d/%Y")
if (any(is.na(raw$date))) stop("Some observation_date values could not be parsed.")

raw$year  <- as.integer(format(raw$date, "%Y"))
raw$month <- as.integer(format(raw$date, "%m"))
raw$qtr   <- ((raw$month - 1) %/% 3) + 1

# Keep the paper's historical window.
raw <- raw[raw$year >= 1964 & raw$year <= 1980, ]

# ---- 2. Monthly -> quarterly average ----------------------------------------
quarter_key <- paste0(raw$year, "Q", raw$qtr)
annual_pct <- aggregate(raw$TB3MS, by = list(quarter = quarter_key), FUN = mean)
names(annual_pct)[2] <- "annual_percent"

# Recover year and quarter as numeric fields.
annual_pct$year <- as.integer(substr(annual_pct$quarter, 1, 4))
annual_pct$qtr  <- as.integer(substr(annual_pct$quarter, 6, 6))

# Sort chronologically.
annual_pct <- annual_pct[order(annual_pct$year, annual_pct$qtr), ]

# ---- 3. Convert annual percent -> effective quarterly decimal ----------------
#
# Example:
#   5 percent annual -> (1.05)^(1/4)-1 quarterly
#
annual_pct$quarterly_rate <-
  (1 + annual_pct$annual_percent / 100)^(1/4) - 1

annual_pct$quarter_index <- annual_pct$year * 4 + annual_pct$qtr

quarterly <- annual_pct[
  , c("quarter", "year", "qtr", "annual_percent",
      "quarterly_rate", "quarter_index")
]

dir.create(file.path("data", "processed"), recursive = TRUE, showWarnings = FALSE)
write.csv(
  quarterly,
  file.path("data", "processed", "world_interest_rate_quarterly.csv"),
  row.names = FALSE
)

# ---- 4. AR(1) estimation function -------------------------------------------
estimate_ar1 <- function(data, end_year) {

  sample_data <- data[
    data$year >= 1964 & data$year <= end_year,
  ]

  current <- sample_data$quarterly_rate[2:nrow(sample_data)]
  lagged  <- sample_data$quarterly_rate[1:(nrow(sample_data)-1)]

  fit <- lm(current ~ lagged)

  a_hat    <- unname(coef(fit)[1])
  rho_hat  <- unname(coef(fit)[2])
  se_rho   <- unname(summary(fit)$coefficients["lagged", "Std. Error"])
  shock_sd <- summary(fit)$sigma

  long_run_rate <- a_hat / (1 - rho_hat)

  data.frame(
    sample = paste0("1964Q1-", end_year, "Q4"),
    n_regression = length(current),
    constant = a_hat,
    rho = rho_hat,
    se_rho = se_rho,
    shock_sd = shock_sd,
    long_run_quarterly_rate = long_run_rate,
    recommended_baseline = ifelse(end_year == 1972, "YES", "NO")
  )
}

# ---- 5. Baseline + robustness samples ---------------------------------------
results <- rbind(
  estimate_ar1(quarterly, 1972),
  estimate_ar1(quarterly, 1978),
  estimate_ar1(quarterly, 1980)
)

dir.create(file.path("output", "estimates"), recursive = TRUE, showWarnings = FALSE)
write.csv(
  results,
  file.path("output", "estimates", "world_interest_rate_ar1.csv"),
  row.names = FALSE
)

recommended <- results[results$recommended_baseline == "YES", ]

rhoiw_row <- data.frame(
  dynare_parameter = "rhoiw",
  model_symbol = "rho_i_star",
  estimate = recommended$rho,
  standard_error = recommended$se_rho,
  shock_sd_eps_iw = recommended$shock_sd,
  long_run_quarterly_rate = recommended$long_run_quarterly_rate,
  sample = recommended$sample,
  method = "OLS AR(1) on quarterly effective rate",
  raw_series = "FRED TB3MS",
  status = "FIRST-STAGE ESTIMATE COMPLETE"
)

write.csv(
  rhoiw_row,
  file.path("output", "estimates", "rhoiw_for_dynare.csv"),
  row.names = FALSE
)

# ---- 6. Plot -----------------------------------------------------------------
dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)

png(
  file.path("output", "figures", "world_interest_rate_quarterly.png"),
  width = 1400, height = 700, res = 150
)

plot(
  seq_len(nrow(quarterly)),
  quarterly$annual_percent,
  type = "l",
  xaxt = "n",
  xlab = "Quarter",
  ylab = "Annualized percent",
  main = "3-Month Treasury Bill Rate — Quarterly Average"
)

tick_pos <- seq(1, nrow(quarterly), by = 8)
axis(
  1,
  at = tick_pos,
  labels = quarterly$quarter[tick_pos],
  las = 2,
  cex.axis = 0.8
)

abline(v = which(quarterly$quarter == "1973Q1"), lty = 2)
abline(v = which(quarterly$quarter == "1979Q1"), lty = 2)

dev.off()

# ---- 7. Console report -------------------------------------------------------
cat("\n============================================================\n")
cat("FIRST PARAMETER ESTIMATION: rho_i*\n")
cat("============================================================\n\n")
print(results, digits = 6)

cat("\nRecommended baseline for the historical model:\n")
cat("  rhoiw =", format(recommended$rho, digits = 8), "\n")
cat("  SE     =", format(recommended$se_rho, digits = 8), "\n")
cat("  shock SD eps_iw =", format(recommended$shock_sd, digits = 8), "\n")
cat("  long-run quarterly rate =", 
    format(recommended$long_run_quarterly_rate, digits = 8), "\n")
cat("\nIMPORTANT: do not overwrite the Dynare baseline automatically.\n")
cat("The parameter registry is updated by R/02_update_parameter_registry.R.\n")

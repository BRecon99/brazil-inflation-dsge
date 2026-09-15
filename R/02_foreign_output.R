# =============================================================================
# 02_foreign_output.R
#
# Parameter estimate:
#   rhoystar = rho_y* = persistence of foreign output
#
# Model equation:
#
#   ystar_t = rhoystar * ystar_(t-1) + eps_ystar_t
#
# Observable:
#   U.S. Real Gross Domestic Product
#   FRED series GDPC1
#
# Transformation:
#   1. Use quarterly real GDP level.
#   2. Take natural log.
#   3. HP-filter log GDP over 1947Q1-1980Q4 using lambda = 1600.
#   4. Define ystar as the cyclical log deviation from trend.
#   5. Estimate AR(1) WITHOUT intercept, consistent with the DSGE equation.
#
# Recommended estimation sample:
#   1964Q1-1972Q4
#
# Robustness samples:
#   1964Q1-1978Q4
#   1964Q1-1980Q4
#
# Raw input:
#   data/raw/GDPC1.csv
#
# Outputs:
#   data/processed/foreign_output_gap_quarterly.csv
#   output/estimates/foreign_output_ar1.csv
#   output/estimates/foreign_output_parameters.csv
#   output/figures/foreign_output_gap.png
#
# IMPORTANT:
#   This script estimates rhoystar.
#   It does NOT estimate rhoF.
#
# This script intentionally uses BASE R ONLY.
# =============================================================================


rm(list = ls())


# ---- 0. Verify project root --------------------------------------------------

required_file <- file.path(
  "data",
  "raw",
  "GDPC1.csv"
)

if (!file.exists(required_file)) {
  
  stop(
    "Cannot find data/raw/GDPC1.csv. ",
    "Place the FRED GDPC1 file in data/raw/ ",
    "and run the script from the repository root."
  )
}


# ---- 1. Read raw quarterly GDP data -----------------------------------------

raw <- read.csv(
  required_file,
  stringsAsFactors = FALSE
)


required_columns <- c(
  "observation_date",
  "GDPC1"
)

if (!all(
  required_columns %in% names(raw)
)) {
  
  stop(
    "GDPC1.csv must contain columns ",
    "observation_date and GDPC1."
  )
}


# ---- 2. Validate GDP values --------------------------------------------------

raw$GDPC1 <- as.numeric(
  raw$GDPC1
)

if (anyNA(raw$GDPC1)) {
  
  stop(
    "Some GDPC1 observations are missing or non-numeric."
  )
}

if (any(raw$GDPC1 <= 0)) {
  
  stop(
    "GDPC1 contains zero or negative observations. ",
    "Natural logs cannot be calculated."
  )
}


# ---- 3. Parse dates ----------------------------------------------------------

raw$date <- as.Date(
  raw$observation_date,
  format = "%m/%d/%Y"
)

if (anyNA(raw$date)) {
  
  stop(
    "Some observation_date values could not be parsed."
  )
}


raw$year <- as.integer(
  format(
    raw$date,
    "%Y"
  )
)

raw$month <- as.integer(
  format(
    raw$date,
    "%m"
  )
)

raw$qtr <-
  ((raw$month - 1) %/% 3) + 1

raw$quarter <- paste0(
  raw$year,
  "Q",
  raw$qtr
)


# ---- 4. Sort chronologically -------------------------------------------------

raw <- raw[
  order(raw$date),
]


# ---- 5. Validate quarterly frequency ----------------------------------------

if (anyDuplicated(raw$quarter)) {
  
  stop(
    "Duplicate quarters were found in GDPC1.csv."
  )
}


quarter_index <-
  raw$year * 4 +
  raw$qtr

quarter_gap <- diff(
  quarter_index
)

if (any(quarter_gap != 1)) {
  
  stop(
    "GDPC1.csv contains missing or irregular quarters."
  )
}


# ---- 6. Create historical detrending window ---------------------------------
#
# We estimate the cyclical component using observations from 1947Q1 through
# 1980Q4.
#
# Using observations before 1964 gives the filter information about the
# pre-estimation trend.
#
# We deliberately do NOT use observations after 1980 to construct the trend
# for the 1964-1980 historical model.
#

historical <- raw[
  raw$year >= 1947 &
    raw$year <= 1980,
]


if (nrow(historical) != 136) {
  
  warning(
    "Expected 136 quarterly observations from 1947Q1 through 1980Q4, ",
    "but found ",
    nrow(historical),
    "."
  )
}


# ---- 7. Take natural log of real GDP -----------------------------------------

historical$log_gdp <- log(
  historical$GDPC1
)


# ---- 8. Base-R HP filter -----------------------------------------------------
#
# HP decomposition:
#
#   log(Y_t*) = trend_t + ystar_t
#
# Quarterly smoothing parameter:
#
#   lambda = 1600
#

hp_filter <- function(
    x,
    lambda = 1600
) {
  
  x <- as.numeric(x)
  
  n <- length(x)
  
  if (n < 4) {
    
    stop(
      "HP filter requires at least four observations."
    )
  }
  
  
  D <- matrix(
    0,
    nrow = n - 2,
    ncol = n
  )
  
  
  for (i in seq_len(n - 2)) {
    
    D[i, i]     <- 1
    D[i, i + 1] <- -2
    D[i, i + 2] <- 1
  }
  
  
  trend <- solve(
    diag(n) +
      lambda * crossprod(D),
    x
  )
  
  
  cycle <- x - trend
  
  
  list(
    trend = as.numeric(trend),
    cycle = as.numeric(cycle)
  )
}


hp_result <- hp_filter(
  historical$log_gdp,
  lambda = 1600
)


historical$log_gdp_trend <-
  hp_result$trend

historical$ystar <-
  hp_result$cycle


# ---- 9. Diagnostic GDP growth series ----------------------------------------
#
# This is saved for comparison only.
#
# GDP growth is NOT the ystar observable used in the AR(1).
#

historical$quarterly_log_growth <- c(
  NA_real_,
  diff(
    historical$log_gdp
  )
)


# ---- 10. Quarter index -------------------------------------------------------

historical$quarter_index <-
  historical$year * 4 +
  historical$qtr


# ---- 11. Save processed foreign-output dataset -------------------------------

processed <- historical[
  ,
  c(
    "quarter",
    "year",
    "qtr",
    "GDPC1",
    "log_gdp",
    "log_gdp_trend",
    "ystar",
    "quarterly_log_growth",
    "quarter_index"
  )
]


dir.create(
  file.path(
    "data",
    "processed"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


write.csv(
  processed,
  file.path(
    "data",
    "processed",
    "foreign_output_gap_quarterly.csv"
  ),
  row.names = FALSE
)


# ---- 12. AR(1) estimation function ------------------------------------------
#
# DSGE equation:
#
#   ystar_t =
#       rhoystar * ystar_(t-1)
#       + eps_ystar_t
#
# Because ystar is a deviation from trend with steady state zero,
# the regression is estimated WITHOUT an intercept.
#

estimate_ar1 <- function(
    data,
    end_year
) {
  
  sample_data <- data[
    data$year >= 1964 &
      data$year <= end_year,
  ]
  
  
  if (nrow(sample_data) < 3) {
    
    stop(
      "Not enough observations for AR(1) estimation."
    )
  }
  
  
  current <- sample_data$ystar[
    2:nrow(sample_data)
  ]
  
  lagged <- sample_data$ystar[
    1:(nrow(sample_data) - 1)
  ]
  
  
  fit <- lm(
    current ~ 0 + lagged
  )
  
  
  rho_hat <- unname(
    coef(fit)["lagged"]
  )
  
  
  se_rho <- unname(
    summary(fit)$coefficients[
      "lagged",
      "Std. Error"
    ]
  )
  
  
  shock_sd <-
    summary(fit)$sigma
  
  
  data.frame(
    sample = paste0(
      "1964Q1-",
      end_year,
      "Q4"
    ),
    
    n_regression =
      length(current),
    
    rho =
      rho_hat,
    
    se_rho =
      se_rho,
    
    shock_sd =
      shock_sd,
    
    recommended_baseline =
      ifelse(
        end_year == 1972,
        "YES",
        "NO"
      ),
    
    stringsAsFactors = FALSE
  )
}


# ---- 13. Baseline + robustness estimates ------------------------------------

results <- rbind(
  estimate_ar1(
    processed,
    1972
  ),
  
  estimate_ar1(
    processed,
    1978
  ),
  
  estimate_ar1(
    processed,
    1980
  )
)


dir.create(
  file.path(
    "output",
    "estimates"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


write.csv(
  results,
  file.path(
    "output",
    "estimates",
    "foreign_output_ar1.csv"
  ),
  row.names = FALSE
)


# ---- 14. Select recommended baseline -----------------------------------------

recommended <- results[
  results$recommended_baseline == "YES",
]


if (nrow(recommended) != 1) {
  
  stop(
    "Exactly one recommended baseline estimate must exist."
  )
}


# ---- 15. Dynare-facing parameter output -------------------------------------

parameter_results <- data.frame(
  
  dynare_name =
    "rhoystar",
  
  symbol =
    "rho_y_star",
  
  estimate =
    recommended$rho,
  
  standard_error =
    recommended$se_rho,
  
  shock_sd =
    recommended$shock_sd,
  
  sample =
    recommended$sample,
  
  detrending_sample =
    "1947Q1-1980Q4",
  
  transformation =
    paste(
      "Natural log of FRED GDPC1;",
      "HP-filtered cyclical component;",
      "lambda = 1600"
    ),
  
  method =
    paste(
      "OLS AR(1) without intercept on",
      "HP-filtered log U.S. real GDP cycle"
    ),
  
  raw_series =
    "FRED GDPC1",
  
  status =
    "FIRST-STAGE ESTIMATE COMPLETE",
  
  notes =
    paste(
      "Recommended pre-oil-shock estimate.",
      "ystar is the cyclical log deviation of U.S. real GDP from HP trend.",
      "rhoF is not estimated by this script."
    ),
  
  stringsAsFactors = FALSE
)


write.csv(
  parameter_results,
  file.path(
    "output",
    "estimates",
    "foreign_output_parameters.csv"
  ),
  row.names = FALSE
)


# ---- 16. Plot foreign output gap ---------------------------------------------

dir.create(
  file.path(
    "output",
    "figures"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


png(
  file.path(
    "output",
    "figures",
    "foreign_output_gap.png"
  ),
  width = 1400,
  height = 700,
  res = 150
)


plot(
  seq_len(
    nrow(processed)
  ),
  processed$ystar,
  type = "l",
  xaxt = "n",
  xlab = "Quarter",
  ylab = "Log deviation from HP trend",
  main = "U.S. Real GDP — Foreign Output Gap"
)


abline(
  h = 0,
  lty = 2
)


tick_pos <- seq(
  1,
  nrow(processed),
  by = 8
)


axis(
  1,
  at = tick_pos,
  labels =
    processed$quarter[tick_pos],
  las = 2,
  cex.axis = 0.8
)


abline(
  v = which(
    processed$quarter == "1973Q1"
  ),
  lty = 2
)


abline(
  v = which(
    processed$quarter == "1979Q1"
  ),
  lty = 2
)


dev.off()


# ---- 17. Console diagnostics -------------------------------------------------

cat("\n============================================================\n")
cat("FOREIGN OUTPUT PARAMETER ESTIMATION\n")
cat("============================================================\n\n")


cat("Raw series:\n")
cat("  FRED GDPC1 — U.S. Real Gross Domestic Product\n\n")


cat("Transformation:\n")
cat("  log(GDPC1)\n")
cat("  minus HP trend\n")
cat("  lambda = 1600\n")
cat("  detrending window = 1947Q1-1980Q4\n\n")


cat("Model equation:\n")
cat(
  "  ystar_t = rhoystar * ystar_(t-1) + eps_ystar_t\n\n"
)


cat("AR(1) results:\n\n")

print(
  results,
  digits = 8,
  row.names = FALSE
)


cat("\nRecommended baseline:\n")

cat(
  "  sample       =",
  recommended$sample,
  "\n"
)

cat(
  "  rhoystar     =",
  format(
    recommended$rho,
    digits = 10
  ),
  "\n"
)

cat(
  "  SE rhoystar  =",
  format(
    recommended$se_rho,
    digits = 10
  ),
  "\n"
)

cat(
  "  shock SD     =",
  format(
    recommended$shock_sd,
    digits = 10
  ),
  "\n"
)


cat("\nInterpretation:\n")

cat(
  "  rhoystar measures persistence of the cyclical foreign-output deviation.\n"
)

cat(
  "  It is NOT persistence of raw GDP levels and NOT persistence of GDP growth.\n"
)

cat(
  "  This script does NOT estimate rhoF.\n"
)


cat("\nOutputs saved:\n")

cat(
  "  data/processed/foreign_output_gap_quarterly.csv\n"
)

cat(
  "  output/estimates/foreign_output_ar1.csv\n"
)

cat(
  "  output/estimates/foreign_output_parameters.csv\n"
)

cat(
  "  output/figures/foreign_output_gap.png\n"
)


cat("\n============================================================\n")
cat("FOREIGN OUTPUT ESTIMATION COMPLETE\n")
cat("============================================================\n")
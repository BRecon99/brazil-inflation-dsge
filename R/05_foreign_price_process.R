# =============================================================================
# 05_foreign_price_process.R
#
# Purpose:
#   Estimate persistence of the foreign-price process:
#
#       pif_t = rhoF * pif_(t-1) + eps_pf,t
#
#   and estimate the standard deviation of eps_pf.
#
# Economic interpretation:
#
#   In the DSGE, imported goods are intermediate production inputs:
#
#       y_t = a_t + (1-mu)n_t + mu*z_t
#
#   Therefore the empirical counterpart of the foreign price P_F should
#   represent prices of foreign industrial inputs/materials rather than
#   consumer goods or capital goods.
#
# Baseline proxy:
#
#   B639RG3Q086SBEA
#   U.S. exports of goods: Industrial supplies and materials
#   Chain-type price index
#   Quarterly
#
# Robustness proxies:
#
#   B686RG3Q086SBEA
#   Industrial supplies/materials - durable goods
#
#   B687RG3Q086SBEA
#   Industrial supplies/materials - nondurable goods
#
# Diagnostic / negative-control proxy:
#
#   B640RG3Q086SBEA
#   Capital goods except automotive
#
# Historical validation:
#
#   Brazil annual import implicit price index
#   from 07_setor_externo_1901_00.xls
#
# Baseline AR(1) regression sample:
#
#   1967Q3-1980Q4
#
# Main model parameters affected:
#
#   rhoF
#   stderr eps_pf
#
# =============================================================================


# =============================================================================
# 0. REQUIRED PACKAGES
# =============================================================================

required_packages <- c(
  "readxl",
  "sandwich",
  "tseries"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install them with:\n\n",
      "install.packages(c(",
      paste(
        paste0('"', missing_packages, '"'),
        collapse = ", "
      ),
      "))"
    )
  )
}


# =============================================================================
# 1. FILE PATHS
# =============================================================================

file_B639 <- file.path(
  "data",
  "raw",
  "B639RG3Q086SBEA.csv"
)

file_B686 <- file.path(
  "data",
  "raw",
  "B686RG3Q086SBEA.csv"
)

file_B687 <- file.path(
  "data",
  "raw",
  "B687RG3Q086SBEA.csv"
)

file_B640 <- file.path(
  "data",
  "raw",
  "B640RG3Q086SBEA.csv"
)

file_brazil <- file.path(
  "data",
  "raw",
  "07_setor_externo_1901_00.xls"
)

# Optional broad U.S. export-price benchmark
file_broad <- file.path(
  "data",
  "raw",
  "USA_Exports.csv"
)


required_files <- c(
  file_B639,
  file_B686,
  file_B687,
  file_B640,
  file_brazil
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste(
      "The following required raw files are missing:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    )
  )
}


# =============================================================================
# 2. CREATE OUTPUT DIRECTORIES
# =============================================================================

dir.create(
  file.path("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path("output", "estimates"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path("output", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "docs",
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 3. HELPER: READ FRED PRICE-INDEX FILE
# =============================================================================
#
# Expected structure:
#
# observation_date     SERIES_CODE
# 1967-01-01           ...
# 1967-04-01           ...
#
# The function:
#
#   - reads the raw file
#   - detects the value column
#   - parses the date
#   - checks observations
#   - creates quarterly log inflation:
#
#       pi_t = log(P_t) - log(P_(t-1))
#
# =============================================================================

read_fred_price <- function(
    path,
    short_name
) {
  
  x <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  if (!"observation_date" %in% names(x)) {
    stop(
      paste(
        "observation_date not found in",
        path
      )
    )
  }
  
  value_columns <- setdiff(
    names(x),
    "observation_date"
  )
  
  if (length(value_columns) != 1) {
    stop(
      paste(
        "Expected exactly one value column in",
        path,
        "but found",
        length(value_columns)
      )
    )
  }
  
  value_column <- value_columns[1]
  
  out <- data.frame(
    date = as.Date(x$observation_date),
    price = suppressWarnings(
      as.numeric(x[[value_column]])
    ),
    stringsAsFactors = FALSE
  )
  
  names(out)[2] <- short_name
  
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  if (any(is.na(out$date))) {
    stop(
      paste(
        "Missing or invalid dates found in",
        path
      )
    )
  }
  
  if (anyDuplicated(out$date) > 0) {
    stop(
      paste(
        "Duplicate dates found in",
        path
      )
    )
  }
  
  out <- out[
    order(out$date),
  ]
  
  if (
    any(
      out[[short_name]] <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      paste(
        "Non-positive price-index observation found in",
        path
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Time variables
  # ---------------------------------------------------------------------------
  
  out$year <- as.integer(
    format(out$date, "%Y")
  )
  
  out$month <- as.integer(
    format(out$date, "%m")
  )
  
  out$qtr <-
    ((out$month - 1) %/% 3) + 1
  
  out$quarter <- paste0(
    out$year,
    "Q",
    out$qtr
  )
  
  
  # ---------------------------------------------------------------------------
  # Log inflation
  # ---------------------------------------------------------------------------
  
  inflation_name <- paste0(
    short_name,
    "_log_inflation"
  )
  
  out[[inflation_name]] <-
    c(
      NA,
      diff(
        log(
          out[[short_name]]
        )
      )
    )
  
  
  return(out)
}


# =============================================================================
# 4. READ QUARTERLY U.S. PRICE SERIES
# =============================================================================

B639 <- read_fred_price(
  file_B639,
  "B639_index"
)

B686 <- read_fred_price(
  file_B686,
  "B686_index"
)

B687 <- read_fred_price(
  file_B687,
  "B687_index"
)

B640 <- read_fred_price(
  file_B640,
  "B640_index"
)


# =============================================================================
# 5. OPTIONAL BROAD U.S. EXPORT PRICE SERIES
# =============================================================================

use_broad <- file.exists(file_broad)

if (use_broad) {
  
  broad <- read_fred_price(
    file_broad,
    "broad_export_index"
  )
  
  cat(
    "\nBroad export benchmark found and included.\n"
  )
  
} else {
  
  cat(
    "\nUSA_Exports.csv not found.",
    "Broad export benchmark will be skipped.\n"
  )
}


# =============================================================================
# 6. CHECK QUARTERLY COVERAGE
# =============================================================================

cat("\nQuarterly price-series coverage:\n")

cat(
  "B639: ",
  min(B639$quarter),
  " to ",
  max(B639$quarter),
  "\n",
  sep = ""
)

cat(
  "B686: ",
  min(B686$quarter),
  " to ",
  max(B686$quarter),
  "\n",
  sep = ""
)

cat(
  "B687: ",
  min(B687$quarter),
  " to ",
  max(B687$quarter),
  "\n",
  sep = ""
)

cat(
  "B640: ",
  min(B640$quarter),
  " to ",
  max(B640$quarter),
  "\n",
  sep = ""
)


# =============================================================================
# 7. MERGE QUARTERLY PRICE SERIES
# =============================================================================

keep_B639 <- B639[
  ,
  c(
    "date",
    "year",
    "qtr",
    "quarter",
    "B639_index",
    "B639_index_log_inflation"
  )
]

keep_B686 <- B686[
  ,
  c(
    "date",
    "B686_index",
    "B686_index_log_inflation"
  )
]

keep_B687 <- B687[
  ,
  c(
    "date",
    "B687_index",
    "B687_index_log_inflation"
  )
]

keep_B640 <- B640[
  ,
  c(
    "date",
    "B640_index",
    "B640_index_log_inflation"
  )
]


quarterly <- merge(
  keep_B639,
  keep_B686,
  by = "date",
  all = TRUE
)

quarterly <- merge(
  quarterly,
  keep_B687,
  by = "date",
  all = TRUE
)

quarterly <- merge(
  quarterly,
  keep_B640,
  by = "date",
  all = TRUE
)


if (use_broad) {
  
  keep_broad <- broad[
    ,
    c(
      "date",
      "broad_export_index",
      "broad_export_index_log_inflation"
    )
  ]
  
  quarterly <- merge(
    quarterly,
    keep_broad,
    by = "date",
    all = TRUE
  )
}


quarterly <- quarterly[
  order(quarterly$date),
]


# Restore time variables after merge if necessary

quarterly$year <- as.integer(
  format(quarterly$date, "%Y")
)

quarterly$month <- as.integer(
  format(quarterly$date, "%m")
)

quarterly$qtr <-
  ((quarterly$month - 1) %/% 3) + 1

quarterly$quarter <- paste0(
  quarterly$year,
  "Q",
  quarterly$qtr
)


# =============================================================================
# 8. RESTRICT PROCESSED QUARTERLY DATA TO MODEL PERIOD
# =============================================================================

quarterly <- quarterly[
  quarterly$date >= as.Date("1967-01-01") &
    quarterly$date <= as.Date("1980-10-01"),
]


# =============================================================================
# 9. VALIDATE QUARTERLY OBSERVATIONS
# =============================================================================

main_series <- c(
  "B639_index",
  "B686_index",
  "B687_index",
  "B640_index"
)

for (v in main_series) {
  
  if (any(is.na(quarterly[[v]]))) {
    stop(
      paste(
        "Missing observations detected in",
        v,
        "during 1967Q1-1980Q4."
      )
    )
  }
}


# =============================================================================
# 10. SAVE PROCESSED QUARTERLY DATA
# =============================================================================

quarterly_file <- file.path(
  "data",
  "processed",
  "foreign_price_quarterly.csv"
)

write.csv(
  quarterly,
  quarterly_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# =============================================================================
# 11. READ BRAZIL HISTORICAL IMPORT PRICE INDEX
# =============================================================================
#
# Workbook:
#
#   07_setor_externo_1901_00.xls
#
# Sheet:
#
#   "7"
#
# Column 1:
#
#   Year
#
# Column 7:
#
#   Importacao - Indice de preco implicito
#
# =============================================================================

brazil_raw <- readxl::read_excel(
  file_brazil,
  sheet = "7",
  col_names = FALSE
)


brazil <- data.frame(
  
  year = suppressWarnings(
    as.integer(
      as.character(
        brazil_raw[[1]]
      )
    )
  ),
  
  brazil_import_price = suppressWarnings(
    as.numeric(
      as.character(
        brazil_raw[[7]]
      )
    )
  )
)


brazil <- brazil[
  !is.na(brazil$year) &
    !is.na(brazil$brazil_import_price),
]


brazil <- brazil[
  brazil$year >= 1964 &
    brazil$year <= 1980,
]


brazil <- brazil[
  order(brazil$year),
]


if (nrow(brazil) == 0) {
  stop(
    "Could not extract Brazil import-price data."
  )
}


# Annual log inflation

brazil$brazil_import_log_inflation <-
  c(
    NA,
    diff(
      log(
        brazil$brazil_import_price
      )
    )
  )


# =============================================================================
# 12. HELPER: ANNUALIZE QUARTERLY PRICE INDEX
# =============================================================================
#
# We calculate:
#
#   A. annual average price level
#   B. Q4 price level
#
# The annual average is the baseline validation method.
#
# Q4/Q4 inflation is a robustness check.
#
# =============================================================================

annualize_price <- function(
    data,
    price_column,
    prefix
) {
  
  years <- sort(
    unique(data$year)
  )
  
  annual <- data.frame(
    year = years
  )
  
  
  # Annual average only when all 4 quarters exist
  
  annual_average <- vapply(
    years,
    function(y) {
      
      x <- data[
        data$year == y,
        price_column
      ]
      
      x <- x[
        !is.na(x)
      ]
      
      if (length(x) == 4) {
        mean(x)
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
  
  
  # Q4 level
  
  q4_value <- vapply(
    years,
    function(y) {
      
      x <- data[
        data$year == y &
          data$qtr == 4,
        price_column
      ]
      
      x <- x[
        !is.na(x)
      ]
      
      if (length(x) == 1) {
        x
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
  
  
  annual[[paste0(prefix, "_price_avg")]] <-
    annual_average
  
  annual[[paste0(prefix, "_price_q4")]] <-
    q4_value
  
  
  annual[[paste0(prefix, "_infl_avg")]] <-
    c(
      NA,
      diff(
        log(
          annual_average
        )
      )
    )
  
  annual[[paste0(prefix, "_infl_q4")]] <-
    c(
      NA,
      diff(
        log(
          q4_value
        )
      )
    )
  
  
  return(annual)
}


# =============================================================================
# 13. ANNUALIZE ALL CANDIDATE PROXIES
# =============================================================================

annual_B639 <- annualize_price(
  quarterly,
  "B639_index",
  "B639"
)

annual_B686 <- annualize_price(
  quarterly,
  "B686_index",
  "B686"
)

annual_B687 <- annualize_price(
  quarterly,
  "B687_index",
  "B687"
)

annual_B640 <- annualize_price(
  quarterly,
  "B640_index",
  "B640"
)


annual_validation <- merge(
  brazil,
  annual_B639,
  by = "year",
  all = TRUE
)

annual_validation <- merge(
  annual_validation,
  annual_B686,
  by = "year",
  all = TRUE
)

annual_validation <- merge(
  annual_validation,
  annual_B687,
  by = "year",
  all = TRUE
)

annual_validation <- merge(
  annual_validation,
  annual_B640,
  by = "year",
  all = TRUE
)


if (use_broad) {
  
  annual_broad <- annualize_price(
    quarterly,
    "broad_export_index",
    "broad"
  )
  
  annual_validation <- merge(
    annual_validation,
    annual_broad,
    by = "year",
    all = TRUE
  )
}


annual_validation <- annual_validation[
  order(annual_validation$year),
]


# =============================================================================
# 14. SAVE ANNUAL VALIDATION DATASET
# =============================================================================

annual_validation_file <- file.path(
  "data",
  "processed",
  "foreign_price_annual_validation.csv"
)

write.csv(
  annual_validation,
  annual_validation_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# =============================================================================
# 15. PROXY VALIDATION CORRELATIONS
# =============================================================================
#
# The baseline proxy is NOT chosen mechanically by maximum correlation.
#
# B639 is selected ex ante because it maps most directly to the imported
# intermediate-input variable z_t in the DSGE.
#
# Correlation is used to validate this theoretical choice.
#
# =============================================================================

validation_periods <- list(
  
  "1968-1972" = c(
    1968,
    1972
  ),
  
  "1973-1980" = c(
    1973,
    1980
  ),
  
  "1968-1980" = c(
    1968,
    1980
  )
)


validation_proxies <- c(
  B639 = "B639",
  B686 = "B686",
  B687 = "B687",
  B640 = "B640"
)

if (use_broad) {
  validation_proxies <- c(
    validation_proxies,
    broad = "broad"
  )
}


proxy_validation <- data.frame()


for (proxy in names(validation_proxies)) {
  
  prefix <- validation_proxies[[proxy]]
  
  for (
    annual_method in c(
      "avg",
      "q4"
    )
  ) {
    
    proxy_column <- paste0(
      prefix,
      "_infl_",
      annual_method
    )
    
    for (
      period_name in names(validation_periods)
    ) {
      
      bounds <- validation_periods[[period_name]]
      
      temp <- annual_validation[
        annual_validation$year >= bounds[1] &
          annual_validation$year <= bounds[2],
      ]
      
      complete <- complete.cases(
        temp$brazil_import_log_inflation,
        temp[[proxy_column]]
      )
      
      temp <- temp[
        complete,
      ]
      
      if (nrow(temp) >= 3) {
        
        pearson <- cor(
          temp$brazil_import_log_inflation,
          temp[[proxy_column]],
          method = "pearson"
        )
        
        spearman <- cor(
          temp$brazil_import_log_inflation,
          temp[[proxy_column]],
          method = "spearman"
        )
        
      } else {
        
        pearson <- NA_real_
        spearman <- NA_real_
        
      }
      
      
      new_row <- data.frame(
        
        proxy = proxy,
        
        role = ifelse(
          proxy == "B639",
          "Baseline",
          ifelse(
            proxy %in% c(
              "B686",
              "B687"
            ),
            "Robustness",
            "Diagnostic"
          )
        ),
        
        annualization = annual_method,
        
        period = period_name,
        
        n = nrow(temp),
        
        pearson_correlation = pearson,
        
        spearman_correlation = spearman,
        
        stringsAsFactors = FALSE
      )
      
      
      proxy_validation <- rbind(
        proxy_validation,
        new_row
      )
    }
  }
}


# =============================================================================
# 16. SAVE PROXY VALIDATION RESULTS
# =============================================================================

proxy_validation_file <- file.path(
  "output",
  "estimates",
  "rhoF_proxy_validation.csv"
)

write.csv(
  proxy_validation,
  proxy_validation_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# =============================================================================
# 17. PRINT BASELINE VALIDATION
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "B639 PROXY VALIDATION - ANNUAL AVERAGE METHOD\n"
)

cat(
  "============================================================\n"
)

print(
  proxy_validation[
    proxy_validation$proxy == "B639" &
      proxy_validation$annualization == "avg",
  ]
)


# =============================================================================
# 18. HELPER: ESTIMATE AR(1)
# =============================================================================
#
# Empirical specification:
#
#   pi_t = a + rho*pi_(t-1) + u_t
#
# Dynare specification:
#
#   pif_t = rhoF*pif_(t-1) + eps_pf,t
#
# The empirical intercept captures the unconditional mean.
#
# Dynare works with deviations around the steady-state mean, so only rhoF
# enters the zero-mean AR(1) model.
#
# =============================================================================

fit_ar1 <- function(
    data,
    inflation_column,
    proxy,
    role,
    sample_label,
    start_date,
    end_date
) {
  
  x <- data[
    order(data$date),
    c(
      "date",
      inflation_column
    )
  ]
  
  names(x)[2] <- "inflation"
  
  
  # Lag is constructed BEFORE filtering.
  # This preserves the correct lag observation at the sample boundary.
  
  x$lag1 <- c(
    NA,
    x$inflation[
      -nrow(x)
    ]
  )
  
  
  sample <- x[
    x$date >= as.Date(start_date) &
      x$date <= as.Date(end_date) &
      complete.cases(
        x$inflation,
        x$lag1
      ),
  ]
  
  
  if (nrow(sample) < 8) {
    stop(
      paste(
        "Too few observations for",
        proxy,
        sample_label
      )
    )
  }
  
  
  model <- lm(
    inflation ~ lag1,
    data = sample
  )
  
  
  coef_model <- coef(
    summary(model)
  )
  
  
  intercept <-
    coef(model)[
      "(Intercept)"
    ]
  
  rho <-
    coef(model)[
      "lag1"
    ]
  
  se_ols <-
    coef_model[
      "lag1",
      "Std. Error"
    ]
  
  
  # Heteroskedasticity-robust SE
  
  vcov_hc1 <-
    sandwich::vcovHC(
      model,
      type = "HC1"
    )
  
  se_hc1 <-
    sqrt(
      diag(
        vcov_hc1
      )
    )[
      "lag1"
    ]
  
  
  # Newey-West SE
  
  vcov_nw <-
    sandwich::NeweyWest(
      model,
      lag = 4,
      prewhite = FALSE,
      adjust = TRUE
    )
  
  se_newey_west <-
    sqrt(
      diag(
        vcov_nw
      )
    )[
      "lag1"
    ]
  
  
  # Shock standard deviation
  
  shock_sd <- sigma(
    model
  )
  
  
  # Unconditional mean
  
  implied_mean <-
    if (
      abs(rho) < 1
    ) {
      intercept / (1 - rho)
    } else {
      NA_real_
    }
  
  
  # Half-life in quarters
  
  half_life <-
    if (
      rho > 0 &&
      rho < 1
    ) {
      log(0.5) /
        log(rho)
    } else {
      NA_real_
    }
  
  
  # Residual autocorrelation diagnostics
  
  residuals_model <- residuals(
    model
  )
  
  
  lag4 <- min(
    4,
    length(residuals_model) - 1
  )
  
  lag8 <- min(
    8,
    length(residuals_model) - 1
  )
  
  
  ljung_box_p4 <-
    if (
      lag4 > 1
    ) {
      
      Box.test(
        residuals_model,
        lag = lag4,
        type = "Ljung-Box",
        fitdf = 1
      )$p.value
      
    } else {
      
      NA_real_
      
    }
  
  
  ljung_box_p8 <-
    if (
      lag8 > 1
    ) {
      
      Box.test(
        residuals_model,
        lag = lag8,
        type = "Ljung-Box",
        fitdf = 1
      )$p.value
      
    } else {
      
      NA_real_
      
    }
  
  
  result <- data.frame(
    
    proxy = proxy,
    
    role = role,
    
    sample = sample_label,
    
    start_date = as.character(
      min(sample$date)
    ),
    
    end_date = as.character(
      max(sample$date)
    ),
    
    n = nrow(sample),
    
    intercept = intercept,
    
    rhoF = rho,
    
    standard_error = se_ols,
    
    robust_se_HC1 = se_hc1,
    
    newey_west_se = se_newey_west,
    
    shock_sd = shock_sd,
    
    R_squared = summary(model)$r.squared,
    
    implied_mean = implied_mean,
    
    half_life_quarters = half_life,
    
    ljung_box_p_lag4 = ljung_box_p4,
    
    ljung_box_p_lag8 = ljung_box_p8,
    
    stringsAsFactors = FALSE
  )
  
  
  return(
    list(
      result = result,
      model = model,
      sample = sample
    )
  )
}


# =============================================================================
# 19. BASELINE AR(1): B639
# =============================================================================

baseline_fit <- fit_ar1(
  
  data = quarterly,
  
  inflation_column =
    "B639_index_log_inflation",
  
  proxy = "B639",
  
  role = "Baseline",
  
  sample_label =
    "1967Q3-1980Q4",
  
  start_date =
    "1967-07-01",
  
  end_date =
    "1980-10-01"
)


# =============================================================================
# 20. SAMPLE ROBUSTNESS: B639
# =============================================================================

start_1968_fit <- fit_ar1(
  
  data = quarterly,
  
  inflation_column =
    "B639_index_log_inflation",
  
  proxy = "B639",
  
  role = "Sample robustness",
  
  sample_label =
    "1968Q1-1980Q4",
  
  start_date =
    "1968-01-01",
  
  end_date =
    "1980-10-01"
)


pre_second_oil_fit <- fit_ar1(
  
  data = quarterly,
  
  inflation_column =
    "B639_index_log_inflation",
  
  proxy = "B639",
  
  role = "Sample robustness",
  
  sample_label =
    "1967Q3-1978Q4",
  
  start_date =
    "1967-07-01",
  
  end_date =
    "1978-10-01"
)


pre_first_oil_fit <- fit_ar1(
  
  data = quarterly,
  
  inflation_column =
    "B639_index_log_inflation",
  
  proxy = "B639",
  
  role = "Regime diagnostic",
  
  sample_label =
    "1967Q3-1972Q4",
  
  start_date =
    "1967-07-01",
  
  end_date =
    "1972-10-01"
)


oil_era_fit <- fit_ar1(
  
  data = quarterly,
  
  inflation_column =
    "B639_index_log_inflation",
  
  proxy = "B639",
  
  role = "Regime diagnostic",
  
  sample_label =
    "1973Q1-1980Q4",
  
  start_date =
    "1973-01-01",
  
  end_date =
    "1980-10-01"
)


# =============================================================================
# 21. PROXY ROBUSTNESS
# =============================================================================

B686_fit <- fit_ar1(
  
  quarterly,
  
  "B686_index_log_inflation",
  
  "B686",
  
  "Proxy robustness",
  
  "1967Q3-1980Q4",
  
  "1967-07-01",
  
  "1980-10-01"
)


B687_fit <- fit_ar1(
  
  quarterly,
  
  "B687_index_log_inflation",
  
  "B687",
  
  "Proxy robustness",
  
  "1967Q3-1980Q4",
  
  "1967-07-01",
  
  "1980-10-01"
)


# Capital goods = diagnostic only

B640_fit <- fit_ar1(
  
  quarterly,
  
  "B640_index_log_inflation",
  
  "B640",
  
  "Capital-goods diagnostic",
  
  "1967Q3-1980Q4",
  
  "1967-07-01",
  
  "1980-10-01"
)


# Optional broad export benchmark

if (use_broad) {
  
  broad_fit <- fit_ar1(
    
    quarterly,
    
    "broad_export_index_log_inflation",
    
    "Broad exports",
    
    "Broad-price diagnostic",
    
    "1967Q3-1980Q4",
    
    "1967-07-01",
    
    "1980-10-01"
  )
}


# =============================================================================
# 22. COMBINE ALL AR(1) RESULTS
# =============================================================================

rhoF_results <- rbind(
  
  baseline_fit$result,
  
  start_1968_fit$result,
  
  pre_second_oil_fit$result,
  
  pre_first_oil_fit$result,
  
  oil_era_fit$result,
  
  B686_fit$result,
  
  B687_fit$result,
  
  B640_fit$result
)


if (use_broad) {
  
  rhoF_results <- rbind(
    rhoF_results,
    broad_fit$result
  )
}


rhoF_results$recommended_baseline <-
  ifelse(
    rhoF_results$proxy == "B639" &
      rhoF_results$sample ==
      "1967Q3-1980Q4" &
      rhoF_results$role ==
      "Baseline",
    "YES",
    "NO"
  )


# =============================================================================
# 23. SAVE AR(1) RESULTS
# =============================================================================

rhoF_results_file <- file.path(
  "output",
  "estimates",
  "rhoF_ar1.csv"
)

write.csv(
  rhoF_results,
  rhoF_results_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# =============================================================================
# 24. STATIONARITY DIAGNOSTICS
# =============================================================================

stationarity_data <- quarterly[
  
  quarterly$date >=
    as.Date("1967-04-01") &
    
    quarterly$date <=
    as.Date("1980-10-01"),
  
  "B639_index_log_inflation"
]

stationarity_data <-
  stationarity_data[
    !is.na(stationarity_data)
  ]


set.seed(1964)


adf_result <- tseries::adf.test(
  stationarity_data
)

kpss_result <- tseries::kpss.test(
  stationarity_data,
  null = "Level"
)


stationarity_results <- data.frame(
  
  test = c(
    "ADF",
    "KPSS"
  ),
  
  null_hypothesis = c(
    "Unit root",
    "Level stationary"
  ),
  
  statistic = c(
    unname(
      adf_result$statistic
    ),
    unname(
      kpss_result$statistic
    )
  ),
  
  p_value = c(
    adf_result$p.value,
    kpss_result$p.value
  ),
  
  stringsAsFactors = FALSE
)


# =============================================================================
# 25. AR(1) vs AR(2) DIAGNOSTIC
# =============================================================================
#
# Both models use the SAME sample so AIC/BIC are comparable.
#
# =============================================================================

ar_compare <- quarterly[
  order(quarterly$date),
  c(
    "date",
    "B639_index_log_inflation"
  )
]

names(ar_compare)[2] <-
  "inflation"

ar_compare$lag1 <- c(
  NA,
  ar_compare$inflation[
    -nrow(ar_compare)
  ]
)

ar_compare$lag2 <- c(
  NA,
  NA,
  ar_compare$inflation[
    1:(nrow(ar_compare) - 2)
  ]
)


ar_compare <- ar_compare[
  
  ar_compare$date >=
    as.Date("1967-10-01") &
    
    ar_compare$date <=
    as.Date("1980-10-01") &
    
    complete.cases(
      ar_compare$inflation,
      ar_compare$lag1,
      ar_compare$lag2
    ),
]


ar1_common <- lm(
  inflation ~ lag1,
  data = ar_compare
)

ar2_common <- lm(
  inflation ~ lag1 + lag2,
  data = ar_compare
)


ar_model_comparison <- data.frame(
  
  model = c(
    "AR(1)",
    "AR(2)"
  ),
  
  n = c(
    nobs(ar1_common),
    nobs(ar2_common)
  ),
  
  AIC = c(
    AIC(ar1_common),
    AIC(ar2_common)
  ),
  
  BIC = c(
    BIC(ar1_common),
    BIC(ar2_common)
  ),
  
  stringsAsFactors = FALSE
)


# =============================================================================
# 26. RESIDUAL BOOTSTRAP FOR BASELINE rhoF
# =============================================================================
#
# Diagnostic uncertainty measure for the small sample.
#
# Residual bootstrap under the fitted AR(1).
#
# =============================================================================

bootstrap_B <- 5000

set.seed(1964)


baseline_model <-
  baseline_fit$model

baseline_sample <-
  baseline_fit$sample


a_hat <-
  coef(
    baseline_model
  )[
    "(Intercept)"
  ]

rho_hat <-
  coef(
    baseline_model
  )[
    "lag1"
  ]


resid_centered <-
  residuals(
    baseline_model
  ) -
  mean(
    residuals(
      baseline_model
    )
  )


n_boot_sample <-
  nrow(
    baseline_sample
  )


bootstrap_rho <- numeric(
  bootstrap_B
)


initial_value <-
  baseline_sample$lag1[1]


for (
  b in seq_len(
    bootstrap_B
  )
) {
  
  simulated <- numeric(
    n_boot_sample + 1
  )
  
  simulated[1] <-
    initial_value
  
  
  boot_errors <- sample(
    resid_centered,
    size = n_boot_sample,
    replace = TRUE
  )
  
  
  for (
    t in 2:(n_boot_sample + 1)
  ) {
    
    simulated[t] <-
      a_hat +
      rho_hat *
      simulated[t - 1] +
      boot_errors[t - 1]
  }
  
  
  boot_model <- lm(
    simulated[-1] ~
      simulated[
        -(n_boot_sample + 1)
      ]
  )
  
  
  bootstrap_rho[b] <-
    coef(
      boot_model
    )[2]
}


bootstrap_ci <-
  quantile(
    bootstrap_rho,
    probs = c(
      0.025,
      0.975
    ),
    na.rm = TRUE
  )


bootstrap_results <- data.frame(
  
  bootstrap_replications =
    bootstrap_B,
  
  rhoF_original =
    rho_hat,
  
  bootstrap_mean =
    mean(
      bootstrap_rho,
      na.rm = TRUE
    ),
  
  bootstrap_sd =
    sd(
      bootstrap_rho,
      na.rm = TRUE
    ),
  
  CI_2_5 =
    bootstrap_ci[1],
  
  CI_97_5 =
    bootstrap_ci[2],
  
  stringsAsFactors = FALSE
)


# =============================================================================
# 27. SAVE DIAGNOSTIC TABLES
# =============================================================================

write.csv(
  stationarity_results,
  file.path(
    "output",
    "estimates",
    "rhoF_stationarity_tests.csv"
  ),
  row.names = FALSE
)

write.csv(
  ar_model_comparison,
  file.path(
    "output",
    "estimates",
    "rhoF_ar_model_comparison.csv"
  ),
  row.names = FALSE
)

write.csv(
  bootstrap_results,
  file.path(
    "output",
    "estimates",
    "rhoF_bootstrap.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 28. ANNUAL PERSISTENCE VALIDATION
# =============================================================================
#
# This is NOT used to obtain quarterly rhoF.
#
# It is only a diagnostic comparison between:
#
#   actual Brazil import-price inflation
#
# and
#
#   annualized proxy inflation.
#
# =============================================================================

fit_annual_ar1 <- function(
    year,
    inflation,
    label
) {
  
  d <- data.frame(
    year = year,
    inflation = inflation
  )
  
  d <- d[
    d$year >= 1968 &
      d$year <= 1980,
  ]
  
  d$lag1 <- c(
    NA,
    d$inflation[
      -nrow(d)
    ]
  )
  
  d <- d[
    complete.cases(
      d$inflation,
      d$lag1
    ),
  ]
  
  model <- lm(
    inflation ~ lag1,
    data = d
  )
  
  data.frame(
    
    series = label,
    
    n = nrow(d),
    
    annual_rho = coef(model)[
      "lag1"
    ],
    
    standard_error =
      coef(summary(model))[
        "lag1",
        "Std. Error"
      ],
    
    stringsAsFactors = FALSE
  )
}


annual_persistence <- rbind(
  
  fit_annual_ar1(
    annual_validation$year,
    annual_validation$
      brazil_import_log_inflation,
    "Brazil import implicit price"
  ),
  
  fit_annual_ar1(
    annual_validation$year,
    annual_validation$B639_infl_avg,
    "B639"
  ),
  
  fit_annual_ar1(
    annual_validation$year,
    annual_validation$B686_infl_avg,
    "B686"
  ),
  
  fit_annual_ar1(
    annual_validation$year,
    annual_validation$B687_infl_avg,
    "B687"
  ),
  
  fit_annual_ar1(
    annual_validation$year,
    annual_validation$B640_infl_avg,
    "B640"
  )
)


write.csv(
  annual_persistence,
  file.path(
    "output",
    "estimates",
    "rhoF_annual_persistence_validation.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# 29. FIGURE 1: ANNUAL HISTORICAL VALIDATION
# =============================================================================

figure_validation <- file.path(
  "output",
  "figures",
  "foreign_price_proxy_annual_validation.png"
)


plot_data <- annual_validation[
  
  annual_validation$year >= 1968 &
    annual_validation$year <= 1980,
]


png(
  filename = figure_validation,
  width = 1200,
  height = 700,
  res = 120
)


plot(
  plot_data$year,
  100 *
    plot_data$
    brazil_import_log_inflation,
  type = "l",
  lwd = 2,
  xlab = "Year",
  ylab = "Annual log price inflation (%)",
  main =
    "Brazil Import Prices vs U.S. Industrial-Supply Export Prices",
  ylim = range(
    100 *
      plot_data$
      brazil_import_log_inflation,
    100 *
      plot_data$
      B639_infl_avg,
    na.rm = TRUE
  )
)


lines(
  plot_data$year,
  100 *
    plot_data$B639_infl_avg,
  lwd = 2,
  lty = 2
)


abline(
  h = 0,
  lty = 3
)

abline(
  v = c(
    1973,
    1979
  ),
  lty = 3
)


legend(
  "topleft",
  legend = c(
    "Brazil import implicit-price inflation",
    "B639 industrial supplies/materials"
  ),
  lty = c(
    1,
    2
  ),
  lwd = 2,
  bty = "n"
)


dev.off()


# =============================================================================
# 30. FIGURE 2: QUARTERLY FOREIGN-PRICE INFLATION
# =============================================================================

figure_quarterly <- file.path(
  "output",
  "figures",
  "foreign_price_quarterly_inflation.png"
)


png(
  filename = figure_quarterly,
  width = 1200,
  height = 700,
  res = 120
)


plot(
  quarterly$date,
  100 *
    quarterly$
    B639_index_log_inflation,
  type = "l",
  lwd = 2,
  xlab = "Quarter",
  ylab = "Quarterly log price inflation (%)",
  main =
    "U.S. Export Prices: Industrial Supplies and Materials"
)


abline(
  h = 0,
  lty = 3
)


abline(
  v = as.Date(
    c(
      "1973-01-01",
      "1979-01-01"
    )
  ),
  lty = 3
)


dev.off()


# =============================================================================
# 31. FIGURE 3: BASELINE RESIDUALS
# =============================================================================

figure_residuals <- file.path(
  "output",
  "figures",
  "rhoF_residuals.png"
)


png(
  filename = figure_residuals,
  width = 1200,
  height = 700,
  res = 120
)


plot(
  baseline_fit$sample$date,
  residuals(
    baseline_fit$model
  ),
  type = "l",
  lwd = 2,
  xlab = "Quarter",
  ylab = "AR(1) residual",
  main =
    "rhoF Baseline AR(1) Residuals"
)


abline(
  h = 0,
  lty = 3
)


dev.off()


# =============================================================================
# 32. FIGURE 4: ACF OF QUARTERLY INFLATION
# =============================================================================

png(
  filename = file.path(
    "output",
    "figures",
    "rhoF_inflation_acf.png"
  ),
  width = 1000,
  height = 700,
  res = 120
)

acf(
  stationarity_data,
  main =
    "ACF: B639 Quarterly Log Inflation"
)

dev.off()


# =============================================================================
# 33. FIGURE 5: PACF OF QUARTERLY INFLATION
# =============================================================================

png(
  filename = file.path(
    "output",
    "figures",
    "rhoF_inflation_pacf.png"
  ),
  width = 1000,
  height = 700,
  res = 120
)

pacf(
  stationarity_data,
  main =
    "PACF: B639 Quarterly Log Inflation"
)

dev.off()


# =============================================================================
# 34. EXTRACT FINAL BASELINE ESTIMATES
# =============================================================================

rhoF_final <-
  baseline_fit$result$rhoF[1]

rhoF_se <-
  baseline_fit$result$
  standard_error[1]

rhoF_hc1_se <-
  baseline_fit$result$
  robust_se_HC1[1]

rhoF_nw_se <-
  baseline_fit$result$
  newey_west_se[1]

eps_pf_sd <-
  baseline_fit$result$
  shock_sd[1]

rhoF_half_life <-
  baseline_fit$result$
  half_life_quarters[1]


if (
  abs(
    rhoF_final
  ) >= 1
) {
  
  warning(
    "Estimated rhoF is outside the stationary AR(1) region."
  )
}


# =============================================================================
# 35. UPDATE PARAMETER REGISTRY
# =============================================================================

registry_file <- file.path(
  "docs",
  "parameter_registry.csv"
)


if (!file.exists(registry_file)) {
  stop(
    "Cannot find docs/parameter_registry.csv"
  )
}


registry <- read.csv(
  registry_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


idx_rhoF <- which(
  registry$dynare_name == "rhoF"
)


if (
  length(
    idx_rhoF
  ) != 1
) {
  
  stop(
    paste0(
      "Expected exactly one rhoF row in parameter registry. Found ",
      length(idx_rhoF),
      "."
    )
  )
}


registry$classification[
  idx_rhoF
] <- "Estimated"


registry$research_value[
  idx_rhoF
] <- rhoF_final


registry$status[
  idx_rhoF
] <-
  "FIRST-STAGE ESTIMATE COMPLETE"


registry$method[
  idx_rhoF
] <-
  paste(
    "OLS AR(1) with intercept on quarterly log inflation",
    "of U.S. export price index for industrial supplies",
    "and materials (B639RG3Q086SBEA)."
  )


registry$sample[
  idx_rhoF
] <-
  "1967Q3-1980Q4"


registry$raw_or_primary_data[
  idx_rhoF
] <-
  paste(
    "data/raw/B639RG3Q086SBEA.csv;",
    "data/raw/07_setor_externo_1901_00.xls"
  )


registry$source_or_literature[
  idx_rhoF
] <-
  paste(
    "BEA via FRED, B639RG3Q086SBEA;",
    "Brazil historical import implicit-price index",
    "used for annual validation."
  )


registry$standard_error[
  idx_rhoF
] <- rhoF_se


registry$shock_sd[
  idx_rhoF
] <- eps_pf_sd


registry$notes[
  idx_rhoF
] <-
  paste(
    "B639 selected ex ante because the model's imported good",
    "is an intermediate production input.",
    "B686 and B687 provide proxy robustness.",
    "B640 capital goods is diagnostic only.",
    paste0(
      "HC1 SE = ",
      round(
        rhoF_hc1_se,
        6
      ),
      "."
    ),
    paste0(
      "Newey-West SE = ",
      round(
        rhoF_nw_se,
        6
      ),
      "."
    ),
    paste0(
      "Bootstrap 95% CI = [",
      round(
        bootstrap_ci[1],
        4
      ),
      ", ",
      round(
        bootstrap_ci[2],
        4
      ),
      "]."
    ),
    "See output/estimates/rhoF_ar1.csv."
  )


write.csv(
  registry,
  registry_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# =============================================================================
# 36. UPDATE DATA DICTIONARY
# =============================================================================

dictionary_file <- file.path(
  "docs",
  "data_dictionary.csv"
)


if (!file.exists(dictionary_file)) {
  stop(
    "Cannot find docs/data_dictionary.csv"
  )
}


data_dictionary <- read.csv(
  dictionary_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  colClasses = "character"
)


if (
  !"dataset_name" %in%
  names(
    data_dictionary
  )
) {
  
  stop(
    "data_dictionary.csv does not contain dataset_name."
  )
}


upsert_dictionary_row <- function(
    data,
    dataset_name,
    values
) {
  
  idx <- which(
    data$dataset_name ==
      dataset_name
  )
  
  
  if (
    length(
      idx
    ) > 1
  ) {
    
    stop(
      paste(
        "Duplicate dataset_name found:",
        dataset_name
      )
    )
  }
  
  
  if (
    length(
      idx
    ) == 0
  ) {
    
    new_row <- as.list(
      rep(
        "",
        ncol(
          data
        )
      )
    )
    
    names(
      new_row
    ) <- names(
      data
    )
    
    new_row$
      dataset_name <-
      dataset_name
    
    
    data <- rbind(
      data,
      as.data.frame(
        new_row,
        stringsAsFactors = FALSE
      )
    )
    
    
    idx <- nrow(
      data
    )
  }
  
  
  for (
    field in names(
      values
    )
  ) {
    
    if (
      field %in%
      names(
        data
      )
    ) {
      
      data[
        idx,
        field
      ] <-
        as.character(
          values[[field]]
        )
    }
  }
  
  
  return(
    data
  )
}


data_dictionary <- upsert_dictionary_row(
  
  data_dictionary,
  
  "foreign_price_b639",
  
  list(
    
    series_code =
      "B639RG3Q086SBEA",
    
    description =
      paste(
        "U.S. exports of goods:",
        "industrial supplies and materials,",
        "chain-type price index."
      ),
    
    raw_frequency =
      "Quarterly",
    
    raw_units =
      "Chain-type price index",
    
    transformation =
      "Quarterly log inflation = log(P_t) - log(P_(t-1))",
    
    target_sample =
      "1967Q3-1980Q4 AR(1) baseline",
    
    local_file =
      "data/raw/B639RG3Q086SBEA.csv",
    
    status =
      "PROCESSED - rhoF BASELINE COMPLETE",
    
    purpose =
      paste(
        "Baseline proxy for foreign price of",
        "imported industrial intermediate inputs;",
        "estimate rhoF and eps_pf shock SD."
      )
  )
)


data_dictionary <- upsert_dictionary_row(
  
  data_dictionary,
  
  "foreign_price_b686",
  
  list(
    
    series_code =
      "B686RG3Q086SBEA",
    
    description =
      paste(
        "U.S. export price index:",
        "durable industrial supplies and materials."
      ),
    
    raw_frequency =
      "Quarterly",
    
    raw_units =
      "Chain-type price index",
    
    transformation =
      "Quarterly log inflation",
    
    target_sample =
      "1967Q3-1980Q4",
    
    local_file =
      "data/raw/B686RG3Q086SBEA.csv",
    
    status =
      "PROCESSED - ROBUSTNESS COMPLETE",
    
    purpose =
      "rhoF proxy robustness"
  )
)


data_dictionary <- upsert_dictionary_row(
  
  data_dictionary,
  
  "foreign_price_b687",
  
  list(
    
    series_code =
      "B687RG3Q086SBEA",
    
    description =
      paste(
        "U.S. export price index:",
        "nondurable industrial supplies and materials."
      ),
    
    raw_frequency =
      "Quarterly",
    
    raw_units =
      "Chain-type price index",
    
    transformation =
      "Quarterly log inflation",
    
    target_sample =
      "1967Q3-1980Q4",
    
    local_file =
      "data/raw/B687RG3Q086SBEA.csv",
    
    status =
      "PROCESSED - ROBUSTNESS COMPLETE",
    
    purpose =
      "rhoF proxy robustness"
  )
)


data_dictionary <- upsert_dictionary_row(
  
  data_dictionary,
  
  "foreign_price_b640",
  
  list(
    
    series_code =
      "B640RG3Q086SBEA",
    
    description =
      paste(
        "U.S. export price index:",
        "capital goods except automotive."
      ),
    
    raw_frequency =
      "Quarterly",
    
    raw_units =
      "Chain-type price index",
    
    transformation =
      "Quarterly log inflation",
    
    target_sample =
      "1967Q3-1980Q4",
    
    local_file =
      "data/raw/B640RG3Q086SBEA.csv",
    
    status =
      "PROCESSED - DIAGNOSTIC COMPLETE",
    
    purpose =
      paste(
        "Capital-goods diagnostic;",
        "not baseline because the DSGE has no capital."
      )
  )
)


data_dictionary <- upsert_dictionary_row(
  
  data_dictionary,
  
  "brazil_import_price_annual",
  
  list(
    
    series_code =
      "Brazil external sector table 7",
    
    description =
      "Brazil annual import implicit price index",
    
    raw_frequency =
      "Annual",
    
    raw_units =
      "Implicit price index",
    
    transformation =
      "Annual log inflation",
    
    target_sample =
      "1968-1980 validation",
    
    local_file =
      "data/raw/07_setor_externo_1901_00.xls",
    
    status =
      "PROCESSED - VALIDATION COMPLETE",
    
    purpose =
      paste(
        "Historical Brazil-specific validation",
        "of quarterly foreign-price proxy."
      )
  )
)


write.csv(
  data_dictionary,
  dictionary_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# =============================================================================
# 37. CREATE PARAMETER DOCUMENTATION
# =============================================================================

parameter_doc_file <- file.path(
  "docs",
  "parameter_rhoF.md"
)


parameter_document <- c(
  
  "# Parameter Decision: rhoF",
  
  "",
  
  "## Model parameter",
  
  "",
  
  "`rhoF` governs persistence of foreign-price inflation:",
  
  "",
  
  "```text",
  "pif_t = rhoF * pif_(t-1) + eps_pf,t",
  "```",
  
  "",
  
  "## Economic interpretation",
  
  "",
  
  paste(
    "The model treats imported goods as intermediate production inputs.",
    "There is no capital stock in the model."
  ),
  
  "",
  
  paste(
    "The empirical foreign-price proxy therefore targets prices of",
    "industrial supplies and materials rather than consumer or capital goods."
  ),
  
  "",
  
  "## Baseline empirical proxy",
  
  "",
  
  "`B639RG3Q086SBEA`",
  
  "",
  
  paste(
    "U.S. exports of goods: industrial supplies and materials,",
    "chain-type price index."
  ),
  
  "",
  
  "## Historical validation",
  
  "",
  
  paste(
    "The quarterly proxy is validated against Brazil's annual historical",
    "import implicit-price index."
  ),
  
  "",
  
  paste(
    "The Brazilian annual series is not interpolated to quarterly frequency",
    "and is not used directly to estimate rhoF."
  ),
  
  "",
  
  "## Transformation",
  
  "",
  
  "Quarterly foreign-price inflation is:",
  
  "",
  
  "```text",
  "piF_t = log(P_t) - log(P_(t-1))",
  "```",
  
  "",
  
  "## Baseline estimation",
  
  "",
  
  "The empirical AR(1) is:",
  
  "",
  
  "```text",
  "piF_t = a + rhoF * piF_(t-1) + u_t",
  "```",
  
  "",
  
  "Baseline regression sample:",
  
  "",
  
  "`1967Q3-1980Q4`",
  
  "",
  
  "## Estimate",
  
  "",
  
  paste0(
    "`rhoF = ",
    format(
      rhoF_final,
      digits = 12
    ),
    "`"
  ),
  
  "",
  
  paste0(
    "Conventional OLS SE: ",
    format(
      rhoF_se,
      digits = 8
    )
  ),
  
  "",
  
  paste0(
    "HC1 robust SE: ",
    format(
      rhoF_hc1_se,
      digits = 8
    )
  ),
  
  "",
  
  paste0(
    "Newey-West SE: ",
    format(
      rhoF_nw_se,
      digits = 8
    )
  ),
  
  "",
  
  paste0(
    "Estimated quarterly shock SD: ",
    format(
      eps_pf_sd,
      digits = 8
    )
  ),
  
  "",
  
  paste0(
    "Implied half-life: ",
    round(
      rhoF_half_life,
      3
    ),
    " quarters."
  ),
  
  "",
  
  paste0(
    "Residual-bootstrap 95% interval: [",
    round(
      bootstrap_ci[1],
      4
    ),
    ", ",
    round(
      bootstrap_ci[2],
      4
    ),
    "]."
  ),
  
  "",
  
  "## Robustness",
  
  "",
  
  "- B686: durable industrial supplies/materials.",
  "- B687: nondurable industrial supplies/materials.",
  "- B640: capital-goods diagnostic only.",
  "- Alternative sample ending in 1978Q4.",
  "- Pre-1973 and 1973-1980 regime diagnostics.",
  "- Annual-average and Q4/Q4 proxy validation.",
  "- Pearson and Spearman validation correlations.",
  "- AR(1) versus AR(2) diagnostic.",
  "- ADF and KPSS stationarity diagnostics.",
  
  "",
  
  "## Reproducibility",
  
  "",
  
  "Estimation script:",
  
  "",
  
  "`R/05_foreign_price_process.R`",
  
  "",
  
  "Processed quarterly data:",
  
  "",
  
  "`data/processed/foreign_price_quarterly.csv`",
  
  "",
  
  "Historical validation data:",
  
  "",
  
  "`data/processed/foreign_price_annual_validation.csv`",
  
  "",
  
  "Main estimation output:",
  
  "",
  
  "`output/estimates/rhoF_ar1.csv`",
  
  "",
  
  "Proxy-validation output:",
  
  "",
  
  "`output/estimates/rhoF_proxy_validation.csv`"
)


writeLines(
  parameter_document,
  parameter_doc_file,
  useBytes = TRUE
)


# =============================================================================
# 38. UPDATE SOURCES.MD
# =============================================================================

sources_file <- file.path(
  "docs",
  "sources.md"
)


if (
  file.exists(
    sources_file
  )
) {
  
  sources_lines <- readLines(
    sources_file,
    warn = FALSE
  )
  
} else {
  
  sources_lines <- character(0)
}


start_marker <-
  "<!-- BEGIN RHO_F SOURCES -->"

end_marker <-
  "<!-- END RHO_F SOURCES -->"


rhoF_source_block <- c(
  
  start_marker,
  
  "",
  
  "## Foreign-price process (`rhoF`)",
  
  "",
  
  paste(
    "- `B639RG3Q086SBEA`: BEA/FRED quarterly export price index",
    "for industrial supplies and materials.",
    "Baseline foreign-price proxy."
  ),
  
  "",
  
  paste(
    "- `B686RG3Q086SBEA`: durable industrial supplies/materials.",
    "Proxy robustness."
  ),
  
  "",
  
  paste(
    "- `B687RG3Q086SBEA`: nondurable industrial supplies/materials.",
    "Proxy robustness."
  ),
  
  "",
  
  paste(
    "- `B640RG3Q086SBEA`: capital goods except automotive.",
    "Diagnostic only because the DSGE does not contain capital."
  ),
  
  "",
  
  paste(
    "- `07_setor_externo_1901_00.xls`: historical Brazil external-sector",
    "dataset containing the annual import implicit-price index.",
    "Used for proxy validation."
  ),
  
  "",
  
  end_marker
)


start_position <- which(
  sources_lines ==
    start_marker
)

end_position <- which(
  sources_lines ==
    end_marker
)


if (
  length(
    start_position
  ) == 1 &&
  length(
    end_position
  ) == 1 &&
  start_position <
  end_position
) {
  
  before <- if (
    start_position > 1
  ) {
    sources_lines[
      1:(start_position - 1)
    ]
  } else {
    character(0)
  }
  
  after <- if (
    end_position <
    length(
      sources_lines
    )
  ) {
    sources_lines[
      (end_position + 1):
        length(
          sources_lines
        )
    ]
  } else {
    character(0)
  }
  
  sources_lines <- c(
    before,
    rhoF_source_block,
    after
  )
  
} else {
  
  sources_lines <- c(
    sources_lines,
    "",
    rhoF_source_block
  )
}


writeLines(
  sources_lines,
  sources_file,
  useBytes = TRUE
)


# =============================================================================
# 39. FINAL RESULTS
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("FOREIGN-PRICE PROCESS ESTIMATION COMPLETE\n")
cat("============================================================\n")


cat(
  "\nBaseline proxy: B639RG3Q086SBEA\n"
)

cat(
  "Baseline sample: 1967Q3-1980Q4\n\n"
)


cat(
  "rhoF = ",
  format(
    rhoF_final,
    digits = 12
  ),
  "\n",
  sep = ""
)


cat(
  "OLS SE = ",
  format(
    rhoF_se,
    digits = 8
  ),
  "\n",
  sep = ""
)


cat(
  "HC1 robust SE = ",
  format(
    rhoF_hc1_se,
    digits = 8
  ),
  "\n",
  sep = ""
)


cat(
  "Newey-West SE = ",
  format(
    rhoF_nw_se,
    digits = 8
  ),
  "\n",
  sep = ""
)


cat(
  "eps_pf shock SD = ",
  format(
    eps_pf_sd,
    digits = 8
  ),
  "\n",
  sep = ""
)


cat(
  "Half-life = ",
  round(
    rhoF_half_life,
    3
  ),
  " quarters\n",
  sep = ""
)


cat(
  "Bootstrap 95% CI = [",
  round(
    bootstrap_ci[1],
    4
  ),
  ", ",
  round(
    bootstrap_ci[2],
    4
  ),
  "]\n",
  sep = ""
)


cat("\n")
cat("Robustness results:\n\n")


print(
  rhoF_results[
    ,
    c(
      "proxy",
      "role",
      "sample",
      "n",
      "rhoF",
      "standard_error",
      "robust_se_HC1",
      "shock_sd",
      "recommended_baseline"
    )
  ]
)


cat("\n")
cat("Stationarity diagnostics:\n\n")

print(
  stationarity_results
)


cat("\n")
cat("AR model comparison:\n\n")

print(
  ar_model_comparison
)


cat("\n")
cat("Dynare candidate values:\n\n")


cat(
  "rhoF = ",
  format(
    rhoF_final,
    digits = 12
  ),
  ";\n",
  sep = ""
)


cat(
  "stderr eps_pf = ",
  format(
    eps_pf_sd,
    digits = 12
  ),
  ";\n",
  sep = ""
)


cat("\n")
cat("Files created/updated:\n")

cat(
  " - data/processed/foreign_price_quarterly.csv\n"
)

cat(
  " - data/processed/foreign_price_annual_validation.csv\n"
)

cat(
  " - output/estimates/rhoF_ar1.csv\n"
)

cat(
  " - output/estimates/rhoF_proxy_validation.csv\n"
)

cat(
  " - output/estimates/rhoF_stationarity_tests.csv\n"
)

cat(
  " - output/estimates/rhoF_ar_model_comparison.csv\n"
)

cat(
  " - output/estimates/rhoF_bootstrap.csv\n"
)

cat(
  " - output/estimates/rhoF_annual_persistence_validation.csv\n"
)

cat(
  " - docs/parameter_registry.csv\n"
)

cat(
  " - docs/data_dictionary.csv\n"
)

cat(
  " - docs/parameter_rhoF.md\n"
)

cat(
  " - docs/sources.md\n"
)

cat("\n")
cat("============================================================\n")
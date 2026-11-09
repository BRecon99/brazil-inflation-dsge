# =============================================================================
# 05_foreign_price_process.R
#
# Parameter estimated:
#   rhoF
#
# Model equation:
#   pif_t = rhoF * pif_{t-1} + eps_pf,t
#
# Main empirical design:
#   1. Estimate quarterly persistence from the U.S. BEA/FRED export-price index
#      for industrial supplies and materials (B639RG3Q086SBEA).
#   2. Validate that quarterly proxy against Brazil-specific annual import-price
#      indexes from Serra (1982), Table 13.
#   3. Use Serra's intermediate-goods import-price index as the PRIMARY
#      historical validation target because it is the closest empirical match to
#      the model's imported-intermediate-input price concept.
#   4. Keep Serra's general import-price index as a SECONDARY validation target.
#   5. Estimate the Brazilian annual AR(1) only as a diagnostic. It is NOT used
#      as the quarterly Dynare rhoF because temporal aggregation changes the
#      stochastic process.
#
# Baseline quarterly proxy:
#   B639RG3Q086SBEA = U.S. exports of industrial supplies and materials,
#                     chain-type price index.
#
# Robustness / diagnostics:
#   B686RG3Q086SBEA = durable industrial supplies and materials
#   B687RG3Q086SBEA = nondurable industrial supplies and materials
#   B640RG3Q086SBEA = capital goods except automotive
#   USA_Exports.csv = optional broad U.S. export-price benchmark
#
# Brazilian annual validation database:
#   data/processed/brazil_import_price_serra_1963_1981.csv
#
# IMPORTANT:
#   - Quarterly rhoF comes from quarterly B639 inflation.
#   - Serra annual data validate the economic mapping.
#   - Do NOT convert Serra's annual rho into a quarterly rho by fourth-rooting.
# =============================================================================

options(stringsAsFactors = FALSE)

# =============================================================================
# 0. PACKAGES AND DIRECTORIES
# =============================================================================

required_packages <- c("sandwich", "tseries")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them first with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
estimates_dir <- file.path("output", "estimates")
figures_dir <- file.path("output", "figures")
docs_dir <- "docs"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(estimates_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. FILE PATHS
# =============================================================================

file_b639 <- file.path(raw_dir, "B639RG3Q086SBEA.csv")
file_b686 <- file.path(raw_dir, "B686RG3Q086SBEA.csv")
file_b687 <- file.path(raw_dir, "B687RG3Q086SBEA.csv")
file_b640 <- file.path(raw_dir, "B640RG3Q086SBEA.csv")
file_broad <- file.path(raw_dir, "USA_Exports.csv")

file_serra <- file.path(
  processed_dir,
  "brazil_import_price_serra_1963_1981.csv"
)

required_price_files <- c(
  file_b639,
  file_b686,
  file_b687,
  file_b640
)

missing_price_files <- required_price_files[
  !file.exists(required_price_files)
]

if (length(missing_price_files) > 0) {
  stop(
    "Missing required quarterly price file(s):\n - ",
    paste(missing_price_files, collapse = "\n - ")
  )
}

# If Serra processed database is missing, try to rebuild it using Script 00.
if (!file.exists(file_serra)) {
  
  script00 <- file.path(
    "R",
    "00_build_historical_source_database.R"
  )
  
  if (file.exists(script00)) {
    
    message(
      "Serra processed database not found. Running ",
      script00,
      "..."
    )
    
    source(
      script00,
      local = new.env()
    )
  }
}

if (!file.exists(file_serra)) {
  stop(
    "Missing Serra processed database: ",
    file_serra,
    "\nRun R/00_build_historical_source_database.R first."
  )
}

# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

quarter_string <- function(year, qtr) {
  paste0(year, "Q", qtr)
}


# -----------------------------------------------------------------------------
# Read a quarterly FRED/BEA price index
# -----------------------------------------------------------------------------

read_fred_price <- function(path, short_name) {
  
  x <- read.csv(
    path,
    check.names = FALSE,
    na.strings = c("", "NA", ".")
  )
  
  if (nrow(x) == 0) {
    stop(
      "File contains no observations: ",
      path
    )
  }
  
  date_candidates <- names(x)[
    tolower(names(x)) %in%
      c("observation_date", "date")
  ]
  
  if (length(date_candidates) == 0) {
    stop(
      "Could not identify the date column in ",
      path,
      ". Expected observation_date or DATE."
    )
  }
  
  date_col <- date_candidates[1]
  
  dates <- as.Date(x[[date_col]])
  
  if (any(is.na(dates))) {
    stop(
      "Could not parse all dates in: ",
      path
    )
  }
  
  candidate_cols <- setdiff(
    names(x),
    date_col
  )
  
  if (length(candidate_cols) == 0) {
    stop(
      "No price-index column found in: ",
      path
    )
  }
  
  numeric_counts <- vapply(
    candidate_cols,
    function(nm) {
      
      sum(
        !is.na(
          suppressWarnings(
            as.numeric(x[[nm]])
          )
        )
      )
    },
    numeric(1)
  )
  
  if (max(numeric_counts) == 0) {
    stop(
      "No numeric price series found in: ",
      path
    )
  }
  
  value_col <- candidate_cols[
    which.max(numeric_counts)
  ]
  
  values <- suppressWarnings(
    as.numeric(x[[value_col]])
  )
  
  year <- as.integer(
    format(dates, "%Y")
  )
  
  month <- as.integer(
    format(dates, "%m")
  )
  
  qtr <- ((month - 1L) %/% 3L) + 1L
  
  out <- data.frame(
    date = dates,
    year = year,
    qtr = qtr,
    quarter = quarter_string(
      year,
      qtr
    ),
    stringsAsFactors = FALSE
  )
  
  out[[short_name]] <- values
  
  out <- out[
    order(out$date),
  ]
  
  inflation_name <- paste0(
    short_name,
    "_log_inflation"
  )
  
  out[[inflation_name]] <- c(
    NA_real_,
    diff(
      log(out[[short_name]])
    )
  )
  
  out
}


# -----------------------------------------------------------------------------
# Annualize quarterly price index
#
# avg = annual average of four quarterly index observations
# q4  = Q4 price index
# -----------------------------------------------------------------------------

annualize_price <- function(
    qdata,
    price_col,
    proxy_name,
    method = c("avg", "q4")
) {
  
  method <- match.arg(method)
  
  years <- sort(
    unique(qdata$year)
  )
  
  rows <- vector(
    "list",
    length(years)
  )
  
  for (i in seq_along(years)) {
    
    y <- years[i]
    
    z <- qdata[
      qdata$year == y,
      c(
        "year",
        "qtr",
        price_col
      ),
      drop = FALSE
    ]
    
    names(z)[3] <- "price"
    
    complete_four <-
      sum(!is.na(z$price)) == 4
    
    if (method == "avg") {
      
      annual_price <-
        if (complete_four) {
          mean(
            z$price,
            na.rm = TRUE
          )
        } else {
          NA_real_
        }
      
    } else {
      
      q4_value <-
        z$price[z$qtr == 4]
      
      annual_price <-
        if (
          complete_four &&
          length(q4_value) == 1
        ) {
          q4_value
        } else {
          NA_real_
        }
    }
    
    rows[[i]] <- data.frame(
      proxy = proxy_name,
      annualization = method,
      year = y,
      us_price_index = annual_price,
      stringsAsFactors = FALSE
    )
  }
  
  out <- do.call(
    rbind,
    rows
  )
  
  out <- out[
    order(out$year),
  ]
  
  out$us_log_inflation <- c(
    NA_real_,
    diff(
      log(out$us_price_index)
    )
  )
  
  row.names(out) <- NULL
  
  out
}


# -----------------------------------------------------------------------------
# Safe correlation
# -----------------------------------------------------------------------------

safe_cor <- function(
    x,
    y,
    method
) {
  
  ok <- complete.cases(
    x,
    y
  )
  
  x <- x[ok]
  y <- y[ok]
  
  if (
    length(x) < 3 ||
    sd(x) == 0 ||
    sd(y) == 0
  ) {
    return(
      NA_real_
    )
  }
  
  cor(
    x,
    y,
    method = method
  )
}


period_label <- function(
    start_year,
    end_year
) {
  
  paste0(
    start_year,
    "-",
    end_year
  )
}


# -----------------------------------------------------------------------------
# Pearson and Spearman validation
# -----------------------------------------------------------------------------

validation_correlations <- function(
    validation_data,
    proxy_name,
    proxy_role,
    annualization_method,
    target_name,
    target_role,
    target_column,
    periods
) {
  
  z <- validation_data[
    validation_data$proxy == proxy_name &
      validation_data$annualization ==
      annualization_method,
    ,
    drop = FALSE
  ]
  
  result <- vector(
    "list",
    nrow(periods)
  )
  
  for (i in seq_len(nrow(periods))) {
    
    start_year <-
      periods$start_year[i]
    
    end_year <-
      periods$end_year[i]
    
    s <- z[
      z$year >= start_year &
        z$year <= end_year,
      ,
      drop = FALSE
    ]
    
    ok <- complete.cases(
      s$us_log_inflation,
      s[[target_column]]
    )
    
    result[[i]] <- data.frame(
      
      proxy = proxy_name,
      
      proxy_role = proxy_role,
      
      brazil_target = target_name,
      
      target_role = target_role,
      
      annualization =
        annualization_method,
      
      period = period_label(
        start_year,
        end_year
      ),
      
      n = sum(ok),
      
      pearson_correlation =
        safe_cor(
          s$us_log_inflation,
          s[[target_column]],
          "pearson"
        ),
      
      spearman_correlation =
        safe_cor(
          s$us_log_inflation,
          s[[target_column]],
          "spearman"
        ),
      
      stringsAsFactors = FALSE
    )
  }
  
  do.call(
    rbind,
    result
  )
}


# -----------------------------------------------------------------------------
# Quarterly AR(1)
# -----------------------------------------------------------------------------

fit_ar1 <- function(
    data,
    variable,
    start_year,
    start_qtr,
    end_year,
    end_qtr,
    proxy,
    role,
    recommended = "NO"
) {
  
  z <- data[
    order(
      data$year,
      data$qtr
    ),
    c(
      "year",
      "qtr",
      variable
    ),
    drop = FALSE
  ]
  
  names(z)[3] <- "x"
  
  # IMPORTANT:
  # create lag before selecting sample.
  z$lag_x <- c(
    NA_real_,
    head(
      z$x,
      -1
    )
  )
  
  start_index <-
    start_year * 4 +
    start_qtr
  
  end_index <-
    end_year * 4 +
    end_qtr
  
  z_index <-
    z$year * 4 +
    z$qtr
  
  z <- z[
    z_index >= start_index &
      z_index <= end_index &
      complete.cases(
        z$x,
        z$lag_x
      ),
    ,
    drop = FALSE
  ]
  
  if (nrow(z) < 8) {
    stop(
      "Too few observations for AR(1): ",
      proxy,
      " ",
      role
    )
  }
  
  model <- lm(
    x ~ lag_x,
    data = z
  )
  
  coefs <- coef(model)
  
  vc_ols <- vcov(model)
  
  vc_hc1 <- sandwich::vcovHC(
    model,
    type = "HC1"
  )
  
  vc_nw <- sandwich::NeweyWest(
    model,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  intercept <- unname(
    coefs["(Intercept)"]
  )
  
  rho <- unname(
    coefs["lag_x"]
  )
  
  implied_mean <-
    if (
      is.finite(rho) &&
      abs(1 - rho) > 1e-10
    ) {
      
      intercept /
        (1 - rho)
      
    } else {
      
      NA_real_
    }
  
  half_life <-
    if (
      is.finite(rho) &&
      rho > 0 &&
      rho < 1
    ) {
      
      log(0.5) /
        log(rho)
      
    } else {
      
      NA_real_
    }
  
  lb4 <- Box.test(
    residuals(model),
    lag = 4,
    type = "Ljung-Box",
    fitdf = 1
  )
  
  lb8 <- Box.test(
    residuals(model),
    lag = 8,
    type = "Ljung-Box",
    fitdf = 1
  )
  
  summary_row <- data.frame(
    
    proxy = proxy,
    
    role = role,
    
    sample = paste0(
      start_year,
      "Q",
      start_qtr,
      "-",
      end_year,
      "Q",
      end_qtr
    ),
    
    n = nrow(z),
    
    intercept = intercept,
    
    rhoF = rho,
    
    standard_error =
      sqrt(
        vc_ols[
          "lag_x",
          "lag_x"
        ]
      ),
    
    robust_se_HC1 =
      sqrt(
        vc_hc1[
          "lag_x",
          "lag_x"
        ]
      ),
    
    newey_west_se =
      sqrt(
        vc_nw[
          "lag_x",
          "lag_x"
        ]
      ),
    
    shock_sd =
      sigma(model),
    
    implied_unconditional_mean =
      implied_mean,
    
    half_life_quarters =
      half_life,
    
    ljung_box_p_lag4 =
      unname(
        lb4$p.value
      ),
    
    ljung_box_p_lag8 =
      unname(
        lb8$p.value
      ),
    
    recommended_baseline =
      recommended,
    
    stringsAsFactors = FALSE
  )
  
  list(
    summary = summary_row,
    model = model,
    data = z
  )
}


# -----------------------------------------------------------------------------
# Annual AR(1)
#
# DIAGNOSTIC ONLY.
# This is NOT the quarterly Dynare rhoF.
# -----------------------------------------------------------------------------

fit_annual_ar1 <- function(
    data,
    value_col,
    start_year,
    end_year,
    series_name,
    role
) {
  
  z <- data[
    order(data$year),
    c(
      "year",
      value_col
    ),
    drop = FALSE
  ]
  
  names(z)[2] <- "x"
  
  z$lag_x <- c(
    NA_real_,
    head(
      z$x,
      -1
    )
  )
  
  z <- z[
    z$year >= start_year &
      z$year <= end_year &
      complete.cases(
        z$x,
        z$lag_x
      ),
    ,
    drop = FALSE
  ]
  
  if (nrow(z) < 4) {
    
    return(
      data.frame(
        
        series =
          series_name,
        
        role =
          role,
        
        sample =
          paste0(
            start_year,
            "-",
            end_year
          ),
        
        n =
          nrow(z),
        
        intercept =
          NA_real_,
        
        rho_annual =
          NA_real_,
        
        standard_error =
          NA_real_,
        
        robust_se_HC1 =
          NA_real_,
        
        shock_sd =
          NA_real_,
        
        note =
          paste(
            "Too few annual observations",
            "for a meaningful AR(1) diagnostic"
          ),
        
        stringsAsFactors =
          FALSE
      )
    )
  }
  
  model <- lm(
    x ~ lag_x,
    data = z
  )
  
  vc_hc1 <- sandwich::vcovHC(
    model,
    type = "HC1"
  )
  
  data.frame(
    
    series =
      series_name,
    
    role =
      role,
    
    sample =
      paste0(
        start_year,
        "-",
        end_year
      ),
    
    n =
      nrow(z),
    
    intercept =
      unname(
        coef(model)[
          "(Intercept)"
        ]
      ),
    
    rho_annual =
      unname(
        coef(model)[
          "lag_x"
        ]
      ),
    
    standard_error =
      sqrt(
        vcov(model)[
          "lag_x",
          "lag_x"
        ]
      ),
    
    robust_se_HC1 =
      sqrt(
        vc_hc1[
          "lag_x",
          "lag_x"
        ]
      ),
    
    shock_sd =
      sigma(model),
    
    note =
      paste(
        "Annual-frequency diagnostic only;",
        "do not convert mechanically to quarterly rhoF"
      ),
    
    stringsAsFactors =
      FALSE
  )
}


# -----------------------------------------------------------------------------
# Residual bootstrap
# -----------------------------------------------------------------------------

bootstrap_ar1_rho <- function(
    model,
    observed_x,
    B = 5000,
    seed = 1964
) {
  
  set.seed(seed)
  
  a_hat <- unname(
    coef(model)[
      "(Intercept)"
    ]
  )
  
  rho_hat <- unname(
    coef(model)[
      "lag_x"
    ]
  )
  
  residual_pool <-
    residuals(model) -
    mean(
      residuals(model)
    )
  
  n <- length(
    observed_x
  )
  
  draws <- rep(
    NA_real_,
    B
  )
  
  for (b in seq_len(B)) {
    
    x_star <- rep(
      NA_real_,
      n
    )
    
    x_star[1] <-
      observed_x[1]
    
    for (t in 2:n) {
      
      u_star <- sample(
        residual_pool,
        size = 1,
        replace = TRUE
      )
      
      x_star[t] <-
        a_hat +
        rho_hat *
        x_star[t - 1] +
        u_star
    }
    
    boot_df <- data.frame(
      
      x =
        x_star[
          2:n
        ],
      
      lag_x =
        x_star[
          1:(n - 1)
        ]
    )
    
    boot_model <- lm(
      x ~ lag_x,
      data = boot_df
    )
    
    draws[b] <- unname(
      coef(boot_model)[
        "lag_x"
      ]
    )
  }
  
  draws
}

# =============================================================================
# 3. READ QUARTERLY U.S. PRICE SERIES
# =============================================================================

b639 <- read_fred_price(
  file_b639,
  "B639"
)

b686 <- read_fred_price(
  file_b686,
  "B686"
)

b687 <- read_fred_price(
  file_b687,
  "B687"
)

b640 <- read_fred_price(
  file_b640,
  "B640"
)

cat(
  "\nQuarterly price-series coverage:\n"
)

for (
  obj_name in
  c(
    "b639",
    "b686",
    "b687",
    "b640"
  )
) {
  
  obj <- get(
    obj_name
  )
  
  short <- toupper(
    obj_name
  )
  
  first_index <-
    which(
      !is.na(
        obj[[short]]
      )
    )[1]
  
  last_quarter <-
    tail(
      obj$quarter[
        !is.na(
          obj[[short]]
        )
      ],
      1
    )
  
  cat(
    short,
    ": ",
    obj$quarter[first_index],
    " to ",
    last_quarter,
    "\n",
    sep = ""
  )
}

broad_available <-
  file.exists(
    file_broad
  )

if (broad_available) {
  
  broad <- read_fred_price(
    file_broad,
    "Broad_exports"
  )
  
  cat(
    "Broad export benchmark found and included.\n"
  )
  
} else {
  
  cat(
    paste(
      "Broad export benchmark not found;",
      "continuing without it.\n"
    )
  )
}

# =============================================================================
# 4. MERGE QUARTERLY SERIES
# =============================================================================

series_list <- list(
  
  b639[
    ,
    c(
      "date",
      "year",
      "qtr",
      "quarter",
      "B639",
      "B639_log_inflation"
    )
  ],
  
  b686[
    ,
    c(
      "date",
      "B686",
      "B686_log_inflation"
    )
  ],
  
  b687[
    ,
    c(
      "date",
      "B687",
      "B687_log_inflation"
    )
  ],
  
  b640[
    ,
    c(
      "date",
      "B640",
      "B640_log_inflation"
    )
  ]
)

if (broad_available) {
  
  series_list[[length(series_list) + 1]] <- broad[
    ,
    c(
      "date",
      "Broad_exports",
      "Broad_exports_log_inflation"
    )
  ]
}

quarterly <- Reduce(
  
  function(x, y) {
    
    merge(
      x,
      y,
      by = "date",
      all = TRUE
    )
  },
  
  series_list
)

quarterly$year <-
  as.integer(
    format(
      quarterly$date,
      "%Y"
    )
  )

quarterly$month <-
  as.integer(
    format(
      quarterly$date,
      "%m"
    )
  )

quarterly$qtr <-
  (
    (
      quarterly$month -
        1L
    ) %/% 3L
  ) + 1L

quarterly$quarter <-
  quarter_string(
    quarterly$year,
    quarterly$qtr
  )

quarterly <-
  quarterly[
    order(
      quarterly$date
    ),
  ]

quarterly <-
  quarterly[
    quarterly$year >= 1967 &
      quarterly$year <= 1980,
    ,
    drop = FALSE
  ]

write.csv(
  quarterly,
  file.path(
    processed_dir,
    "foreign_price_quarterly.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 5. READ SERRA BRAZILIAN IMPORT-PRICE DATABASE
# =============================================================================

serra <- read.csv(
  file_serra,
  check.names = FALSE,
  na.strings = c("", "NA")
)

required_serra_cols <- c(
  
  "year",
  
  "general_import_price_index",
  
  "intermediate_goods_price_index",
  
  "log_inflation_general_import",
  
  "log_inflation_intermediate_goods",
  
  "usable_full_year_for_rhoF_validation"
)

if (
  !all(
    required_serra_cols %in%
    names(serra)
  )
) {
  
  stop(
    paste0(
      "Serra processed database does not contain ",
      "the expected columns.\nMissing: ",
      paste(
        setdiff(
          required_serra_cols,
          names(serra)
        ),
        collapse = ", "
      )
    )
  )
}

serra$year <-
  as.integer(
    serra$year
  )

if (
  !is.logical(
    serra$usable_full_year_for_rhoF_validation
  )
) {
  
  serra$usable_full_year_for_rhoF_validation <-
    tolower(
      as.character(
        serra$usable_full_year_for_rhoF_validation
      )
    ) %in%
    c(
      "true",
      "t",
      "1",
      "yes"
    )
}

# 1981 is Jan-Oct in Serra and therefore excluded.
serra_validation <-
  serra[
    serra$usable_full_year_for_rhoF_validation &
      serra$year <= 1980,
    ,
    drop = FALSE
  ]

if (
  min(
    serra_validation$year,
    na.rm = TRUE
  ) > 1963 ||
  max(
    serra_validation$year,
    na.rm = TRUE
  ) < 1980
) {
  
  warning(
    paste(
      "Unexpected Serra validation coverage.",
      "Expected full years through 1980."
    )
  )
}

# =============================================================================
# 6. ANNUALIZE U.S. QUARTERLY PRICE INDEXES
# =============================================================================

proxy_specs <- data.frame(
  
  price_col = c(
    "B639",
    "B686",
    "B687",
    "B640"
  ),
  
  proxy = c(
    "B639",
    "B686",
    "B687",
    "B640"
  ),
  
  role = c(
    "Baseline",
    "Proxy robustness",
    "Proxy robustness",
    "Capital-goods diagnostic"
  ),
  
  stringsAsFactors = FALSE
)

if (broad_available) {
  
  proxy_specs <- rbind(
    
    proxy_specs,
    
    data.frame(
      
      price_col =
        "Broad_exports",
      
      proxy =
        "Broad exports",
      
      role =
        "Broad-price diagnostic",
      
      stringsAsFactors =
        FALSE
    )
  )
}

annual_parts <- list()

k <- 1

for (
  i in
  seq_len(
    nrow(proxy_specs)
  )
) {
  
  for (
    method in
    c(
      "avg",
      "q4"
    )
  ) {
    
    a <- annualize_price(
      
      quarterly,
      
      proxy_specs$price_col[i],
      
      proxy_specs$proxy[i],
      
      method
    )
    
    a$proxy_role <-
      proxy_specs$role[i]
    
    annual_parts[[k]] <-
      a
    
    k <- k + 1
  }
}

annual_us <- do.call(
  rbind,
  annual_parts
)

row.names(
  annual_us
) <- NULL

# Attach Serra's Brazilian data.
annual_validation <- merge(
  
  annual_us,
  
  serra_validation[
    ,
    c(
      "year",
      
      "intermediate_goods_price_index",
      
      "log_inflation_intermediate_goods",
      
      "general_import_price_index",
      
      "log_inflation_general_import"
    )
  ],
  
  by = "year",
  
  all.x = TRUE
)

annual_validation <-
  annual_validation[
    order(
      annual_validation$proxy,
      annual_validation$annualization,
      annual_validation$year
    ),
    ,
    drop = FALSE
  ]

write.csv(
  
  annual_validation,
  
  file.path(
    processed_dir,
    "foreign_price_annual_validation.csv"
  ),
  
  row.names = FALSE,
  
  na = ""
)

# =============================================================================
# 7. VALIDATE U.S. PROXY AGAINST SERRA
# =============================================================================

validation_periods <- data.frame(
  
  start_year = c(
    1968,
    1973,
    1968
  ),
  
  end_year = c(
    1972,
    1980,
    1980
  )
)

validation_results <- list()

k <- 1

for (
  i in
  seq_len(
    nrow(proxy_specs)
  )
) {
  
  for (
    method in
    c(
      "avg",
      "q4"
    )
  ) {
    
    # -------------------------------------------------------------------------
    # PRIMARY historical validation:
    # Serra intermediate-goods import prices
    # -------------------------------------------------------------------------
    
    validation_results[[k]] <-
      validation_correlations(
        
        annual_validation,
        
        proxy_specs$proxy[i],
        
        proxy_specs$role[i],
        
        method,
        
        "Serra intermediate-goods import price",
        
        "PRIMARY historical validation",
        
        "log_inflation_intermediate_goods",
        
        validation_periods
      )
    
    k <- k + 1
    
    # -------------------------------------------------------------------------
    # SECONDARY historical validation:
    # Serra general import-price index
    # -------------------------------------------------------------------------
    
    validation_results[[k]] <-
      validation_correlations(
        
        annual_validation,
        
        proxy_specs$proxy[i],
        
        proxy_specs$role[i],
        
        method,
        
        "Serra general import price",
        
        "SECONDARY historical validation",
        
        "log_inflation_general_import",
        
        validation_periods
      )
    
    k <- k + 1
  }
}

proxy_validation <- do.call(
  rbind,
  validation_results
)

row.names(
  proxy_validation
) <- NULL

write.csv(
  
  proxy_validation,
  
  file.path(
    estimates_dir,
    "rhoF_proxy_validation.csv"
  ),
  
  row.names = FALSE,
  
  na = ""
)

cat(
  "\n============================================================\n"
)

cat(
  "B639 vs SERRA INTERMEDIATE-GOODS PRICES - ANNUAL AVERAGE\n"
)

cat(
  "============================================================\n"
)

print(
  
  proxy_validation[
    proxy_validation$proxy == "B639" &
      proxy_validation$annualization == "avg" &
      proxy_validation$brazil_target ==
      "Serra intermediate-goods import price",
    c(
      "proxy",
      "proxy_role",
      "annualization",
      "period",
      "n",
      "pearson_correlation",
      "spearman_correlation"
    )
  ],
  
  row.names = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "B639 vs SERRA GENERAL IMPORT PRICES - SECONDARY CHECK\n"
)

cat(
  "============================================================\n"
)

print(
  
  proxy_validation[
    proxy_validation$proxy == "B639" &
      proxy_validation$annualization == "avg" &
      proxy_validation$brazil_target ==
      "Serra general import price",
    c(
      "proxy",
      "proxy_role",
      "annualization",
      "period",
      "n",
      "pearson_correlation",
      "spearman_correlation"
    )
  ],
  
  row.names = FALSE
)

# =============================================================================
# 8. QUARTERLY AR(1) ESTIMATION OF rhoF
# =============================================================================

ar_results <- list()

k <- 1

# -----------------------------------------------------------------------------
# Baseline
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B639_log_inflation",
  
  1967,
  3,
  
  1980,
  4,
  
  "B639",
  
  "Baseline",
  
  "YES"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Sample robustness: start 1968Q1
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B639_log_inflation",
  
  1968,
  1,
  
  1980,
  4,
  
  "B639",
  
  "Sample robustness",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Sample robustness: exclude second oil shock
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B639_log_inflation",
  
  1967,
  3,
  
  1978,
  4,
  
  "B639",
  
  "Sample robustness",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Regime diagnostic: pre-1973
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B639_log_inflation",
  
  1967,
  3,
  
  1972,
  4,
  
  "B639",
  
  "Regime diagnostic",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Regime diagnostic: oil-shock period
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B639_log_inflation",
  
  1973,
  1,
  
  1980,
  4,
  
  "B639",
  
  "Regime diagnostic",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Durable industrial supplies
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B686_log_inflation",
  
  1967,
  3,
  
  1980,
  4,
  
  "B686",
  
  "Proxy robustness",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Nondurable industrial supplies
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B687_log_inflation",
  
  1967,
  3,
  
  1980,
  4,
  
  "B687",
  
  "Proxy robustness",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Capital-goods diagnostic
# -----------------------------------------------------------------------------

ar_results[[k]] <- fit_ar1(
  
  quarterly,
  
  "B640_log_inflation",
  
  1967,
  3,
  
  1980,
  4,
  
  "B640",
  
  "Capital-goods diagnostic",
  
  "NO"
)

k <- k + 1


# -----------------------------------------------------------------------------
# Broad-export benchmark
# -----------------------------------------------------------------------------

if (broad_available) {
  
  ar_results[[k]] <- fit_ar1(
    
    quarterly,
    
    "Broad_exports_log_inflation",
    
    1967,
    3,
    
    1980,
    4,
    
    "Broad exports",
    
    "Broad-price diagnostic",
    
    "NO"
  )
  
  k <- k + 1
}


rhoF_ar1 <- do.call(
  
  rbind,
  
  lapply(
    ar_results,
    function(x) {
      x$summary
    }
  )
)

row.names(
  rhoF_ar1
) <- NULL

write.csv(
  
  rhoF_ar1,
  
  file.path(
    estimates_dir,
    "rhoF_ar1.csv"
  ),
  
  row.names = FALSE
)

baseline_fit <-
  ar_results[[1]]

baseline_model <-
  baseline_fit$model

baseline_data <-
  baseline_fit$data

baseline_row <-
  baseline_fit$summary[1, ]

# =============================================================================
# 9. STATIONARITY DIAGNOSTICS
# =============================================================================

baseline_x <-
  baseline_data$x

adf_result <- suppressWarnings(
  
  tseries::adf.test(
    baseline_x,
    alternative = "stationary"
  )
)

kpss_result <- suppressWarnings(
  
  tseries::kpss.test(
    baseline_x,
    null = "Level",
    lshort = TRUE
  )
)

stationarity_tests <- data.frame(
  
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
  
  interpretation_rule = c(
    "Small p-value rejects unit root",
    "Small p-value rejects level stationarity"
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  
  stationarity_tests,
  
  file.path(
    estimates_dir,
    "rhoF_stationarity_tests.csv"
  ),
  
  row.names = FALSE
)

# =============================================================================
# 10. AR(1) VERSUS AR(2)
# =============================================================================

ar_compare <- quarterly[
  
  quarterly$year * 4 +
    quarterly$qtr >=
    1967 * 4 + 3 &
    
    quarterly$year * 4 +
    quarterly$qtr <=
    1980 * 4 + 4,
  
  c(
    "year",
    "qtr",
    "B639_log_inflation"
  ),
  
  drop = FALSE
]

names(
  ar_compare
)[3] <- "x"

ar_compare$lag1 <- c(
  NA_real_,
  head(
    ar_compare$x,
    -1
  )
)

ar_compare$lag2 <- c(
  NA_real_,
  NA_real_,
  head(
    ar_compare$x,
    -2
  )
)

ar_compare <-
  ar_compare[
    complete.cases(
      ar_compare
    ),
  ]

ar1_common <- lm(
  x ~ lag1,
  data = ar_compare
)

ar2_common <- lm(
  x ~ lag1 + lag2,
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

write.csv(
  
  ar_model_comparison,
  
  file.path(
    estimates_dir,
    "rhoF_ar_model_comparison.csv"
  ),
  
  row.names = FALSE
)

# =============================================================================
# 11. RESIDUAL BOOTSTRAP
# =============================================================================

bootstrap_draws <- bootstrap_ar1_rho(
  
  baseline_model,
  
  baseline_x,
  
  B = 5000,
  
  seed = 1964
)

bootstrap_ci <- quantile(
  
  bootstrap_draws,
  
  probs = c(
    0.025,
    0.5,
    0.975
  ),
  
  na.rm = TRUE,
  
  names = FALSE
)

bootstrap_output <- data.frame(
  
  draw =
    seq_along(
      bootstrap_draws
    ),
  
  rhoF_bootstrap =
    bootstrap_draws
)

write.csv(
  
  bootstrap_output,
  
  file.path(
    estimates_dir,
    "rhoF_bootstrap.csv"
  ),
  
  row.names = FALSE
)

# =============================================================================
# 12. ANNUAL BRAZILIAN PERSISTENCE DIAGNOSTICS
#
# Again:
# these values are NOT the Dynare quarterly rhoF.
# =============================================================================

annual_persistence <- rbind(
  
  fit_annual_ar1(
    
    serra_validation,
    
    "log_inflation_intermediate_goods",
    
    1964,
    
    1980,
    
    "Serra intermediate-goods import inflation",
    
    "Brazil annual full-sample diagnostic"
  ),
  
  fit_annual_ar1(
    
    serra_validation,
    
    "log_inflation_intermediate_goods",
    
    1964,
    
    1972,
    
    "Serra intermediate-goods import inflation",
    
    "Brazil annual pre-oil diagnostic"
  ),
  
  fit_annual_ar1(
    
    serra_validation,
    
    "log_inflation_intermediate_goods",
    
    1973,
    
    1980,
    
    "Serra intermediate-goods import inflation",
    
    "Brazil annual oil-era diagnostic"
  )
)


# Compare with annualized B639 at the same annual frequency.

b639_avg <- annual_validation[
  
  annual_validation$proxy ==
    "B639" &
    
    annual_validation$annualization ==
    "avg",
  
  c(
    "year",
    "us_log_inflation"
  ),
  
  drop = FALSE
]


annual_persistence <- rbind(
  
  annual_persistence,
  
  fit_annual_ar1(
    
    b639_avg,
    
    "us_log_inflation",
    
    1968,
    
    1980,
    
    "B639 annualized-average inflation",
    
    "U.S. annualized diagnostic"
  )
)


row.names(
  annual_persistence
) <- NULL


write.csv(
  
  annual_persistence,
  
  file.path(
    estimates_dir,
    "rhoF_annual_persistence_validation.csv"
  ),
  
  row.names = FALSE,
  
  na = ""
)

# =============================================================================
# 13. FIGURES
# =============================================================================

# -----------------------------------------------------------------------------
# 13.1 Brazil intermediate-import inflation versus annualized B639
# -----------------------------------------------------------------------------

plot_data <- annual_validation[
  
  annual_validation$proxy ==
    "B639" &
    
    annual_validation$annualization ==
    "avg" &
    
    annual_validation$year >= 1968 &
    
    annual_validation$year <= 1980,
  
  ,
  
  drop = FALSE
]


png(
  
  file.path(
    figures_dir,
    "foreign_price_proxy_annual_validation.png"
  ),
  
  width = 1400,
  
  height = 900,
  
  res = 150
)


plot(
  
  plot_data$year,
  
  100 *
    plot_data$log_inflation_intermediate_goods,
  
  type = "o",
  
  pch = 16,
  
  xlab = "Year",
  
  ylab = "Annual log inflation (%)",
  
  main =
    paste(
      "Brazil Intermediate-Import Prices",
      "vs Annualized B639"
    )
)


lines(
  
  plot_data$year,
  
  100 *
    plot_data$us_log_inflation,
  
  type = "o",
  
  pch = 1,
  
  lty = 2
)


legend(
  
  "topleft",
  
  legend = c(
    "Brazil: Serra intermediate goods",
    "U.S.: B639 annual average"
  ),
  
  lty = c(
    1,
    2
  ),
  
  pch = c(
    16,
    1
  ),
  
  bty = "n"
)


abline(
  h = 0,
  lty = 3
)

dev.off()


# -----------------------------------------------------------------------------
# 13.2 Quarterly B639 inflation
# -----------------------------------------------------------------------------

qplot_data <- quarterly[
  
  quarterly$year >= 1967 &
    quarterly$year <= 1980,
  
  c(
    "date",
    "B639_log_inflation"
  ),
  
  drop = FALSE
]


png(
  
  file.path(
    figures_dir,
    "foreign_price_quarterly_inflation.png"
  ),
  
  width = 1400,
  
  height = 900,
  
  res = 150
)


plot(
  
  qplot_data$date,
  
  100 *
    qplot_data$B639_log_inflation,
  
  type = "l",
  
  xlab = "Quarter",
  
  ylab = "Quarterly log inflation (%)",
  
  main =
    paste(
      "B639 Industrial-Supplies",
      "Export-Price Inflation"
    )
)


abline(
  h = 0,
  lty = 3
)

dev.off()


# -----------------------------------------------------------------------------
# 13.3 Baseline AR(1) residuals
# -----------------------------------------------------------------------------

png(
  
  file.path(
    figures_dir,
    "rhoF_residuals.png"
  ),
  
  width = 1400,
  
  height = 900,
  
  res = 150
)


plot(
  
  seq_along(
    residuals(
      baseline_model
    )
  ),
  
  residuals(
    baseline_model
  ),
  
  type = "h",
  
  xlab =
    "Regression observation",
  
  ylab =
    "Residual",
  
  main =
    "rhoF Baseline AR(1) Residuals"
)


abline(
  h = 0,
  lty = 2
)

dev.off()


# -----------------------------------------------------------------------------
# 13.4 ACF
# -----------------------------------------------------------------------------

png(
  
  file.path(
    figures_dir,
    "rhoF_inflation_acf.png"
  ),
  
  width = 1200,
  
  height = 900,
  
  res = 150
)


acf(
  
  baseline_x,
  
  main =
    "ACF: B639 Quarterly Foreign-Price Inflation"
)

dev.off()


# -----------------------------------------------------------------------------
# 13.5 PACF
# -----------------------------------------------------------------------------

png(
  
  file.path(
    figures_dir,
    "rhoF_inflation_pacf.png"
  ),
  
  width = 1200,
  
  height = 900,
  
  res = 150
)


pacf(
  
  baseline_x,
  
  main =
    "PACF: B639 Quarterly Foreign-Price Inflation"
)

dev.off()

# =============================================================================
# 14. FINAL VALUES
# =============================================================================

rhoF_final <-
  baseline_row$rhoF

rhoF_se <-
  baseline_row$standard_error

rhoF_hc1 <-
  baseline_row$robust_se_HC1

rhoF_nw <-
  baseline_row$newey_west_se

eps_pf_sd <-
  baseline_row$shock_sd

half_life <-
  baseline_row$half_life_quarters


bootstrap_lower <-
  bootstrap_ci[1]

bootstrap_median <-
  bootstrap_ci[2]

bootstrap_upper <-
  bootstrap_ci[3]

# =============================================================================
# 15. UPDATE PARAMETER REGISTRY
# =============================================================================

registry_path <- file.path(
  docs_dir,
  "parameter_registry.csv"
)


set_if_column_exists <- function(
    df,
    row_id,
    column,
    value
) {
  
  if (
    column %in%
    names(df)
  ) {
    
    df[
      row_id,
      column
    ] <- value
  }
  
  df
}


if (
  file.exists(
    registry_path
  )
) {
  
  registry <- read.csv(
    
    registry_path,
    
    check.names = FALSE,
    
    stringsAsFactors = FALSE
  )
  
  
  row_id <- integer(0)
  
  
  if (
    "dynare_name" %in%
    names(registry)
  ) {
    
    row_id <- which(
      trimws(
        registry$dynare_name
      ) == "rhoF"
    )
  }
  
  
  if (
    length(row_id) == 0 &&
    "parameter" %in%
    names(registry)
  ) {
    
    row_id <- which(
      trimws(
        registry$parameter
      ) == "rhoF"
    )
  }
  
  
  if (
    length(row_id) == 1
  ) {
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "classification",
      "Estimated"
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "research_value",
      format(
        rhoF_final,
        digits = 12
      )
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "status",
      "Estimated - accepted first-stage candidate"
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "method",
      paste(
        "Quarterly AR(1) on B639 log inflation;",
        "Serra intermediate-goods prices used as",
        "primary annual historical validation"
      )
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "sample",
      "1967Q3-1980Q4"
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "raw_or_primary_data",
      paste(
        "B639RG3Q086SBEA quarterly;",
        "Serra (1982) Table 13 annual",
        "intermediate-goods import-price index"
      )
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "source_or_literature",
      "BEA/FRED B639; Serra (1982), Table 13"
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "standard_error",
      format(
        rhoF_se,
        digits = 10
      )
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "shock_sd",
      format(
        eps_pf_sd,
        digits = 10
      )
    )
    
    
    registry <- set_if_column_exists(
      registry,
      row_id,
      "notes",
      paste0(
        "Quarterly estimator retained because Dynare model is quarterly. ",
        "Brazil-specific Serra intermediate-goods annual prices validate ",
        "the proxy but do not replace quarterly rhoF. ",
        "HC1 SE=",
        format(
          rhoF_hc1,
          digits = 5
        ),
        "; Newey-West SE=",
        format(
          rhoF_nw,
          digits = 5
        ),
        "; bootstrap 95% CI=[",
        format(
          bootstrap_lower,
          digits = 5
        ),
        ", ",
        format(
          bootstrap_upper,
          digits = 5
        ),
        "]."
      )
    )
    
    
    write.csv(
      registry,
      registry_path,
      row.names = FALSE,
      na = ""
    )
    
    
    cat(
      "\nUpdated parameter registry row for rhoF.\n"
    )
    
  } else {
    
    warning(
      paste(
        "Parameter registry was found, but exactly one",
        "rhoF row could not be identified.",
        "Registry was not modified."
      )
    )
  }
}

# =============================================================================
# 16. UPDATE DATA DICTIONARY
# =============================================================================

dictionary_path <- file.path(
  docs_dir,
  "data_dictionary.csv"
)


upsert_dictionary_row <- function(
    df,
    key_col,
    key_value,
    values
) {
  
  if (
    !(key_col %in%
      names(df))
  ) {
    return(df)
  }
  
  
  idx <- which(
    as.character(
      df[[key_col]]
    ) ==
      key_value
  )
  
  
  if (
    length(idx) == 0
  ) {
    
    new_row <-
      as.list(
        rep(
          NA_character_,
          ncol(df)
        )
      )
    
    names(new_row) <-
      names(df)
    
    new_row[[key_col]] <-
      key_value
    
    
    for (
      nm in
      names(values)
    ) {
      
      if (
        nm %in%
        names(df)
      ) {
        
        new_row[[nm]] <-
          values[[nm]]
      }
    }
    
    
    df <- rbind(
      df,
      as.data.frame(
        new_row,
        stringsAsFactors = FALSE
      )
    )
    
  } else {
    
    idx <- idx[1]
    
    
    for (
      nm in
      names(values)
    ) {
      
      if (
        nm %in%
        names(df)
      ) {
        
        df[
          idx,
          nm
        ] <- values[[nm]]
      }
    }
  }
  
  
  df
}


if (
  file.exists(
    dictionary_path
  )
) {
  
  dictionary <- read.csv(
    
    dictionary_path,
    
    check.names = FALSE,
    
    stringsAsFactors = FALSE
  )
  
  
  if (
    "dataset_name" %in%
    names(dictionary)
  ) {
    
    dictionary <-
      upsert_dictionary_row(
        
        dictionary,
        
        "dataset_name",
        
        "foreign_price_b639",
        
        list(
          
          description =
            paste(
              "Quarterly U.S. export price index for",
              "industrial supplies and materials;",
              "baseline proxy for model foreign",
              "intermediate-input prices"
            ),
          
          frequency =
            "Quarterly",
          
          sample =
            "1967Q1-1980Q4",
          
          source =
            "BEA/FRED B639RG3Q086SBEA",
          
          transformation =
            "Quarterly log difference of price index"
        )
      )
    
    
    dictionary <-
      upsert_dictionary_row(
        
        dictionary,
        
        "dataset_name",
        
        "foreign_price_b686",
        
        list(
          
          description =
            paste(
              "Quarterly U.S. export price index for",
              "durable industrial supplies and materials;",
              "proxy robustness"
            ),
          
          frequency =
            "Quarterly",
          
          source =
            "BEA/FRED B686RG3Q086SBEA",
          
          transformation =
            "Quarterly log difference of price index"
        )
      )
    
    
    dictionary <-
      upsert_dictionary_row(
        
        dictionary,
        
        "dataset_name",
        
        "foreign_price_b687",
        
        list(
          
          description =
            paste(
              "Quarterly U.S. export price index for",
              "nondurable industrial supplies and materials;",
              "proxy robustness"
            ),
          
          frequency =
            "Quarterly",
          
          source =
            "BEA/FRED B687RG3Q086SBEA",
          
          transformation =
            "Quarterly log difference of price index"
        )
      )
    
    
    dictionary <-
      upsert_dictionary_row(
        
        dictionary,
        
        "dataset_name",
        
        "foreign_price_b640",
        
        list(
          
          description =
            paste(
              "Quarterly U.S. export price index for",
              "capital goods except automotive;",
              "negative/control diagnostic"
            ),
          
          frequency =
            "Quarterly",
          
          source =
            "BEA/FRED B640RG3Q086SBEA",
          
          transformation =
            "Quarterly log difference of price index"
        )
      )
    
    
    dictionary <-
      upsert_dictionary_row(
        
        dictionary,
        
        "dataset_name",
        
        "brazil_import_price_serra",
        
        list(
          
          description =
            paste(
              "Brazil annual import-price indexes",
              "by economic use from Serra (1982), Table 13;",
              "intermediate-goods index is primary rhoF",
              "validation target"
            ),
          
          frequency =
            "Annual",
          
          sample =
            paste(
              "1963-1980 full years used for validation;",
              "1981 Jan-Oct excluded"
            ),
          
          source =
            "Serra (1982), Table 13",
          
          transformation =
            "Annual log difference of published price indexes"
        )
      )
    
    
    write.csv(
      
      dictionary,
      
      dictionary_path,
      
      row.names = FALSE,
      
      na = ""
    )
    
    
    cat(
      "Updated data dictionary entries for rhoF datasets.\n"
    )
    
  } else {
    
    warning(
      paste(
        "data_dictionary.csv found but has no",
        "'dataset_name' column.",
        "Dictionary was not modified."
      )
    )
  }
}

# =============================================================================
# 17. WRITE PARAMETER DOCUMENTATION
# =============================================================================

parameter_doc_path <- file.path(
  docs_dir,
  "parameter_rhoF.md"
)


primary_validation_rows <-
  proxy_validation[
    
    proxy_validation$proxy ==
      "B639" &
      
      proxy_validation$annualization ==
      "avg" &
      
      proxy_validation$brazil_target ==
      "Serra intermediate-goods import price",
    
    ,
    
    drop = FALSE
  ]


primary_validation_text <-
  paste(
    
    apply(
      
      primary_validation_rows,
      
      1,
      
      function(r) {
        
        paste0(
          
          "- ",
          r[["period"]],
          
          ": n=",
          r[["n"]],
          
          ", Pearson=",
          sprintf(
            "%.4f",
            as.numeric(
              r[["pearson_correlation"]]
            )
          ),
          
          ", Spearman=",
          sprintf(
            "%.4f",
            as.numeric(
              r[["spearman_correlation"]]
            )
          )
        )
      }
    ),
    
    collapse = "\n"
  )


parameter_doc <- c(
  
  "# Parameter rhoF",
  
  "",
  
  "## Model equation",
  
  "",
  
  "`pif_t = rhoF * pif_{t-1} + eps_pf,t`",
  
  "",
  
  "## Empirical interpretation",
  
  "",
  
  paste0(
    "The quarterly persistence parameter is estimated from the ",
    "BEA/FRED B639 U.S. export-price index for industrial ",
    "supplies and materials. The observable is quarterly log ",
    "inflation, not the price-index level."
  ),
  
  "",
  
  paste0(
    "Serra (1982), Table 13 provides Brazil-specific annual ",
    "import-price indexes. The intermediate-goods series is ",
    "used as the primary historical validation target because ",
    "it is the closest empirical counterpart to the model's ",
    "imported-intermediate-input price concept."
  ),
  
  "",
  
  paste0(
    "The Brazilian annual series is not used directly to ",
    "estimate the quarterly Dynare rhoF because temporal ",
    "aggregation changes the stochastic process. The annual ",
    "AR coefficient is therefore treated only as a diagnostic."
  ),
  
  "",
  
  "## Baseline estimate",
  
  "",
  
  paste0(
    "- Sample: ",
    baseline_row$sample
  ),
  
  paste0(
    "- rhoF: ",
    sprintf(
      "%.12f",
      rhoF_final
    )
  ),
  
  paste0(
    "- OLS SE: ",
    sprintf(
      "%.8f",
      rhoF_se
    )
  ),
  
  paste0(
    "- HC1 robust SE: ",
    sprintf(
      "%.8f",
      rhoF_hc1
    )
  ),
  
  paste0(
    "- Newey-West SE: ",
    sprintf(
      "%.8f",
      rhoF_nw
    )
  ),
  
  paste0(
    "- eps_pf residual shock SD: ",
    sprintf(
      "%.12f",
      eps_pf_sd
    )
  ),
  
  paste0(
    "- Half-life: ",
    sprintf(
      "%.3f",
      half_life
    ),
    " quarters"
  ),
  
  paste0(
    "- Residual-bootstrap 95% CI: [",
    sprintf(
      "%.4f",
      bootstrap_lower
    ),
    ", ",
    sprintf(
      "%.4f",
      bootstrap_upper
    ),
    "]"
  ),
  
  "",
  
  paste(
    "## Primary historical proxy validation:",
    "B639 vs Serra intermediate goods"
  ),
  
  "",
  
  primary_validation_text,
  
  "",
  
  "## Robustness",
  
  "",
  
  paste0(
    "The script reports alternative samples, durable and ",
    "nondurable industrial-supply proxies, a capital-goods ",
    "diagnostic, an optional broad-export benchmark, ADF and ",
    "KPSS stationarity diagnostics, Ljung-Box residual ",
    "diagnostics, ACF/PACF, AR(1) versus AR(2) AIC/BIC, and ",
    "a residual bootstrap."
  ),
  
  "",
  
  "## Dynare mapping",
  
  "",
  
  paste0(
    "`rhoF = ",
    sprintf(
      "%.12f",
      rhoF_final
    ),
    ";`"
  ),
  
  paste0(
    "`stderr eps_pf = ",
    sprintf(
      "%.12f",
      eps_pf_sd
    ),
    ";`"
  ),
  
  "",
  
  "## Source hierarchy",
  
  "",
  
  paste(
    "1. Quarterly estimator:",
    "BEA/FRED B639 industrial supplies and materials price index."
  ),
  
  paste(
    "2. Primary historical validation:",
    "Serra (1982), Table 13, Brazilian intermediate-goods",
    "import-price index."
  ),
  
  paste(
    "3. Secondary historical validation:",
    "Serra (1982), Table 13, general Brazilian import-price index."
  ),
  
  paste(
    "4. Proxy robustness:",
    "B686 and B687."
  ),
  
  paste(
    "5. Negative/control diagnostic:",
    "B640 capital goods."
  ),
  
  paste(
    "6. Optional broad benchmark:",
    "broad U.S. export-price series."
  ),
  
  ""
)


writeLines(
  parameter_doc,
  parameter_doc_path,
  useBytes = TRUE
)

# =============================================================================
# 18. UPDATE SOURCES.MD
# =============================================================================

sources_path <- file.path(
  docs_dir,
  "sources.md"
)


rho_sources_block <- c(
  
  "<!-- BEGIN RHO_F SOURCES -->",
  
  "## rhoF — foreign intermediate-input price persistence",
  
  "",
  
  paste(
    "- **Quarterly estimator:**",
    "BEA/FRED B639RG3Q086SBEA,",
    "U.S. exports of industrial supplies and materials,",
    "chain-type price index."
  ),
  
  paste(
    "- **Proxy robustness:**",
    "BEA/FRED B686RG3Q086SBEA and B687RG3Q086SBEA."
  ),
  
  paste(
    "- **Control diagnostic:**",
    "BEA/FRED B640RG3Q086SBEA,",
    "capital goods except automotive."
  ),
  
  paste(
    "- **Primary Brazilian historical validation:**",
    "Serra, José (1982), Table 13, annual Brazilian",
    "import-price index for intermediate goods."
  ),
  
  paste(
    "- **Secondary Brazilian validation:**",
    "Serra (1982), Table 13, general import-price index."
  ),
  
  paste(
    "- Serra's table reports 1963=100 indexes and identifies",
    "Conjuntura Econômica and Mendonça de Barros (1981) as",
    "underlying sources; 1981 is Jan-Oct and is excluded",
    "from full-year validation."
  ),
  
  "",
  
  paste(
    "Methodological rule: Serra annual data validate the",
    "quarterly B639 proxy but do not replace the quarterly",
    "rhoF estimator."
  ),
  
  "<!-- END RHO_F SOURCES -->"
)


if (
  file.exists(
    sources_path
  )
) {
  
  old_sources <- readLines(
    sources_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  
  begin_idx <- grep(
    "<!-- BEGIN RHO_F SOURCES -->",
    old_sources,
    fixed = TRUE
  )
  
  
  end_idx <- grep(
    "<!-- END RHO_F SOURCES -->",
    old_sources,
    fixed = TRUE
  )
  
  
  if (
    length(begin_idx) == 1 &&
    length(end_idx) == 1 &&
    end_idx > begin_idx
  ) {
    
    before <-
      if (
        begin_idx > 1
      ) {
        old_sources[
          1:(begin_idx - 1)
        ]
      } else {
        character(0)
      }
    
    
    after <-
      if (
        end_idx <
        length(old_sources)
      ) {
        old_sources[
          (end_idx + 1):
            length(old_sources)
        ]
      } else {
        character(0)
      }
    
    
    new_sources <- c(
      before,
      rho_sources_block,
      after
    )
    
  } else {
    
    new_sources <- c(
      old_sources,
      "",
      rho_sources_block
    )
  }
  
  
  writeLines(
    new_sources,
    sources_path,
    useBytes = TRUE
  )
}

# =============================================================================
# 19. CONSOLE REPORT
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "FOREIGN-PRICE PROCESS ESTIMATION COMPLETE\n"
)

cat(
  "============================================================\n\n"
)


cat(
  "Baseline quarterly proxy: B639RG3Q086SBEA\n"
)


cat(
  paste(
    "Primary Brazilian validation:",
    "Serra (1982) intermediate-goods import prices\n"
  )
)


cat(
  "Baseline sample: ",
  baseline_row$sample,
  "\n\n",
  sep = ""
)


cat(
  "rhoF = ",
  sprintf(
    "%.12f",
    rhoF_final
  ),
  "\n",
  sep = ""
)


cat(
  "OLS SE = ",
  sprintf(
    "%.8f",
    rhoF_se
  ),
  "\n",
  sep = ""
)


cat(
  "HC1 robust SE = ",
  sprintf(
    "%.8f",
    rhoF_hc1
  ),
  "\n",
  sep = ""
)


cat(
  "Newey-West SE = ",
  sprintf(
    "%.8f",
    rhoF_nw
  ),
  "\n",
  sep = ""
)


cat(
  "eps_pf shock SD = ",
  sprintf(
    "%.12f",
    eps_pf_sd
  ),
  "\n",
  sep = ""
)


cat(
  "Half-life = ",
  sprintf(
    "%.3f",
    half_life
  ),
  " quarters\n",
  sep = ""
)


cat(
  "Bootstrap 95% CI = [",
  sprintf(
    "%.4f",
    bootstrap_lower
  ),
  ", ",
  sprintf(
    "%.4f",
    bootstrap_upper
  ),
  "]\n\n",
  sep = ""
)


cat(
  "Robustness results:\n\n"
)


print(
  
  rhoF_ar1[
    ,
    c(
      "proxy",
      "role",
      "sample",
      "n",
      "rhoF",
      "standard_error",
      "robust_se_HC1",
      "newey_west_se",
      "shock_sd",
      "recommended_baseline"
    )
  ],
  
  row.names = FALSE
)


cat(
  "\nStationarity diagnostics:\n\n"
)


print(
  stationarity_tests,
  row.names = FALSE
)


cat(
  "\nAR model comparison:\n\n"
)


print(
  ar_model_comparison,
  row.names = FALSE
)


cat(
  "\nAnnual persistence diagnostics (NOT Dynare rhoF):\n\n"
)


print(
  annual_persistence,
  row.names = FALSE
)


cat(
  "\nDynare candidate values:\n\n"
)


cat(
  "rhoF = ",
  sprintf(
    "%.12f",
    rhoF_final
  ),
  ";\n",
  sep = ""
)


cat(
  "stderr eps_pf = ",
  sprintf(
    "%.12f",
    eps_pf_sd
  ),
  ";\n",
  sep = ""
)


cat(
  "\nFiles created/updated:\n"
)


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
  " - docs/parameter_registry.csv (if compatible row exists)\n"
)

cat(
  " - docs/data_dictionary.csv (if compatible schema exists)\n"
)

cat(
  " - docs/parameter_rhoF.md\n"
)

cat(
  " - docs/sources.md (if present)\n"
)

cat(
  " - output/figures/foreign_price_proxy_annual_validation.png\n"
)

cat(
  " - output/figures/foreign_price_quarterly_inflation.png\n"
)

cat(
  " - output/figures/rhoF_residuals.png\n"
)

cat(
  " - output/figures/rhoF_inflation_acf.png\n"
)

cat(
  " - output/figures/rhoF_inflation_pacf.png\n"
)


cat(
  paste0(
    "\nIMPORTANT: Serra's annual AR(1) is diagnostic only. ",
    "The quarterly B639 AR(1)\n",
    "remains the estimator used for the model's quarterly rhoF.\n"
  )
)
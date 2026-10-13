# =============================================================================
# 04i_steady_state_fiscal_shares.R
#
# Brazil DSGE project
#
# CALIBRATION:
#   g_y  = G^C / Y
#   ig_y = I^G / Y
#
# Historical baseline:
#   1968-1973
#
# Interpretation:
#   Post-PAEG / Brazilian Miracle baseline immediately before the
#   first oil-shock regime.
#
# Data source:
#   data/raw/fiscal_raw_1947_1980.csv
#
# The raw fiscal staging file was constructed from:
#   IV.11 - Government consumption
#   IV.14 - Government gross fixed capital formation
#
# Nominal GDP was reconstructed consistently from:
#   IV.1  - nominal global tax revenue
#   IV.2  - global tax burden as % of GDP
#
# IMPORTANT:
#
# These parameters are STEADY-STATE SHARES, not AR(1) coefficients.
#
# We therefore estimate them as historical sample means:
#
#       g_y  = mean(G^C / Y)
#       ig_y = mean(I^G / Y)
#
# The historical data are stored in percent of GDP, so they are divided
# by 100 before being written as Dynare/model parameters.
#
# This script:
#
#   1. reads the fiscal dataset;
#   2. validates it;
#   3. selects 1968-1973;
#   4. calculates g_y and ig_y;
#   5. calculates descriptive statistics;
#   6. calculates a 1968-1972 robustness check;
#   7. writes parameter output files;
#   8. writes a Dynare-ready parameter block;
#   9. updates docs/parameter_registry.csv;
#  10. preserves ALL baseline_value entries;
#  11. leaves cH_y, x_y, cf_y and z_y untouched;
#  12. runs final integrity tests.
#
# =============================================================================


local({
  
  
  # =============================================================================
  # 0. GENERAL SETTINGS
  # =============================================================================
  
  options(
    stringsAsFactors = FALSE,
    scipen = 999
  )
  
  
  cat("\n")
  cat("============================================================\n")
  cat("STEADY-STATE FISCAL SHARE CALIBRATION\n")
  cat("g_y and ig_y\n")
  cat("============================================================\n\n")
  
  
  # =============================================================================
  # 1. FILE PATHS
  # =============================================================================
  
  input_candidates <- c(
    
    "data/raw/fiscal_raw_1947_1980.csv",
    
    "data/processed/fiscal_raw_1947_1980.csv",
    
    "fiscal_raw_1947_1980.csv"
    
  )
  
  
  existing_input <- input_candidates[
    file.exists(
      input_candidates
    )
  ]
  
  
  if (
    length(
      existing_input
    ) == 0
  ) {
    
    stop(
      paste0(
        "\nCould not find fiscal_raw_1947_1980.csv.\n\n",
        "Expected location:\n",
        "data/raw/fiscal_raw_1947_1980.csv\n"
      ),
      call. = FALSE
    )
    
  }
  
  
  input_path <- existing_input[1]
  
  
  registry_path <-
    "docs/parameter_registry.csv"
  
  
  output_dir <-
    "output/estimates"
  
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  cat(
    "Fiscal data:\n",
    input_path,
    "\n\n"
  )
  
  
  # =============================================================================
  # 2. VERIFY PARAMETER REGISTRY EXISTS
  # =============================================================================
  
  if (
    !file.exists(
      registry_path
    )
  ) {
    
    stop(
      paste(
        "Parameter registry not found:",
        registry_path
      ),
      call. = FALSE
    )
    
  }
  
  
  # =============================================================================
  # 3. LOAD FISCAL DATA
  # =============================================================================
  
  fiscal <- read.csv(
    
    input_path,
    
    stringsAsFactors = FALSE,
    
    check.names = FALSE
    
  )
  
  
  required_columns <- c(
    
    "year",
    
    "government_consumption_pct_gdp",
    
    "government_investment_pct_gdp"
    
  )
  
  
  missing_columns <- setdiff(
    
    required_columns,
    
    names(
      fiscal
    )
    
  )
  
  
  if (
    length(
      missing_columns
    ) > 0
  ) {
    
    stop(
      paste(
        "Missing required fiscal columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
    
  }
  
  
  # =============================================================================
  # 4. BASIC DATA VALIDATION
  # =============================================================================
  
  fiscal <- fiscal[
    order(
      fiscal$year
    ),
  ]
  
  
  row.names(
    fiscal
  ) <- NULL
  
  
  if (
    anyDuplicated(
      fiscal$year
    )
  ) {
    
    stop(
      "Duplicate years detected in fiscal dataset.",
      call. = FALSE
    )
    
  }
  
  
  if (
    anyNA(
      fiscal[
        ,
        required_columns
      ]
    )
  ) {
    
    stop(
      "Missing values detected in required fiscal series.",
      call. = FALSE
    )
    
  }
  
  
  if (
    min(
      fiscal$year
    ) > 1968 ||
    max(
      fiscal$year
    ) < 1973
  ) {
    
    stop(
      "Fiscal dataset does not cover the required 1968-1973 baseline.",
      call. = FALSE
    )
    
  }
  
  
  cat(
    "Data coverage:",
    min(
      fiscal$year
    ),
    "to",
    max(
      fiscal$year
    ),
    "\n"
  )
  
  
  cat(
    "Observations:",
    nrow(
      fiscal
    ),
    "\n\n"
  )
  
  
  # =============================================================================
  # 5. DEFINE BASELINE SAMPLE
  #
  # Selected historical window:
  #
  # 1968-1973
  #
  # This represents the post-PAEG / Miracle-era fiscal structure before the
  # economy fully enters the first-oil-shock / II PND regime.
  # =============================================================================
  
  baseline_start <- 1968
  
  baseline_end <- 1973
  
  
  baseline <- fiscal[
    fiscal$year >= baseline_start &
      fiscal$year <= baseline_end,
  ]
  
  
  expected_years <- baseline_start:baseline_end
  
  
  if (
    !identical(
      baseline$year,
      expected_years
    )
  ) {
    
    stop(
      paste0(
        "Baseline sample is incomplete.\n",
        "Expected exactly: 1968, 1969, 1970, 1971, 1972, 1973."
      ),
      call. = FALSE
    )
    
  }
  
  
  if (
    nrow(
      baseline
    ) != 6
  ) {
    
    stop(
      "Expected exactly six annual observations for 1968-1973.",
      call. = FALSE
    )
    
  }
  
  
  cat(
    "Selected baseline:",
    baseline_start,
    "-",
    baseline_end,
    "\n"
  )
  
  
  cat(
    "Baseline observations:",
    nrow(
      baseline
    ),
    "\n\n"
  )
  
  
  # =============================================================================
  # 6. CALCULATE THE STEADY-STATE SHARES
  #
  # Fiscal file stores percentage points of GDP.
  #
  # Example:
  #
  #   10.8 percent of GDP
  #
  # must enter the model as:
  #
  #   0.108
  #
  # Therefore:
  #
  #   parameter = pct_gdp / 100
  # =============================================================================
  
  
  # -----------------------------------------------------------------------------
  # Government consumption share
  # -----------------------------------------------------------------------------
  
  g_y_pct <- mean(
    
    baseline$government_consumption_pct_gdp
    
  )
  
  
  g_y <- g_y_pct / 100
  
  
  # -----------------------------------------------------------------------------
  # Government investment share
  # -----------------------------------------------------------------------------
  
  ig_y_pct <- mean(
    
    baseline$government_investment_pct_gdp
    
  )
  
  
  ig_y <- ig_y_pct / 100
  
  
  # =============================================================================
  # 7. DESCRIPTIVE STATISTICS
  # =============================================================================
  
  g_sd_pct <- sd(
    
    baseline$government_consumption_pct_gdp
    
  )
  
  
  ig_sd_pct <- sd(
    
    baseline$government_investment_pct_gdp
    
  )
  
  
  g_mean_se_pct <-
    
    g_sd_pct /
    sqrt(
      nrow(
        baseline
      )
    )
  
  
  ig_mean_se_pct <-
    
    ig_sd_pct /
    sqrt(
      nrow(
        baseline
      )
    )
  
  
  # =============================================================================
  # 8. BUILD MAIN PARAMETER OUTPUT TABLE
  # =============================================================================
  
  parameters <- data.frame(
    
    dynare_name = c(
      "g_y",
      "ig_y"
    ),
    
    symbol = c(
      "G^C/Y",
      "I^G/Y"
    ),
    
    estimate = c(
      g_y,
      ig_y
    ),
    
    mean_percent_gdp = c(
      g_y_pct,
      ig_y_pct
    ),
    
    sample_sd_percent_gdp = c(
      g_sd_pct,
      ig_sd_pct
    ),
    
    mean_se_percent_gdp = c(
      g_mean_se_pct,
      ig_mean_se_pct
    ),
    
    minimum_percent_gdp = c(
      
      min(
        baseline$government_consumption_pct_gdp
      ),
      
      min(
        baseline$government_investment_pct_gdp
      )
      
    ),
    
    maximum_percent_gdp = c(
      
      max(
        baseline$government_consumption_pct_gdp
      ),
      
      max(
        baseline$government_investment_pct_gdp
      )
      
    ),
    
    n = c(
      nrow(
        baseline
      ),
      nrow(
        baseline
      )
    ),
    
    sample = c(
      "1968-1973",
      "1968-1973"
    ),
    
    method = c(
      
      paste0(
        "Arithmetic mean of annual government consumption / GDP; ",
        "1968-1973; percentage-of-GDP series converted to decimal share"
      ),
      
      paste0(
        "Arithmetic mean of annual government investment / GDP; ",
        "1968-1973; percentage-of-GDP series converted to decimal share"
      )
      
    ),
    
    stringsAsFactors = FALSE
    
  )
  
  
  # =============================================================================
  # 9. SAVE BASELINE OBSERVATIONS USED
  # =============================================================================
  
  baseline_output <- data.frame(
    
    year =
      baseline$year,
    
    government_consumption_pct_gdp =
      baseline$government_consumption_pct_gdp,
    
    government_consumption_share_gdp =
      baseline$government_consumption_pct_gdp / 100,
    
    government_investment_pct_gdp =
      baseline$government_investment_pct_gdp,
    
    government_investment_share_gdp =
      baseline$government_investment_pct_gdp / 100
    
  )
  
  
  write.csv(
    
    baseline_output,
    
    file.path(
      output_dir,
      "steady_state_fiscal_share_sample.csv"
    ),
    
    row.names = FALSE
    
  )
  
  
  # =============================================================================
  # 10. SAVE MAIN PARAMETER TABLE
  # =============================================================================
  
  write.csv(
    
    parameters,
    
    file.path(
      output_dir,
      "steady_state_fiscal_share_parameters.csv"
    ),
    
    row.names = FALSE
    
  )
  
  
  # =============================================================================
  # 11. ROBUSTNESS CHECK: 1968-1972
  #
  # 1973 is retained in the preferred calibration because that was our agreed
  # historical baseline window.
  #
  # Since the first oil shock begins late in 1973, we also report a strict
  # pre-1973 robustness mean.
  #
  # This DOES NOT replace the preferred calibration.
  # =============================================================================
  
  robustness <- fiscal[
    fiscal$year >= 1968 &
      fiscal$year <= 1972,
  ]
  
  
  g_y_1968_1972 <-
    
    mean(
      robustness$government_consumption_pct_gdp
    ) / 100
  
  
  ig_y_1968_1972 <-
    
    mean(
      robustness$government_investment_pct_gdp
    ) / 100
  
  
  robustness_table <- data.frame(
    
    dynare_name = c(
      "g_y",
      "ig_y"
    ),
    
    preferred_1968_1973 = c(
      g_y,
      ig_y
    ),
    
    robustness_1968_1972 = c(
      g_y_1968_1972,
      ig_y_1968_1972
    ),
    
    difference = c(
      
      g_y_1968_1972 - g_y,
      
      ig_y_1968_1972 - ig_y
      
    ),
    
    stringsAsFactors = FALSE
    
  )
  
  
  write.csv(
    
    robustness_table,
    
    file.path(
      output_dir,
      "steady_state_fiscal_share_robustness.csv"
    ),
    
    row.names = FALSE
    
  )
  
  
  # =============================================================================
  # 12. WRITE DYNARE-READY PARAMETER BLOCK
  # =============================================================================
  
  dynare_lines <- c(
    
    "// ============================================================",
    
    "// Fiscal steady-state shares",
    
    "// Historical baseline: Brazil, 1968-1973",
    
    "// ============================================================",
    
    "",
    
    sprintf(
      "g_y  = %.10f;",
      g_y
    ),
    
    sprintf(
      "ig_y = %.10f;",
      ig_y
    ),
    
    "",
    
    "// Definitions:",
    
    "// g_y  = government consumption / GDP",
    
    "// ig_y = government gross fixed capital formation / GDP",
    
    "",
    
    "// These are decimal shares of GDP, not percentages."
    
  )
  
  
  writeLines(
    
    dynare_lines,
    
    file.path(
      output_dir,
      "steady_state_fiscal_shares_dynare.txt"
    )
    
  )
  
  
  # =============================================================================
  # 13. LOAD PARAMETER REGISTRY
  # =============================================================================
  
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
    
    names(
      registry
    )
    
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
      ),
      call. = FALSE
    )
    
  }
  
  
  # =============================================================================
  # 14. SAVE REGISTRY STRUCTURE BEFORE UPDATE
  #
  # We use these objects later to guarantee:
  #
  # - no row was added;
  # - no row was deleted;
  # - no parameter was reordered;
  # - no baseline_value was altered.
  # =============================================================================
  
  registry_names_before <-
    registry$dynare_name
  
  
  registry_n_before <-
    nrow(
      registry
    )
  
  
  baseline_before <-
    registry$baseline_value
  
  
  names(
    baseline_before
  ) <- registry$dynare_name
  
  
  # =============================================================================
  # 15. VERIFY TARGET PARAMETERS EXIST EXACTLY ONCE
  # =============================================================================
  
  target_parameters <- c(
    "g_y",
    "ig_y"
  )
  
  
  for (
    parameter_name in target_parameters
  ) {
    
    parameter_rows <- which(
      registry$dynare_name ==
        parameter_name
    )
    
    
    if (
      length(
        parameter_rows
      ) != 1
    ) {
      
      stop(
        paste(
          "Expected exactly one registry row for",
          parameter_name,
          "but found",
          length(
            parameter_rows
          )
        ),
        call. = FALSE
      )
      
    }
    
  }
  
  
  # =============================================================================
  # 16. UPDATE g_y
  # =============================================================================
  
  g_row <- which(
    registry$dynare_name ==
      "g_y"
  )
  
  
  registry$research_value[
    g_row
  ] <- g_y
  
  
  registry$status[
    g_row
  ] <- "FIRST-STAGE CALIBRATION COMPLETE"
  
  
  registry$method[
    g_row
  ] <- paste0(
    
    "Historical steady-state calibration: arithmetic mean of annual ",
    
    "government consumption / nominal GDP over 1968-1973; ",
    
    "percentage-of-GDP observations converted to decimal share."
    
  )
  
  
  registry$sample[
    g_row
  ] <- "1968-1973"
  
  
  registry$raw_or_primary_data[
    g_row
  ] <- "data/raw/fiscal_raw_1947_1980.csv"
  
  
  registry$source_or_literature[
    g_row
  ] <- paste0(
    
    "Brazil historical public-sector accounts: ",
    
    "IV.11 government consumption; ",
    
    "GDP denominator reconstructed consistently from IV.1 and IV.2"
    
  )
  
  
  registry$standard_error[
    g_row
  ] <- NA_real_
  
  
  registry$shock_sd[
    g_row
  ] <- NA_real_
  
  
  registry$notes[
    g_row
  ] <- paste0(
    
    "Preferred research calibration uses the 1968-1973 post-PAEG / ",
    
    "pre-first-oil-shock baseline. ",
    
    "1968-1972 mean is saved as a robustness check. ",
    
    "This is a steady-state share calibration, not an AR(1) estimate."
    
  )
  
  
  # =============================================================================
  # 17. UPDATE ig_y
  # =============================================================================
  
  ig_row <- which(
    registry$dynare_name ==
      "ig_y"
  )
  
  
  registry$research_value[
    ig_row
  ] <- ig_y
  
  
  registry$status[
    ig_row
  ] <- "FIRST-STAGE CALIBRATION COMPLETE"
  
  
  registry$method[
    ig_row
  ] <- paste0(
    
    "Historical steady-state calibration: arithmetic mean of annual ",
    
    "government gross fixed capital formation / nominal GDP over 1968-1973; ",
    
    "percentage-of-GDP observations converted to decimal share."
    
  )
  
  
  registry$sample[
    ig_row
  ] <- "1968-1973"
  
  
  registry$raw_or_primary_data[
    ig_row
  ] <- "data/raw/fiscal_raw_1947_1980.csv"
  
  
  registry$source_or_literature[
    ig_row
  ] <- paste0(
    
    "Brazil historical public-sector accounts: ",
    
    "IV.14 government gross fixed capital formation; ",
    
    "GDP denominator reconstructed consistently from IV.1 and IV.2"
    
  )
  
  
  registry$standard_error[
    ig_row
  ] <- NA_real_
  
  
  registry$shock_sd[
    ig_row
  ] <- NA_real_
  
  
  registry$notes[
    ig_row
  ] <- paste0(
    
    "Preferred research calibration uses the 1968-1973 post-PAEG / ",
    
    "pre-first-oil-shock baseline. ",
    
    "1968-1972 mean is saved as a robustness check. ",
    
    "This is a steady-state share calibration, not an AR(1) estimate."
    
  )
  
  
  # =============================================================================
  # 18. REGISTRY SAFETY TESTS BEFORE WRITING
  # =============================================================================
  
  if (
    nrow(
      registry
    ) != registry_n_before
  ) {
    
    stop(
      "Registry row count changed unexpectedly.",
      call. = FALSE
    )
    
  }
  
  
  if (
    !identical(
      registry$dynare_name,
      registry_names_before
    )
  ) {
    
    stop(
      "Registry parameter order or names changed unexpectedly.",
      call. = FALSE
    )
    
  }
  
  
  baseline_after <-
    registry$baseline_value
  
  
  names(
    baseline_after
  ) <- registry$dynare_name
  
  
  if (
    !isTRUE(
      all.equal(
        
        baseline_before,
        
        baseline_after,
        
        check.attributes = FALSE
        
      )
    )
  ) {
    
    stop(
      "A baseline_value entry changed. Registry update aborted.",
      call. = FALSE
    )
    
  }
  
  
  if (
    anyDuplicated(
      registry$dynare_name
    )
  ) {
    
    stop(
      "Duplicate Dynare parameter names detected.",
      call. = FALSE
    )
    
  }
  
  
  # =============================================================================
  # 19. WRITE UPDATED PARAMETER REGISTRY
  # =============================================================================
  
  write.csv(
    
    registry,
    
    registry_path,
    
    row.names = FALSE,
    
    na = ""
    
  )
  
  
  # =============================================================================
  # 20. RELOAD REGISTRY AND VERIFY THE SAVED VALUES
  # =============================================================================
  
  registry_check <- read.csv(
    
    registry_path,
    
    stringsAsFactors = FALSE,
    
    check.names = FALSE
    
  )
  
  
  g_check <- registry_check[
    registry_check$dynare_name ==
      "g_y",
  ]
  
  
  ig_check <- registry_check[
    registry_check$dynare_name ==
      "ig_y",
  ]
  
  
  same_number <- function(
    x,
    y,
    tolerance = 1e-12
  ) {
    
    isTRUE(
      all.equal(
        
        as.numeric(
          x
        ),
        
        as.numeric(
          y
        ),
        
        tolerance = tolerance,
        
        check.attributes = FALSE
        
      )
    )
    
  }
  
  
  if (
    !same_number(
      g_check$research_value,
      g_y
    )
  ) {
    
    stop(
      "Registry g_y does not match calculated g_y.",
      call. = FALSE
    )
    
  }
  
  
  if (
    !same_number(
      ig_check$research_value,
      ig_y
    )
  ) {
    
    stop(
      "Registry ig_y does not match calculated ig_y.",
      call. = FALSE
    )
    
  }
  
  
  # =============================================================================
  # 21. VERIFY PARAMETERS WITHOUT DATA WERE NOT TOUCHED
  # =============================================================================
  
  not_yet_estimated <- c(
    "cH_y",
    "x_y",
    "cf_y",
    "z_y"
  )
  
  
  for (
    parameter_name in not_yet_estimated
  ) {
    
    if (
      parameter_name %in%
      registry$dynare_name
    ) {
      
      before_row <- which(
        registry_names_before ==
          parameter_name
      )
      
      after_row <- which(
        registry_check$dynare_name ==
          parameter_name
      )
      
      
      # Baseline must certainly remain unchanged.
      
      if (
        !same_number(
          registry_check$baseline_value[
            after_row
          ],
          baseline_before[
            parameter_name
          ]
        )
      ) {
        
        stop(
          paste(
            "Unexpected baseline modification for",
            parameter_name
          ),
          call. = FALSE
        )
        
      }
      
    }
    
  }
  
  
  # =============================================================================
  # 22. EXPECTED OUTPUT FILE TESTS
  # =============================================================================
  
  expected_output_files <- c(
    
    file.path(
      output_dir,
      "steady_state_fiscal_share_sample.csv"
    ),
    
    file.path(
      output_dir,
      "steady_state_fiscal_share_parameters.csv"
    ),
    
    file.path(
      output_dir,
      "steady_state_fiscal_share_robustness.csv"
    ),
    
    file.path(
      output_dir,
      "steady_state_fiscal_shares_dynare.txt"
    ),
    
    registry_path
    
  )
  
  
  for (
    file_name in expected_output_files
  ) {
    
    if (
      !file.exists(
        file_name
      )
    ) {
      
      stop(
        paste(
          "Expected output file was not created:",
          file_name
        ),
        call. = FALSE
      )
      
    }
    
    
    if (
      file.info(
        file_name
      )$size <= 0
    ) {
      
      stop(
        paste(
          "Expected output file is empty:",
          file_name
        ),
        call. = FALSE
      )
      
    }
    
  }
  
  
  # =============================================================================
  # 23. PRINT RESULTS
  # =============================================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("PREFERRED CALIBRATION RESULTS\n")
  cat("============================================================\n\n")
  
  
  cat(
    "g_y  =",
    format(
      g_y,
      digits = 12
    ),
    "\n"
  )
  
  
  cat(
    "      =",
    format(
      g_y_pct,
      digits = 10
    ),
    "% of GDP\n\n"
  )
  
  
  cat(
    "ig_y =",
    format(
      ig_y,
      digits = 12
    ),
    "\n"
  )
  
  
  cat(
    "      =",
    format(
      ig_y_pct,
      digits = 10
    ),
    "% of GDP\n\n"
  )
  
  
  cat(
    "Sample:",
    baseline_start,
    "-",
    baseline_end,
    "\n\n"
  )
  
  
  # =============================================================================
  # 24. PRINT ROBUSTNESS RESULTS
  # =============================================================================
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "ROBUSTNESS: STRICT 1968-1972 WINDOW\n"
  )
  
  cat(
    "============================================================\n\n"
  )
  
  
  cat(
    "g_y  1968-1972 =",
    format(
      g_y_1968_1972,
      digits = 12
    ),
    "\n"
  )
  
  
  cat(
    "ig_y 1968-1972 =",
    format(
      ig_y_1968_1972,
      digits = 12
    ),
    "\n\n"
  )
  
  
  # =============================================================================
  # 25. PRINT REGISTRY ROWS
  # =============================================================================
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "UPDATED PARAMETER REGISTRY\n"
  )
  
  cat(
    "============================================================\n\n"
  )
  
  
  print(
    
    registry_check[
      registry_check$dynare_name %in%
        c(
          "g_y",
          "ig_y"
        ),
      c(
        "dynare_name",
        "symbol",
        "classification",
        "baseline_value",
        "research_value",
        "status",
        "sample"
      )
    ],
    
    row.names = FALSE,
    
    digits = 12
    
  )
  
  
  # =============================================================================
  # 26. FINAL REPORT
  # =============================================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("FILES CREATED / UPDATED\n")
  cat("============================================================\n\n")
  
  
  cat(
    "1. output/estimates/steady_state_fiscal_share_sample.csv\n"
  )
  
  
  cat(
    "2. output/estimates/steady_state_fiscal_share_parameters.csv\n"
  )
  
  
  cat(
    "3. output/estimates/steady_state_fiscal_share_robustness.csv\n"
  )
  
  
  cat(
    "4. output/estimates/steady_state_fiscal_shares_dynare.txt\n"
  )
  
  
  cat(
    "5. docs/parameter_registry.csv\n\n"
  )
  
  
  cat(
    "Parameters deliberately NOT modified:\n"
  )
  
  
  cat(
    "cH_y, x_y, cf_y, z_y\n\n"
  )
  
  
  cat(
    "============================================================\n"
  )
  
  
  cat(
    "FISCAL STEADY-STATE SHARE CALIBRATION COMPLETE\n"
  )
  
  
  cat(
    "============================================================\n\n"
  )
  
  
})
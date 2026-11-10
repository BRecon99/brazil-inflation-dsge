# =============================================================================
# run_all.R
# =============================================================================
#
# Run the complete validated parameter/calibration pipeline in dependency order.
#
# IMPORTANT ARCHITECTURE
# ----------------------
# Script 99 MUST run last because it is the sole writer of:
#
#   docs/parameter_registry.csv
#
# Expected local corrected scripts:
#   03_brazil_inflation_steady_state.R
#   04_fiscal_processes.R
#   04i_steady_state_fiscal_shares.R
#   05_foreign_price_process.R
#   06_trade_steady_state_shares.R
#   07_alpha_mu_lambda_s.R
#   008_build_external_debt_database.R
#   08_external_debt_and_financing_wedge_parameters.R
#   99_update_parameter_registry.R
#
# Script 00 is not explicitly run here because Scripts 05/06 already rebuild
# their required Script-00 processed databases if they are absent.
# =============================================================================

rm(list = ls())

options(stringsAsFactors = FALSE)

# =============================================================================
# 0. PIPELINE ORDER
# =============================================================================

scripts <- c(
  "R/01_world_interest_rate.R",
  "R/02_foreign_output.R",
  "R/03_brazil_inflation_steady_state.R",
  "R/04_fiscal_processes.R",
  "R/04i_steady_state_fiscal_shares.R",
  "R/05_foreign_price_process.R",
  "R/06_trade_steady_state_shares.R",
  "R/07_alpha_mu_lambda_s.R",
  "R/008_build_external_debt_database.R",
  "R/08_external_debt_and_financing_wedge_parameters.R",
  "R/99_update_parameter_registry.R"
)

missing_scripts <- scripts[
  !file.exists(
    scripts
  )
]

if (length(missing_scripts) > 0) {
  stop(
    "Missing pipeline script(s):\n - ",
    paste(
      missing_scripts,
      collapse = "\n - "
    )
  )
}

required_static_files <- c(
  file.path(
    "data",
    "raw",
    "miebach_marquetti_2022_labor_share_1967_1972_v1.csv"
  )
)

missing_static_files <- required_static_files[
  !file.exists(
    required_static_files
  )
]

if (length(missing_static_files) > 0) {
  stop(
    "Missing required versioned raw input(s):\n - ",
    paste(
      missing_static_files,
      collapse = "\n - "
    )
  )
}

# =============================================================================
# 1. SMALL CHECKPOINT HELPERS
# =============================================================================

read_project_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

check_after_script <- function(script) {
  
  # ---------------------------------------------------------------------------
  # Script 03
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/03_brazil_inflation_steady_state.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "Pi_ss_calibration.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 03 did not create ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    row <- d[
      d$sample == "1968Q1-1972Q4",
      ,
      drop = FALSE
    ]
    
    if (nrow(row) != 1) {
      stop(
        "Script 03 checkpoint: baseline 1968Q1-1972Q4 not found uniquely."
      )
    }
    
    if (
      abs(
        as.numeric(row$Pi_ss) -
        1.048995251092
      ) > 1e-10
    ) {
      stop(
        "Script 03 checkpoint: Pi_ss differs from accepted value."
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 04
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/04_fiscal_processes.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "fiscal_persistence_parameters.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 04 did not create ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    expected <- c(
      "rhotau",
      "rhoG",
      "rhoIG"
    )
    
    if (
      !setequal(
        trimws(
          as.character(
            d$dynare_name
          )
        ),
        expected
      )
    ) {
      stop(
        paste(
          "Script 04 checkpoint failed.",
          "Expected canonical names rhotau, rhoG, rhoIG only."
        )
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 04i
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/04i_steady_state_fiscal_shares.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "steady_state_fiscal_share_parameters.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 04i did not create ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    if (
      !all(
        trimws(
          as.character(
            d$sample
          )
        ) == "1968-1972"
      )
    ) {
      stop(
        paste(
          "Script 04i checkpoint failed:",
          "the corrected baseline must be 1968-1972."
        )
      )
    }
    
    g <- d$estimate[
      d$dynare_name == "g_y"
    ]
    
    ig <- d$estimate[
      d$dynare_name == "ig_y"
    ]
    
    if (
      length(g) != 1 ||
      abs(
        as.numeric(g) -
        0.110279410789871
      ) > 1e-12
    ) {
      stop(
        "Script 04i checkpoint failed for g_y."
      )
    }
    
    if (
      length(ig) != 1 ||
      abs(
        as.numeric(ig) -
        0.044702876856991
      ) > 1e-12
    ) {
      stop(
        "Script 04i checkpoint failed for ig_y."
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 05
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/05_foreign_price_process.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "foreign_price_process_parameters.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 05 did not create compact rhoF parameter output: ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    rhoF <- d$estimate[
      d$dynare_name == "rhoF"
    ]
    
    if (
      length(rhoF) != 1 ||
      abs(
        as.numeric(rhoF) -
        0.723969948147
      ) > 1e-12
    ) {
      stop(
        "Script 05 checkpoint failed for rhoF."
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 06
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/06_trade_steady_state_shares.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "trade_steady_state_share_parameters.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 06 did not create ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    cH <- d$value[
      d$parameter == "cH_y"
    ]
    
    if (
      length(cH) != 1 ||
      abs(
        as.numeric(cH) -
        0.7781620666
      ) > 1e-8
    ) {
      stop(
        "Script 06 checkpoint failed: cH_y is not approximately 0.7781620666."
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 07
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/07_alpha_mu_lambda_s.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "alpha_mu_lambda_s_parameters.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 07 did not create ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    values <- setNames(
      as.numeric(d$value),
      d$parameter
    )
    
    expected <- c(
      alpha = 0.01513018,
      mu = 0.06827873,
      lambda_s = 0.08237584
    )
    
    for (p in names(expected)) {
      if (
        !(p %in% names(values)) ||
        abs(
          values[p] -
          expected[p]
        ) > 1e-7
      ) {
        stop(
          "Script 07 checkpoint failed for ",
          p,
          "."
        )
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 008
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/008_build_external_debt_database.R"
    )
  ) {
    
    f <- file.path(
      "data",
      "processed",
      "brazil_external_debt_master_1964_1980.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 008 did not create ",
        f
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Script 08
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      script,
      "R/08_external_debt_and_financing_wedge_parameters.R"
    )
  ) {
    
    f <- file.path(
      "output",
      "estimates",
      "external_financing_final_parameter_candidates.csv"
    )
    
    if (!file.exists(f)) {
      stop(
        "Script 08 did not create ",
        f
      )
    }
    
    d <- read_project_csv(f)
    
    if (
      "phiDD" %in%
      trimws(
        as.character(
          d$parameter
        )
      )
    ) {
      stop(
        "Script 08 checkpoint failed: phiDD is still a live parameter."
      )
    }
    
    if (
      !setequal(
        trimws(
          as.character(
            d$parameter
          )
        ),
        c(
          "dbar",
          "phiD"
        )
      )
    ) {
      stop(
        "Script 08 checkpoint failed: final live parameter table must contain only dbar and phiD."
      )
    }
  }
  
  invisible(
    TRUE
  )
}

# =============================================================================
# 2. RUN PIPELINE
# =============================================================================

for (script in scripts) {
  
  cat(
    "\n\n============================================================\n"
  )
  
  cat(
    "RUNNING: ",
    script,
    "\n",
    sep = ""
  )
  
  cat(
    "============================================================\n\n"
  )
  
  script_environment <- new.env(
    parent = globalenv()
  )
  
  sys.source(
    script,
    envir = script_environment
  )
  
  rm(
    script_environment
  )
  
  check_after_script(
    script
  )
  
  cat(
    "\nCHECKPOINT PASSED: ",
    script,
    "\n",
    sep = ""
  )
}

# =============================================================================
# 3. FINAL REPORT
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "FULL PARAMETER PIPELINE COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
)

cat(
  "Script 99 ran last and consolidated the final parameter registry.\n"
)

cat(
  "Final registry: docs/parameter_registry.csv\n"
)

cat(
  "Registry audit: output/estimates/parameter_registry_update_audit.csv\n"
)

cat(
  "============================================================\n"
)

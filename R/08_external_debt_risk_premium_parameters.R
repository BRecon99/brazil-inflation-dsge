# =============================================================================
# 08_external_debt_and_financing_wedge_parameters.R
# =============================================================================
#
# PURPOSE
# -------
# Finalize the parsimonious external-debt / external-financing block for
# Paper I while preserving the same structural architecture for Papers II-III.
#
# STRUCTURAL BASELINE
# -------------------
#
#   rp_t = phiD * d_t + eps_rp,t
#
# We DROP phiDD from the baseline model.
#
# Parameter strategy:
#
#   dbar  -> Brazil-specific historical calibration
#   phiD  -> small positive quarterly SOE literature calibration
#   phiDD -> zero / remove from final model
#   rp_t  -> historically disciplined EXTERNAL FINANCING WEDGE
#
#
# PAPER-I OBSERVABLE
# ------------------
#
# Brazil's syndicated loans were priced approximately as:
#
#   Brazil marginal external rate
#     = LIBOR + Brazil contractual spread over LIBOR
#
# The model's world rate istar is already mapped to TB3MS.
#
# Therefore the model-consistent observed financing wedge is:
#
#   rp_obs
#     = (LIBOR - TB3MS)
#       + Brazil spread over LIBOR
#
# so that:
#
#   TB3MS + rp_obs
#     = LIBOR + Brazil spread.
#
#
# IMPORTANT
# ---------
#
# This script DOES NOT estimate phiD from the 1970s Brazilian data.
#
# Why?
#
#   1) the usable marginal-spread sample is short;
#   2) debt and spreads are jointly determined;
#   3) global credit supply strongly affected Brazil's borrowing conditions;
#   4) the economic role of phiD in this model is primarily stationarity and
#      long-run debt discipline;
#   5) historically important movements in financing conditions are allowed to
#      enter through eps_rp,t.
#
#
# BASELINE phiD
# -------------
#
# We use:
#
#   phiD = 0.001
#
# at quarterly frequency.
#
# This is a small debt-elastic risk-premium calibration used directly in
# quarterly small-open-economy work and described as comparable to the
# Schmitt-Grohe & Uribe (2003) debt-elastic-premium calibration.
#
# Schmitt-Grohe & Uribe use:
#
#   p(d) = psi2 * ( exp(d-dbar) - 1 )
#
# whose first-order approximation around dbar is:
#
#   p(d) ~= psi2 * (d-dbar).
#
# Thus our linear rule has the same local form.
#
# We intentionally use phiD as a small structural/stationarity parameter, not
# as an attempt to explain the level of historical Brazilian sovereign spreads.
#
#
# ROBUSTNESS
# ----------
#
# Following the wide sensitivity exercise in Schmitt-Grohe & Uribe, save:
#
#   low  = 0.0001
#   base = 0.0010
#   high = 0.0100
#
#
# OUTPUT
# ------
#
#   dbar
#   phiD
#   phiDD = 0 cleanup flag
#   Paper-I annual marginal financing wedge, 1974-1980
#   1972 approximate pre-shock centering anchor
#   implied annual historical eps_rp observations
#   Fishlow/Sachs robustness comparison
#
# No false quarterly interpolation is created here.
#
# =============================================================================


options(stringsAsFactors = FALSE)


# =============================================================================
# 0. DIRECTORIES AND FILES
# =============================================================================

processed_dir <- file.path("data", "processed")
raw_debt_dir  <- file.path("data", "raw", "historical_external_debt")
raw_fred_dir  <- file.path("data", "raw", "fred")
output_dir    <- file.path("output", "estimates")
figure_dir    <- file.path("output", "figures")
docs_dir      <- "docs"

for (d in c(processed_dir, raw_debt_dir, raw_fred_dir, output_dir, figure_dir, docs_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


master_file <- file.path(
  processed_dir,
  "brazil_external_debt_master_1964_1980.csv"
)

bacha_file <- file.path(
  raw_debt_dir,
  "bacha_diaz_alejandro_1982_table4_brazil_libor_spreads_1974_1980.csv"
)

sachs_file <- file.path(
  raw_debt_dir,
  "sachs_fishlow_brazil_spread_over_libor_1974_1980.csv"
)

anchor_file <- file.path(
  raw_debt_dir,
  "paper1_pre_shock_marginal_rp_anchor_1972.csv"
)

aiolfi_file <- file.path(
  raw_debt_dir,
  "aiolfi_catao_timmermann_brazil_effective_external_cost_spread.csv"
)


required_files <- c(
  master_file,
  bacha_file,
  sachs_file,
  anchor_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files)) {
  stop(
    paste(
      "Required Script 008 output file(s) are missing:",
      paste(missing_files, collapse = "; "),
      "\nRun the revised Script 008 first."
    )
  )
}


# =============================================================================
# 1. HELPER FUNCTIONS
# =============================================================================

effective_quarterly <- function(r_annual_decimal) {
  
  out <- rep(NA_real_, length(r_annual_decimal))
  
  ok <- is.finite(r_annual_decimal) &
    (1 + r_annual_decimal > 0)
  
  out[ok] <- (1 + r_annual_decimal[ok])^(1 / 4) - 1
  
  out
}


get_by_year <- function(df, value_col, years) {
  
  idx <- match(years, df$year)
  
  out <- rep(NA_real_, length(years))
  
  good <- !is.na(idx)
  
  out[good] <- df[idx[good], value_col]
  
  out
}


# =============================================================================
# 2. LOAD DEBT DATABASE AND CONSTRUCT NET DEBT/GDP
# =============================================================================

debt <- read.csv(
  master_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


required_cols <- c(
  "year",
  "serra_gross_external_debt_usd_million",
  "serra_international_reserves_usd_million",
  "serra_net_external_debt_usd_million",
  "bresser1983_external_debt_to_gdp_pct"
)

missing_cols <- setdiff(required_cols, names(debt))

if (length(missing_cols)) {
  stop(
    "Master debt database is missing required column(s): ",
    paste(missing_cols, collapse = ", ")
  )
}


debt$gross_debt_ratio <-
  debt$bresser1983_external_debt_to_gdp_pct / 100


debt$net_to_gross_debt_ratio <-
  debt$serra_net_external_debt_usd_million /
  debt$serra_gross_external_debt_usd_million


debt$net_debt_ratio <-
  debt$gross_debt_ratio *
  debt$net_to_gross_debt_ratio


# =============================================================================
# 3. CALIBRATE dbar
# =============================================================================
#
# Baseline:
#
#   mean net external debt / GDP, 1970-1972
#
# This is a pre-first-oil-shock steady-state calibration.
#
# Gross debt/GDP is retained as robustness.
#

dbar_sample <- debt[
  debt$year >= 1970 &
    debt$year <= 1972,
  ,
  drop = FALSE
]


if (sum(is.finite(dbar_sample$net_debt_ratio)) != 3) {
  stop("Expected three complete net debt/GDP observations for 1970-1972.")
}


dbar <- mean(
  dbar_sample$net_debt_ratio
)


dbar_net_median <- median(
  dbar_sample$net_debt_ratio
)


dbar_net_1972 <- dbar_sample$net_debt_ratio[
  dbar_sample$year == 1972
]


dbar_gross_mean <- mean(
  dbar_sample$gross_debt_ratio
)


dbar_gross_median <- median(
  dbar_sample$gross_debt_ratio
)


dbar_gross_1972 <- dbar_sample$gross_debt_ratio[
  dbar_sample$year == 1972
]


dbar_candidates <- data.frame(
  
  parameter = "dbar",
  
  specification = c(
    "BASELINE mean net external debt/GDP 1970-1972",
    "Net debt/GDP median 1970-1972",
    "Net debt/GDP 1972 endpoint",
    "Gross external debt/GDP mean 1970-1972",
    "Gross external debt/GDP median 1970-1972",
    "Gross external debt/GDP 1972 endpoint"
  ),
  
  value = c(
    dbar,
    dbar_net_median,
    dbar_net_1972,
    dbar_gross_mean,
    dbar_gross_median,
    dbar_gross_1972
  ),
  
  role = c(
    "baseline",
    "robustness",
    "historical_initial_condition",
    "gross_debt_robustness",
    "gross_debt_robustness",
    "historical_initial_condition"
  ),
  
  stringsAsFactors = FALSE
)


debt$d_gap_net <-
  debt$net_debt_ratio -
  dbar


# =============================================================================
# 4. CALIBRATE phiD; DROP phiDD
# =============================================================================
#
# Structural baseline:
#
#   rp_t = phiD * d_t + eps_rp,t
#
# Baseline quarterly phiD:
#
#   0.001
#
# This is intentionally a small stationarity / long-run debt-discipline
# parameter, not a Brazil-specific causal spread estimate.
#

phiD <- 0.001

phiD_low <- 0.0001

phiD_high <- 0.0100

phiDD <- 0


phiD_calibration <- data.frame(
  
  parameter = c(
    "phiD_low",
    "phiD",
    "phiD_high",
    "phiDD"
  ),
  
  value = c(
    phiD_low,
    phiD,
    phiD_high,
    phiDD
  ),
  
  status = c(
    "robustness",
    "BASELINE quarterly literature calibration",
    "robustness",
    "removed_from_baseline_model"
  ),
  
  source_or_reason = c(
    "one-tenth baseline sensitivity",
    "quarterly SOE debt-elastic risk-premium calibration comparable to Schmitt-Grohe-Uribe",
    "ten-times baseline sensitivity",
    "dropped because the Brazilian contemporaneous estimate is timing/endogeneity sensitive and lacks a clean structural interpretation"
  ),
  
  stringsAsFactors = FALSE
)


# =============================================================================
# 5. LOAD PAPER-I MARGINAL LOAN DATA
# =============================================================================

bacha <- read.csv(
  bacha_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


sachs <- read.csv(
  sachs_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


anchor <- read.csv(
  anchor_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# =============================================================================
# 6. LOAD / DOWNLOAD TB3MS
# =============================================================================
#
# The model world-rate block was already disciplined using TB3MS.
#
# We preserve that choice.
#

fred_local_file <- file.path(
  raw_fred_dir,
  "TB3MS_1964_1980.csv"
)


fred_download_url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?",
  "id=TB3MS&cosd=1964-01-01&coed=1980-12-31"
)


if (!file.exists(fred_local_file)) {
  
  message(
    "Local TB3MS raw file not found. Downloading from FRED..."
  )
  
  tryCatch(
    utils::download.file(
      fred_download_url,
      fred_local_file,
      method = "libcurl",
      mode = "wb",
      quiet = TRUE
    ),
    error = function(e) {
      stop(
        "Could not download TB3MS and no local file is available. ",
        conditionMessage(e)
      )
    }
  )
}


tb3ms_raw <- read.csv(
  fred_local_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


date_candidates <- intersect(
  names(tb3ms_raw),
  c(
    "DATE",
    "observation_date",
    "date"
  )
)


if (length(date_candidates) != 1) {
  stop("Could not identify the TB3MS date column uniquely.")
}


if (!("TB3MS" %in% names(tb3ms_raw))) {
  stop("TB3MS value column not found.")
}


tb3ms_raw$date <- as.Date(
  tb3ms_raw[[date_candidates[1]]]
)


tb3ms_raw$tb3ms_pct <- suppressWarnings(
  as.numeric(tb3ms_raw$TB3MS)
)


tb3ms_raw$year <- as.integer(
  format(tb3ms_raw$date, "%Y")
)


tb3ms_annual <- aggregate(
  tb3ms_pct ~ year,
  data = tb3ms_raw[is.finite(tb3ms_raw$tb3ms_pct), , drop = FALSE],
  FUN = mean
)


names(tb3ms_annual)[names(tb3ms_annual) == "tb3ms_pct"] <-
  "tb3ms_annual_pct"


tb3ms_annual$tb3ms_annual_decimal <-
  tb3ms_annual$tb3ms_annual_pct / 100


tb3ms_annual$tb3ms_quarterly_effective <-
  effective_quarterly(
    tb3ms_annual$tb3ms_annual_decimal
  )


# =============================================================================
# 7. PAPER-I BASELINE MARGINAL FINANCING WEDGE
# =============================================================================
#
# Historical marginal Brazil rate:
#
#   r_BR = LIBOR + Brazil spread
#
# Model-consistent wedge over TB3MS:
#
#   rp_obs = r_BR - TB3MS
#
# At quarterly frequency we transform BOTH gross annual rates separately:
#
#   r_BR,q  = (1+r_BR,a)^(1/4)-1
#   i*,q    = (1+i*,a)^(1/4)-1
#
#   rp_obs,q = r_BR,q - i*,q
#

paper1_rp <- merge(
  bacha,
  tb3ms_annual,
  by = "year",
  all.x = TRUE,
  sort = TRUE
)


paper1_rp$brazil_marginal_rate_annual_decimal <-
  (
    paper1_rp$libor_annual_pct +
      paper1_rp$brazil_spread_over_libor_pp
  ) / 100


paper1_rp$libor_annual_decimal <-
  paper1_rp$libor_annual_pct / 100


paper1_rp$brazil_spread_over_libor_decimal <-
  paper1_rp$brazil_spread_over_libor_pp / 100


paper1_rp$global_bank_wedge_over_tb3ms_annual_pp <-
  paper1_rp$libor_annual_pct -
  paper1_rp$tb3ms_annual_pct


paper1_rp$rp_model_annual_pp <-
  paper1_rp$libor_annual_pct +
  paper1_rp$brazil_spread_over_libor_pp -
  paper1_rp$tb3ms_annual_pct


paper1_rp$brazil_marginal_rate_quarterly_effective <-
  effective_quarterly(
    paper1_rp$brazil_marginal_rate_annual_decimal
  )


paper1_rp$rp_model_quarterly <-
  paper1_rp$brazil_marginal_rate_quarterly_effective -
  paper1_rp$tb3ms_quarterly_effective


paper1_rp$rp_model_quarterly_bp <-
  10000 *
  paper1_rp$rp_model_quarterly


paper1_rp$d_gap_net <-
  get_by_year(
    debt,
    "d_gap_net",
    paper1_rp$year
  )


paper1_rp$structural_debt_premium_quarterly <-
  phiD *
  paper1_rp$d_gap_net


paper1_rp$structural_debt_premium_quarterly_bp <-
  10000 *
  paper1_rp$structural_debt_premium_quarterly


# =============================================================================
# 8. 1972 PRE-SHOCK CENTERING ANCHOR
# =============================================================================
#
# The 1972 spread observation is approximate / mid-year.
#
# We use it only to define a transparent pre-shock normalization:
#
#   rp_gap_t = rp_obs_t - rp_anchor_1972
#
# It is NOT treated as a high-precision annual estimate.
#

anchor$tb3ms_annual_pct <-
  get_by_year(
    tb3ms_annual,
    "tb3ms_annual_pct",
    anchor$year
  )


anchor$tb3ms_annual_decimal <-
  anchor$tb3ms_annual_pct / 100


anchor$brazil_marginal_rate_annual_decimal <-
  (
    anchor$libor_annual_pct +
      anchor$brazil_spread_over_libor_pp
  ) / 100


anchor$brazil_marginal_rate_quarterly_effective <-
  effective_quarterly(
    anchor$brazil_marginal_rate_annual_decimal
  )


anchor$tb3ms_quarterly_effective <-
  effective_quarterly(
    anchor$tb3ms_annual_decimal
  )


anchor$rp_anchor_quarterly <-
  anchor$brazil_marginal_rate_quarterly_effective -
  anchor$tb3ms_quarterly_effective


anchor$rp_anchor_quarterly_bp <-
  10000 *
  anchor$rp_anchor_quarterly


rp_anchor_q <- anchor$rp_anchor_quarterly[1]


# =============================================================================
# 9. IMPLIED PAPER-I HISTORICAL eps_rp
# =============================================================================
#
# The model equation is expressed around steady state:
#
#   rp_gap_t = phiD * d_gap_t + eps_rp,t
#
# Therefore:
#
#   eps_rp,t = rp_gap_t - phiD*d_gap_t
#
# These are ANNUAL historical observations expressed in quarterly rate units.
#
# DO NOT repeat each value four times and pretend that creates quarterly data.
#

paper1_rp$rp_gap_from_1972_anchor_quarterly <-
  paper1_rp$rp_model_quarterly -
  rp_anchor_q


paper1_rp$rp_gap_from_1972_anchor_quarterly_bp <-
  10000 *
  paper1_rp$rp_gap_from_1972_anchor_quarterly


paper1_rp$eps_rp_implied_quarterly_rate_units <-
  paper1_rp$rp_gap_from_1972_anchor_quarterly -
  paper1_rp$structural_debt_premium_quarterly


paper1_rp$eps_rp_implied_bp <-
  10000 *
  paper1_rp$eps_rp_implied_quarterly_rate_units


# =============================================================================
# 10. FISHLOW / SACHS ROBUSTNESS
# =============================================================================
#
# We keep the Bacha-Diaz-Alejandro LIBOR annual series and substitute only the
# alternative Brazil contractual spread.
#
# This isolates sensitivity to the country-spread source.
#

rp_robust <- merge(
  bacha[
    ,
    c(
      "year",
      "libor_annual_pct"
    )
  ],
  sachs[
    ,
    c(
      "year",
      "brazil_spread_over_libor_pp",
      "status"
    )
  ],
  by = "year",
  all = FALSE,
  sort = TRUE,
  suffixes = c(
    "_bacha",
    "_sachs"
  )
)


names(rp_robust)[names(rp_robust) == "brazil_spread_over_libor_pp"] <-
  "sachs_brazil_spread_over_libor_pp"


rp_robust <- merge(
  rp_robust,
  tb3ms_annual[
    ,
    c(
      "year",
      "tb3ms_annual_pct",
      "tb3ms_quarterly_effective"
    )
  ],
  by = "year",
  all.x = TRUE,
  sort = TRUE
)


rp_robust$bacha_spread_pp <-
  get_by_year(
    bacha,
    "brazil_spread_over_libor_pp",
    rp_robust$year
  )


rp_robust$bacha_brazil_rate_annual <-
  (
    rp_robust$libor_annual_pct +
      rp_robust$bacha_spread_pp
  ) / 100


rp_robust$sachs_brazil_rate_annual <-
  (
    rp_robust$libor_annual_pct +
      rp_robust$sachs_brazil_spread_over_libor_pp
  ) / 100


rp_robust$bacha_rp_q <-
  effective_quarterly(
    rp_robust$bacha_brazil_rate_annual
  ) -
  rp_robust$tb3ms_quarterly_effective


rp_robust$sachs_rp_q <-
  effective_quarterly(
    rp_robust$sachs_brazil_rate_annual
  ) -
  rp_robust$tb3ms_quarterly_effective


rp_robust$bacha_rp_bp <-
  10000 *
  rp_robust$bacha_rp_q


rp_robust$sachs_rp_bp <-
  10000 *
  rp_robust$sachs_rp_q


rp_robust$difference_bp_sachs_minus_bacha <-
  rp_robust$sachs_rp_bp -
  rp_robust$bacha_rp_bp


# =============================================================================
# 11. AIOLFI / CARDOSO-FISHLOW EFFECTIVE-COST VALIDATION
# =============================================================================
#
# Retain this published series as a separate concept:
#
#   effective inherited-debt cost spread
#
# NOT:
#
#   marginal syndicated-loan spread.
#

if (file.exists(aiolfi_file)) {
  
  aiolfi <- read.csv(
    aiolfi_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  aiolfi_compare <- merge(
    paper1_rp[
      ,
      c(
        "year",
        "rp_model_annual_pp",
        "rp_model_quarterly_bp"
      )
    ],
    aiolfi[
      ,
      c(
        "year",
        "effective_external_cost_spread_pp"
      )
    ],
    by = "year",
    all.x = TRUE,
    sort = TRUE
  )
  
} else {
  
  aiolfi_compare <- data.frame()
}


# =============================================================================
# 12. phiD SENSITIVITY IN ECONOMIC UNITS
# =============================================================================
#
# Show how small the structural stationarity premium is.
#
# Debt gaps below are expressed as shares of GDP:
#
#   0.01 = 1 percentage point of GDP
#   0.05 = 5 percentage points
#   0.10 = 10 percentage points
#

debt_gap_examples <- c(
  0.01,
  0.05,
  0.10
)


phiD_sensitivity <- expand.grid(
  debt_gap = debt_gap_examples,
  phiD = c(
    phiD_low,
    phiD,
    phiD_high
  )
)


phiD_sensitivity$quarterly_premium_decimal <-
  phiD_sensitivity$phiD *
  phiD_sensitivity$debt_gap


phiD_sensitivity$quarterly_premium_bp <-
  10000 *
  phiD_sensitivity$quarterly_premium_decimal


# =============================================================================
# 13. FINAL PARAMETER TABLE
# =============================================================================

external_financing_parameters <- data.frame(
  
  parameter = c(
    "dbar",
    "phiD",
    "phiDD"
  ),
  
  value = c(
    dbar,
    phiD,
    phiDD
  ),
  
  parameter_type = c(
    "Brazil-specific steady-state calibration",
    "Quarterly literature calibration",
    "Removed from baseline"
  ),
  
  baseline_status = c(
    "ACCEPT",
    "ACCEPT",
    "DROP"
  ),
  
  note = c(
    "Mean net external debt/GDP, 1970-1972",
    "Small positive debt-elastic premium used for stationarity and long-run debt discipline",
    "Do not structurally estimate from sparse 1970s data; remove from Dynare and registry during cleanup"
  ),
  
  stringsAsFactors = FALSE
)


# =============================================================================
# 14. SAVE OUTPUTS
# =============================================================================

write.csv(
  dbar_candidates,
  file.path(
    output_dir,
    "external_debt_dbar_candidates_final.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  phiD_calibration,
  file.path(
    output_dir,
    "external_financing_phiD_literature_calibration.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  paper1_rp,
  file.path(
    processed_dir,
    "paper1_marginal_external_financing_wedge_1974_1980.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  anchor,
  file.path(
    processed_dir,
    "paper1_marginal_external_financing_wedge_1972_anchor.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  rp_robust,
  file.path(
    output_dir,
    "paper1_marginal_rp_source_robustness.csv"
  ),
  row.names = FALSE,
  na = ""
)


if (nrow(aiolfi_compare)) {
  
  write.csv(
    aiolfi_compare,
    file.path(
      output_dir,
      "paper1_marginal_vs_effective_cost_spread_comparison.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}


write.csv(
  phiD_sensitivity,
  file.path(
    output_dir,
    "phiD_stationarity_sensitivity.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  external_financing_parameters,
  file.path(
    output_dir,
    "external_financing_final_parameter_candidates.csv"
  ),
  row.names = FALSE,
  na = ""
)


# =============================================================================
# 15. DYNARE CLEANUP / CANDIDATE FILE
# =============================================================================
#
# Temporary compatibility:
#
# Until the model is cleaned, phiDD may still exist as a declared parameter.
# If needed to run the old file before cleanup, set phiDD = 0.
#
# FINAL cleanup:
#
#   remove phiDD from
#     - parameter declaration
#     - parameter assignment
#     - rp equation
#     - parameter registry
#     - scripts / documentation
#
# and change:
#
#   rp = phiD*d + phiDD*(d-d(-1)) + eps_rp;
#
# to:
#
#   rp = phiD*d + eps_rp;
#

dynare_lines <- c(
  
  "// ================================================================",
  "// External debt / financing wedge - final baseline candidates",
  "// ================================================================",
  "",
  
  sprintf(
    "dbar = %.12f; // mean net external debt/GDP, 1970-1972",
    dbar
  ),
  
  sprintf(
    "phiD = %.12f; // quarterly SOE literature calibration; stationarity role",
    phiD
  ),
  
  "",
  
  "// TEMPORARY ONLY if old model still declares phiDD:",
  "phiDD = 0;",
  
  "",
  
  "// FINAL risk-premium equation after cleanup:",
  "// rp = phiD*d + eps_rp;",
  
  "",
  
  "// IMPORTANT:",
  "// Historical financing variation is carried primarily by eps_rp,",
  "// disciplined by observed marginal external-financing wedges."
)


writeLines(
  dynare_lines,
  file.path(
    output_dir,
    "external_financing_dynare_cleanup_candidates.txt"
  )
)


# =============================================================================
# 16. METHODOLOGY NOTE
# =============================================================================

method_note <- c(
  
  "# External debt and external-financing wedge: final Paper-I methodology",
  
  "",
  
  "## Structural equation",
  
  "rp_t = phiD*d_t + eps_rp,t",
  
  "",
  
  "phiDD is dropped.",
  
  "",
  
  "## dbar",
  
  paste0(
    "dbar is calibrated as mean net external debt/GDP over 1970-1972. ",
    "Gross debt/GDP is retained as robustness."
  ),
  
  "",
  
  "## phiD",
  
  paste0(
    "phiD is calibrated to 0.001 at quarterly frequency. ",
    "Its role is stationarity and long-run debt discipline. ",
    "It is not estimated from the short Brazilian 1970s spread sample."
  ),
  
  "",
  
  "## Paper-I historical rp",
  
  paste0(
    "The historical marginal financing wedge is constructed as ",
    "(LIBOR - TB3MS) + Brazil's contractual spread over LIBOR. ",
    "This ensures TB3MS + rp equals the historical marginal syndicated-loan rate."
  ),
  
  "",
  
  "## 1972 anchor",
  
  paste0(
    "The 1972 spread is an approximate mid-year observation and is used only ",
    "as a pre-shock normalization / sensitivity anchor."
  ),
  
  "",
  
  "## eps_rp",
  
  paste0(
    "Observed financing-wedge movements relative to the 1972 anchor are ",
    "decomposed into the small structural debt component phiD*d_gap and an ",
    "implied exogenous financing-wedge component eps_rp. The annual history ",
    "is not falsely expanded into quarterly observations in this script."
  ),
  
  "",
  
  "## Cross-paper continuity",
  
  paste0(
    "The equation remains unchanged in Papers II and III. ",
    "Only the historical observable used to discipline eps_rp changes with ",
    "Brazil's financing regime: bank-loan margins, debt-crisis bond/secondary ",
    "market evidence, then EMBI/Brady/Eurobond spreads."
  )
)


writeLines(
  method_note,
  file.path(
    docs_dir,
    "parameter_external_financing_wedge_final.md"
  )
)


# =============================================================================
# 17. FIGURES
# =============================================================================

png(
  file.path(
    figure_dir,
    "paper1_marginal_external_financing_wedge_1974_1980.png"
  ),
  width = 1000,
  height = 650
)

plot(
  paper1_rp$year,
  paper1_rp$rp_model_quarterly_bp,
  type = "o",
  xlab = "Year",
  ylab = "Quarterly-equivalent basis points",
  main = "Brazil marginal external-financing wedge over TB3MS"
)

abline(
  h = anchor$rp_anchor_quarterly_bp[1],
  lty = 2
)

dev.off()


png(
  file.path(
    figure_dir,
    "paper1_external_financing_decomposition_1974_1980.png"
  ),
  width = 1000,
  height = 650
)

plot(
  paper1_rp$year,
  paper1_rp$global_bank_wedge_over_tb3ms_annual_pp,
  type = "o",
  xlab = "Year",
  ylab = "Percentage points, annual",
  main = "External-financing wedge decomposition"
)

lines(
  paper1_rp$year,
  paper1_rp$brazil_spread_over_libor_pp,
  type = "o"
)

legend(
  "topleft",
  legend = c(
    "LIBOR - TB3MS",
    "Brazil spread over LIBOR"
  ),
  lty = 1,
  pch = 1,
  bty = "n"
)

dev.off()


# =============================================================================
# 18. CONSOLE REPORT
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "SCRIPT 08: FINAL EXTERNAL DEBT / FINANCING-WEDGE CALIBRATION\n"
)

cat(
  "============================================================\n"
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "FINAL STRUCTURAL PARAMETERS\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  external_financing_parameters,
  row.names = FALSE
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "dbar ROBUSTNESS\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  dbar_candidates,
  row.names = FALSE
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "phiD LITERATURE CALIBRATION / SENSITIVITY\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  phiD_calibration,
  row.names = FALSE
)


cat(
  "\nEconomic-size check:\n"
)

print(
  phiD_sensitivity,
  row.names = FALSE
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "1972 PRE-SHOCK MARGINAL FINANCING-WEDGE ANCHOR\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  anchor,
  row.names = FALSE
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "PAPER-I MARGINAL FINANCING WEDGE, 1974-1980\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  paper1_rp[
    ,
    c(
      "year",
      "libor_annual_pct",
      "brazil_spread_over_libor_pp",
      "tb3ms_annual_pct",
      "global_bank_wedge_over_tb3ms_annual_pp",
      "rp_model_annual_pp",
      "rp_model_quarterly_bp",
      "d_gap_net",
      "structural_debt_premium_quarterly_bp",
      "rp_gap_from_1972_anchor_quarterly_bp",
      "eps_rp_implied_bp"
    )
  ],
  row.names = FALSE
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "FISHLOW / SACHS ROBUSTNESS\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  rp_robust,
  row.names = FALSE
)


if (nrow(aiolfi_compare)) {
  
  cat(
    "\n------------------------------------------------------------\n"
  )
  
  cat(
    "MARGINAL WEDGE VS AIOLFI EFFECTIVE-COST SPREAD\n"
  )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  print(
    aiolfi_compare,
    row.names = FALSE
  )
}


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "MODEL CLEANUP REQUIRED AFTER PARAMETER STAGE\n"
)

cat(
  "------------------------------------------------------------\n"
)

cat(
  paste(
    "1) Change rp equation to: rp = phiD*d + eps_rp;",
    "2) Remove phiDD from Dynare declarations and assignments;",
    "3) Remove phiDD from the parameter registry and documentation;",
    "4) Keep the 1970s bespoke phiD/phiDD regressions as archived diagnostics, not baseline estimation;",
    "5) During the final registry cleanup also fix the previously identified duplicate fiscal aliases",
    "   rhotau/rho_tau, rhoG/rho_g, rhoIG/rho_ig and retain Dynare names;",
    "6) Verify g_y and ig_y baseline rows use 1968-1972, with 1968-1973 only as robustness.",
    sep = "\n"
  )
)

cat(
  "\n\nIMPORTANT:\n"
)

cat(
  paste(
    "Do not create artificial quarterly eps_rp observations by repeating these annual values.",
    "Quarterly historical-shock timing will be handled later when the Paper-I historical simulation is constructed.",
    "\n"
  )
)

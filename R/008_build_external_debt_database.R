# =============================================================================
# 008_build_external_debt_database.R
# =============================================================================
#
# PURPOSE
# -------
# Build a reproducible historical database for the Brazil external-debt block
# BEFORE estimating/calibrating any DSGE parameter.
#
# This script does NOT estimate:
#   dbar
#   phiD
#   a causal country-risk equation
#   a debt equation
#
# It creates:
#   1) separate raw-source tables preserving each historical concept;
#   2) a source registry with definitions, tables, units, URLs, and notes;
#   3) an official BCB SGS 3560 registered-external-debt series;
#   4) cross-source comparison tables;
#   5) one processed annual research panel for 1964-1980, without silently
#      averaging conflicting debt concepts;
#   6) a cross-era external-financing measurement architecture for Papers I-III;
#   7) Paper-I marginal syndicated-loan spread data and later-period source maps;
#   8) a reading/citation list for the later paper-writing stage.
#
# PRINCIPLE
# ---------
# We preserve source disagreement instead of forcing a single "correct" debt
# series. Different papers use different debt concepts (registered debt,
# gross debt, net debt, broader total debt including short-term items, etc.).
#
# The next script (08) will decide which empirical concept maps best to:
#
#   d_t = consolidated public-sector external debt / net external liability
#
# =============================================================================

options(stringsAsFactors = FALSE)

# =============================================================================
# 0. PACKAGES + DIRECTORIES
# =============================================================================

needed <- c("jsonlite")

missing_pkgs <- needed[
  !vapply(
    needed,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_pkgs)) {
  stop(
    "Missing package(s): ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstall with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
    "))"
  )
}

raw_root       <- file.path("data", "raw")
raw_debt_dir   <- file.path(raw_root, "historical_external_debt")
processed_dir  <- file.path("data", "processed")
docs_dir       <- "docs"
output_dir     <- file.path("output", "estimates")

for (d in c(raw_debt_dir, processed_dir, docs_dir, output_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# =============================================================================
# 1. SOURCE REGISTRY
# =============================================================================

source_registry <- data.frame(
  
  source_id = c(
    "BCB_SGS_3560",
    "SERRA_1982_TABLE7",
    "BRESSER_1983_TABLE4",
    "BRESSER_1983_TABLE5",
    "BRESSER_1987_TABLE2",
    "BATISTA_1987_II_PND",
    "CARDOSO_FISHLOW_1989",
    "BARBOSA_SANCHEZ_1992_TABLE2",
    "WORLD_BANK_BRAZIL_1964_90"
  ),
  
  author = c(
    "Banco Central do Brasil",
    "José Serra",
    "Luiz Carlos Bresser-Pereira",
    "Luiz Carlos Bresser-Pereira",
    "Luiz Carlos Bresser-Pereira",
    "Jorge Chami Batista",
    "Eliana Cardoso; Albert Fishlow",
    "Fernando de Holanda Barbosa; Manuel Sánchez de la Cal",
    "World Bank"
  ),
  
  year = c(
    NA, 1982, 1983, 1983, 1987, 1987, 1989, 1992, NA
  ),
  
  title = c(
    "Registered external debt - SGS 3560",
    "Ciclos e mudanças estruturais na economia brasileira do após-guerra",
    "Auge e declínio nos anos setenta",
    "Auge e declínio nos anos setenta",
    "Mudanças no padrão de financiamento do investimento no Brasil",
    "A estratégia de ajustamento externo do Segundo Plano Nacional de Desenvolvimento",
    "The Macroeconomics of the Brazilian External Debt",
    "Crescimento econômico e renegociação da dívida externa",
    "Macroeconomic crises, policies, and growth in Brazil, 1964-90"
  ),
  
  table_or_section = c(
    "SGS series 3560",
    "Table 7",
    "Table 4",
    "Table 5",
    "Table 2",
    "II PND / growth-cum-debt discussion",
    "Chapter 4; debt strategy of the 1970s and appendices",
    "Table 2; Section II-III",
    "Chapter on external borrowing; Appendix interest-cost discussion"
  ),
  
  role = c(
    "backbone",
    "validation",
    "validation",
    "validation",
    "alternative",
    "mechanism",
    "mechanism;risk_input",
    "mechanism;alternative",
    "risk_input;mechanism"
  ),
  
  primary_variables = c(
    "registered external debt",
    "gross debt; reserves; net debt; current account; interest; debt service; FDI",
    "exports; imports; trade balance; services; interest; current account; gross debt; reserves",
    "debt/GDP; net debt/exports; terms of trade; import coefficients",
    "broader external debt; debt increase; interest; real resource transfer",
    "II PND timing; growth-cum-debt strategy; external adjustment",
    "net debt; external debt dynamics; foreign borrowing costs",
    "Resolution 63; Law 4131; commercial-bank external borrowing; floating-rate transition",
    "external borrowing cost; LIBOR/prime spreads; public-sector share of external debt"
  ),
  
  unit = c(
    "US$ million, end of period",
    "US$ million",
    "US$ million",
    "ratios / percent",
    "US$ million; percent GDP",
    "historical narrative / tables",
    "US$ million; rates / ratios",
    "US$ million",
    "rates / historical narrative"
  ),
  
  url = c(
    "https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?hdOidSeriesSelecionadas=3560&method=consultarGraficoPorId",
    "https://doi.org/10.1590/0101-31571982-2005",
    "https://doi.org/10.1590/0101-31571983-2103",
    "https://doi.org/10.1590/0101-31571983-2103",
    "https://doi.org/10.1590/0101-31571987-4005",
    "https://doi.org/10.1590/0101-31571987-2066",
    "https://www.nber.org/books-and-chapters/developing-country-debt-and-economic-performance-volume-2-country-studies-argentina-bolivia-brazil-mexico/macroeconomics-brazilian-external-debt",
    "https://doi.org/10.1590/0101-31571992-0638",
    "https://documents.worldbank.org/"
  ),
  
  definition_notes = c(
    paste(
      "Official BCB annual registered external debt.",
      "Series is inactive but historical observations remain available.",
      "Registered debt is not automatically identical to total/gross/net debt."
    ),
    paste(
      "Table sources: Conjuntura Econômica and Banco Central.",
      "Gross debt minus reserves equals reported net debt.",
      "Excellent internal consistency for the 1967-1980 window."
    ),
    paste(
      "Table sources: Banco Central and Conjuntura Econômica.",
      "Very close to Serra Table 7 but retains balance-of-payments detail.",
      "Signs are preserved in the source table."
    ),
    paste(
      "Debt/GDP is reported directly by the historical source.",
      "Useful because nominal debt is in US dollars whereas historical GDP",
      "in domestic currency cannot be divided without an exchange-rate conversion."
    ),
    paste(
      "Debt concept is broader than the registered/gross series used in the",
      "early-1980s tables, producing higher debt levels.",
      "Keep as an alternative concept; do not splice mechanically."
    ),
    paste(
      "Historical mechanism source.",
      "Defines 1974-1979 II PND as a growth-cum-debt external-adjustment strategy."
    ),
    paste(
      "Core conceptual source for the model debt block.",
      "Reports net debt growth and discusses borrowing conditions.",
      "Do not transcribe unverified appendix rate observations in this script."
    ),
    paste(
      "Commercial-bank debt composition.",
      "Notes fixed-rate loans until 1973 and floating rates thereafter,",
      "generally repriced every six months; Resolution 63 and Law 4131."
    ),
    paste(
      "Reports that Brazil generally paid spreads of roughly 1-2 percentage",
      "points over LIBOR/prime before the 1982 cutoff and discusses an ex-post",
      "borrowing-cost measure based on interest payments relative to debt.",
      "This is evidence for later construction, not a time series in this script."
    )
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  source_registry,
  file.path(raw_debt_dir, "source_registry.csv"),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 2. OFFICIAL BCB SERIES: SGS 3560
# =============================================================================
#
# Banco Central do Brasil - SGS 3560
#
# Series:
#   Dívida externa registrada - Total - anual
#
# Unit:
#   US$ million
#
# Frequency:
#   Annual
#
# IMPORTANT
# ---------
# This is an old/inactive historical SGS series.
#
# We download the full small annual series and filter 1964-1980 locally.
# CSV is attempted first. JSON is the fallback.
#
# =============================================================================

bcb_series_id <- 3560

bcb_csv_url <- paste0(
  "https://api.bcb.gov.br/dados/serie/bcdata.sgs.",
  bcb_series_id,
  "/dados?formato=csv"
)

bcb_json_url <- paste0(
  "https://api.bcb.gov.br/dados/serie/bcdata.sgs.",
  bcb_series_id,
  "/dados?formato=json"
)

# Local cache written by this script after a successful official download.
# It is also accepted as a fallback when the BCB endpoint is unavailable.
bcb_local_cache_file <- file.path(
  raw_debt_dir,
  "bcb_sgs3560_registered_external_debt_1964_1980.csv"
)

# -----------------------------------------------------------------------------
# Safe parser for BCB values
# -----------------------------------------------------------------------------
#
# Handles:
#   12571.5   -> 12571.5
#   12571,5   -> 12571.5
#   12.571,5  -> 12571.5
#
# Critically, decimal points are NOT removed unless a comma is also present,
# so values like 12571.5 do not become 125715.

parse_bcb_value <- function(x) {
  
  x <- trimws(
    as.character(x)
  )
  
  x[
    x %in% c(
      "",
      "NA",
      "null"
    )
  ] <- NA_character_
  
  out <- rep(
    NA_real_,
    length(x)
  )
  
  for (i in seq_along(x)) {
    
    z <- x[i]
    
    if (is.na(z)) {
      next
    }
    
    has_comma <- grepl(
      ",",
      z,
      fixed = TRUE
    )
    
    has_dot <- grepl(
      ".",
      z,
      fixed = TRUE
    )
    
    # Brazilian thousands + decimal:
    # 12.571,5 -> 12571.5
    if (
      has_comma &&
      has_dot
    ) {
      
      z <- gsub(
        ".",
        "",
        z,
        fixed = TRUE
      )
      
      z <- sub(
        ",",
        ".",
        z,
        fixed = TRUE
      )
      
      # Decimal comma only:
      # 12571,5 -> 12571.5
    } else if (
      has_comma
    ) {
      
      z <- sub(
        ",",
        ".",
        z,
        fixed = TRUE
      )
      
      # Decimal point only:
      # 12571.5
      # Leave unchanged.
    }
    
    out[i] <- suppressWarnings(
      as.numeric(z)
    )
  }
  
  out
}

# -----------------------------------------------------------------------------
# Attempt 1: official CSV endpoint
# -----------------------------------------------------------------------------

bcb_all <- NULL

tmp_csv <- tempfile(
  fileext = ".csv"
)

csv_ok <- tryCatch(
  
  {
    
    utils::download.file(
      url = bcb_csv_url,
      destfile = tmp_csv,
      method = "libcurl",
      mode = "wb",
      quiet = TRUE
    )
    
    raw_csv <- utils::read.csv2(
      tmp_csv,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    
    if (
      !all(
        c(
          "data",
          "valor"
        ) %in%
        names(raw_csv)
      )
    ) {
      
      stop(
        "BCB CSV response does not contain columns 'data' and 'valor'."
      )
    }
    
    bcb_all <- data.frame(
      
      date = as.Date(
        raw_csv$data,
        format = "%d/%m/%Y"
      ),
      
      value = parse_bcb_value(
        raw_csv$valor
      ),
      
      stringsAsFactors = FALSE
    )
    
    TRUE
  },
  
  error = function(e) {
    
    message(
      "BCB CSV attempt failed: ",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (
  file.exists(
    tmp_csv
  )
) {
  
  unlink(
    tmp_csv
  )
}

# -----------------------------------------------------------------------------
# Attempt 2: official JSON endpoint
# -----------------------------------------------------------------------------

if (
  !csv_ok ||
  is.null(
    bcb_all
  ) ||
  !nrow(
    bcb_all
  )
) {
  
  message(
    "Trying BCB JSON endpoint..."
  )
  
  tmp_json <- tempfile(
    fileext = ".json"
  )
  
  json_ok <- tryCatch(
    
    {
      
      utils::download.file(
        url = bcb_json_url,
        destfile = tmp_json,
        method = "libcurl",
        mode = "wb",
        quiet = TRUE
      )
      
      raw_json <- jsonlite::fromJSON(
        tmp_json
      )
      
      if (
        !all(
          c(
            "data",
            "valor"
          ) %in%
          names(raw_json)
        )
      ) {
        
        stop(
          "BCB JSON response does not contain columns 'data' and 'valor'."
        )
      }
      
      bcb_all <- data.frame(
        
        date = as.Date(
          raw_json$data,
          format = "%d/%m/%Y"
        ),
        
        value = parse_bcb_value(
          raw_json$valor
        ),
        
        stringsAsFactors = FALSE
      )
      
      TRUE
    },
    
    error = function(e) {
      
      message(
        "BCB JSON attempt failed: ",
        conditionMessage(e)
      )
      
      FALSE
    }
  )
  
  if (
    file.exists(
      tmp_json
    )
  ) {
    
    unlink(
      tmp_json
    )
  }
}

# -----------------------------------------------------------------------------
# Attempt 3: local previously validated cache
# -----------------------------------------------------------------------------

if (
  is.null(bcb_all) ||
  !nrow(bcb_all)
) {

  if (file.exists(bcb_local_cache_file)) {

    message(
      "Official BCB endpoints unavailable. Trying local SGS 3560 cache..."
    )

    cached_bcb <- tryCatch(
      read.csv(
        bcb_local_cache_file,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      error = function(e) NULL
    )

    if (
      !is.null(cached_bcb) &&
      all(
        c(
          "date",
          "registered_external_debt_usd_million"
        ) %in% names(cached_bcb)
      )
    ) {

      bcb_all <- data.frame(
        date = as.Date(cached_bcb$date),
        value = suppressWarnings(
          as.numeric(
            cached_bcb$registered_external_debt_usd_million
          )
        ),
        stringsAsFactors = FALSE
      )

      message(
        "Local SGS 3560 cache loaded successfully."
      )
    }
  }
}

# -----------------------------------------------------------------------------
# Validate and filter BCB data
# -----------------------------------------------------------------------------

if (
  is.null(
    bcb_all
  ) ||
  !nrow(
    bcb_all
  )
) {
  
  warning(
    paste(
      "BCB SGS 3560 could not be downloaded.",
      "Historical-paper tables remain usable, but the official BCB",
      "backbone is missing."
    )
  )
  
  bcb_3560 <- data.frame(
    
    source_id =
      character(0),
    
    date =
      as.Date(
        character(0)
      ),
    
    year =
      integer(0),
    
    registered_external_debt_usd_million =
      numeric(0),
    
    stringsAsFactors = FALSE
  )
  
} else {
  
  bcb_all$year <- as.integer(
    format(
      bcb_all$date,
      "%Y"
    )
  )
  
  bcb_3560 <- bcb_all[
    bcb_all$year >= 1964 &
      bcb_all$year <= 1980,
    ,
    drop = FALSE
  ]
  
  names(
    bcb_3560
  )[
    names(
      bcb_3560
    ) ==
      "value"
  ] <-
    "registered_external_debt_usd_million"
  
  bcb_3560$source_id <-
    "BCB_SGS_3560"
  
  bcb_3560 <- bcb_3560[
    ,
    c(
      "source_id",
      "date",
      "year",
      "registered_external_debt_usd_million"
    )
  ]
  
  bcb_3560 <- bcb_3560[
    order(
      bcb_3560$year
    ),
    ,
    drop = FALSE
  ]
  
  expected_years <-
    1964:1980
  
  missing_years <- setdiff(
    expected_years,
    bcb_3560$year
  )
  
  duplicated_years <- unique(
    bcb_3560$year[
      duplicated(
        bcb_3560$year
      )
    ]
  )
  
  if (
    length(
      missing_years
    )
  ) {
    
    warning(
      "BCB SGS 3560 is missing expected year(s): ",
      paste(
        missing_years,
        collapse = ", "
      )
    )
  }
  
  if (
    length(
      duplicated_years
    )
  ) {
    
    warning(
      "BCB SGS 3560 contains duplicated year(s): ",
      paste(
        duplicated_years,
        collapse = ", "
      )
    )
  }
  
  if (
    any(
      !is.finite(
        bcb_3560$registered_external_debt_usd_million
      )
    )
  ) {
    
    warning(
      "BCB SGS 3560 contains non-numeric or missing debt observations."
    )
  }

  # Deterministic local validation.
  #
  # These checks are intentionally simple and transparent rather than
  # cryptographic: they detect the historical decimal-parsing failures that
  # previously turned values such as 6621.3 into 66213.
  expected_bcb_years <- 1964:1980
  expected_bcb_sum <- 299560.1
  expected_bcb_weighted_sum <- 3977069.8

  bcb_checksum_sum <- sum(
    bcb_3560$registered_external_debt_usd_million,
    na.rm = TRUE
  )

  bcb_checksum_weighted <- sum(
    seq_len(nrow(bcb_3560)) *
      bcb_3560$registered_external_debt_usd_million,
    na.rm = TRUE
  )

  if (
    identical(bcb_3560$year, expected_bcb_years) &&
    (
      abs(bcb_checksum_sum - expected_bcb_sum) > 1e-6 ||
      abs(bcb_checksum_weighted - expected_bcb_weighted_sum) > 1e-6
    )
  ) {
    stop(
      paste(
        "BCB SGS 3560 checksum validation failed.",
        "Expected sum=", expected_bcb_sum,
        "and weighted sum=", expected_bcb_weighted_sum,
        "but obtained", bcb_checksum_sum,
        "and", bcb_checksum_weighted, ".",
        "Check decimal parsing or the cached/downloaded source."
      )
    )
  }

  # Historical cross-check against independent literature.
  historical_check <- data.frame(
    
    year = c(
      1968,
      1972,
      1973,
      1979,
      1980
    ),
    
    literature_value_usd_million = c(
      3780,
      9521,
      12571,
      49904,
      53847
    )
  )
  
  historical_check$bcb_value_usd_million <-
    bcb_3560$registered_external_debt_usd_million[
      match(
        historical_check$year,
        bcb_3560$year
      )
    ]
  
  historical_check$difference_usd_million <-
    historical_check$bcb_value_usd_million -
    historical_check$literature_value_usd_million
  
  historical_check$pct_difference <-
    100 *
    historical_check$difference_usd_million /
    historical_check$literature_value_usd_million
  
  write.csv(
    historical_check,
    file.path(
      processed_dir,
      "bcb3560_historical_source_check.csv"
    ),
    row.names = FALSE,
    na = ""
  )
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "BCB SGS 3560 DOWNLOAD SUCCESSFUL\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "\nSeries: Dívida externa registrada - Total - anual\n"
  )
  
  cat(
    "Unit: US$ million\n"
  )
  
  cat(
    "Window retained: 1964-1980\n"
  )
  
  cat(
    "Observations retained: ",
    nrow(
      bcb_3560
    ),
    "\n\n",
    sep = ""
  )
  
  print(
    bcb_3560,
    row.names = FALSE
  )
  
  cat(
    "\nHistorical literature cross-check:\n\n"
  )
  
  print(
    historical_check,
    row.names = FALSE
  )
}

write.csv(
  bcb_3560,
  bcb_local_cache_file,
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 3. SERRA (1982), TABLE 7
# =============================================================================

serra_t7 <- data.frame(
  
  source_id = "SERRA_1982_TABLE7",
  
  year = 1967:1981,
  
  gross_external_debt_usd_million = c(
    3372, 3780, 4403, 5295, 6622,
    9521, 12571, 17166, 21171, 25985,
    32037, 43511, 49904, 53847, 63247
  ),
  
  international_reserves_usd_million = c(
    199, 256, 655, 1187, 1723,
    4183, 6416, 5269, 4040, 6544,
    7256, 11895, 9689, 6913, 6900
  ),
  
  net_external_debt_usd_million = c(
    3173, 3524, 3748, 4108, 4899,
    5338, 6155, 11897, 17131, 19441,
    24781, 31616, 40215, 46934, 56347
  ),
  
  current_account_deficit_usd_million = c(
    237, 508, 281, 562, 1307,
    1489, 1688, 7122, 6700, 6013,
    4037, 6990, 10742, 12886, 10600
  ),
  
  net_interest_payments_usd_million = c(
    184, 144, 182, 234, 302,
    359, 514, 652, 1498, 1809,
    2103, 2696, 4106, 6311, 8700
  ),
  
  profit_remittances_usd_million = c(
    73, 84, 21, 119, 118,
    161, 198, 248, 235, 380,
    455, 564, 636, 310, NA
  ),
  
  foreign_investment_usd_million = c(
    115, 111, 322, 168, 564,
    492, 1175, 1448, 1276, 1702,
    2223, 2512, 2212, 1923, NA
  ),
  
  debt_service_usd_million = c(
    628, 960, 1250, 1476, 1685,
    2322, 2577, 2595, 3666, 4814,
    6226, 8122, 10713, 13013, 16400
  ),
  
  stringsAsFactors = FALSE
)

serra_t7$net_debt_check <-
  serra_t7$gross_external_debt_usd_million -
  serra_t7$international_reserves_usd_million

serra_t7$net_debt_table_minus_check <-
  serra_t7$net_external_debt_usd_million -
  serra_t7$net_debt_check

write.csv(
  serra_t7,
  file.path(
    raw_debt_dir,
    "serra_1982_table7_external_debt.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 4. BRESSER-PEREIRA (1983), TABLE 4
# =============================================================================

bresser_1983_t4 <- data.frame(
  
  source_id = "BRESSER_1983_TABLE4",
  
  year = 1967:1981,
  
  exports_usd_million = c(
    1654, 1881, 2311, 2739, 2904,
    3991, 6199, 7951, 8670, 10128,
    12120, 12659, 15244, 20132, 23293
  ),
  
  imports_usd_million = c(
    1411, 1855, 1993, 2507, 3247,
    4232, 6192, 12641, 12210, 12383,
    12023, 13683, 18084, 22955, 22091
  ),
  
  trade_balance_usd_million = c(
    213, 26, 318, 232, -343,
    -241, 7, -4690, -3540, -2255,
    97, -1024, -2839, -2823, 1213
  ),
  
  net_services_ex_interest_profit_usd_million = c(
    -270, -328, -367, -462, -560,
    -730, -1010, -1532, -1429, -1574,
    -1576, -1720, -2378, -3120, -2837
  ),
  
  interest_payments_usd_million = c(
    184, 144, 182, 234, 302,
    359, 514, 652, 1498, 1809,
    2103, 2695, 4185, 6311, 9179
  ),
  
  current_account_balance_usd_million = c(
    -237, -508, -281, -562, -1307,
    -1489, -1688, -7122, -6700, -6013,
    -4037, -5927, -10742, -12807, -11717
  ),
  
  gross_external_debt_usd_million = c(
    3372, 3780, 4403, 5295, 6622,
    9521, 12572, 17166, 21171, 25985,
    32037, 43511, 49904, 53847, 61411
  ),
  
  international_reserves_usd_million = c(
    199, 257, 656, 1187, 1723,
    4183, 6415, 5269, 4040, 6644,
    7256, 11895, 9689, 6913, 7507
  ),
  
  stringsAsFactors = FALSE
)

bresser_1983_t4$net_external_debt_constructed_usd_million <-
  bresser_1983_t4$gross_external_debt_usd_million -
  bresser_1983_t4$international_reserves_usd_million

write.csv(
  bresser_1983_t4,
  file.path(
    raw_debt_dir,
    "bresser_pereira_1983_table4_bop_debt.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 5. BRESSER-PEREIRA (1983), TABLE 5
# =============================================================================

bresser_1983_t5 <- data.frame(
  
  source_id = "BRESSER_1983_TABLE5",
  
  year = 1970:1981,
  
  import_coefficient_pct = c(
    5.4, 6.1, 6.7, 7.4, 11.5, 9.3,
    7.8, 6.7, 6.6, 7.7, 9.2, 7.7
  ),
  
  import_coefficient_ex_oil_pct = c(
    5.0, 5.5, 6.1, 6.7, 9.1, 7.3,
    5.7, 4.7, 4.6, 5.0, 5.4, 4.0
  ),
  
  terms_of_trade_index_1970_100 = c(
    100.0, 92.8, 98.2, 107.9, 88.3, 85.5,
    96.0, 112.7, 94.5, 89.1, 70.2, 65.1
  ),
  
  net_debt_to_exports_ratio = c(
    1.50, 1.69, 1.34, 0.99, 1.50, 1.98,
    1.92, 2.04, 2.50, 2.64, 2.33, 2.31
  ),
  
  external_debt_to_gdp_pct = c(
    9.0, 9.2, 8.6, 7.4, 10.8, 13.1,
    12.3, 13.8, 15.2, 17.2, 18.8, 18.8
  ),
  
  exchange_rate_valuation_index_1970_100 = c(
    100.0, 94.0, 92.4, 91.0, 87.0, 85.2,
    85.5, 81.7, 75.6, 65.2, 56.8, 68.9
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  bresser_1983_t5,
  file.path(
    raw_debt_dir,
    "bresser_pereira_1983_table5_external_ratios.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 6. BRESSER-PEREIRA (1987), TABLE 2
# =============================================================================

bresser_1987_t2 <- data.frame(
  
  source_id = "BRESSER_1987_TABLE2",
  
  year = 1970:1985,
  
  external_debt_usd_million = c(
    6049, 7947, 11026, 13962,
    18871, 24186, 30970, 36736,
    50143, 53986, 62765, 71878,
    83265, 91632, 99765, 100773
  ),
  
  increase_external_debt_usd_million = c(
    NA, 1898, 3079, 2936,
    4909, 5315, 6784, 5776,
    13407, 3843, 8779, 9113,
    11387, 8367, 8133, 1008
  ),
  
  interest_payments_usd_million = c(
    NA, 302, 359, 514,
    652, 1498, 1809, 2103,
    2696, 4185, 6311, 9161,
    11353, 9555, 10203, 9589
  ),
  
  real_resource_transfer_pct_gdp = c(
    -0.4, -1.7, -1.6, -1.2,
    -5.9, -4.0, -2.4, -0.6,
    -1.2, -2.0, -2.1, -0.4,
    -0.6, 2.3, 5.6, 5.2
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  bresser_1987_t2,
  file.path(
    raw_debt_dir,
    "bresser_pereira_1987_table2_debt_resource_transfer.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 7. BARBOSA & SANCHEZ DE LA CAL (1992), TABLE 2
# =============================================================================

barbosa_sanchez_t2 <- data.frame(
  
  source_id = "BARBOSA_SANCHEZ_1992_TABLE2",
  
  year = c(
    1973, 1975, 1977, 1979, 1980,
    1981, 1983, 1985, 1986, 1987,
    1988, 1989
  ),
  
  resolution_63_usd_million = c(
    2398, 3734, 5240, 7723, 9924,
    13456, 15115, 11379, 8772, 6831,
    4317, 2604
  ),
  
  law_4131_usd_million = c(
    5379, 10781, 16262, 26883, 27887,
    31520, 44511, 54809, 58406, 61099,
    60133, 58470
  ),
  
  commercial_bank_external_debt_total_usd_million = c(
    7777, 14515, 21502, 34606, 37811,
    44976, 59626, 66188, 67178, 67930,
    64450, 61074
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  barbosa_sanchez_t2,
  file.path(
    raw_debt_dir,
    "barbosa_sanchez_1992_table2_commercial_bank_debt.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 8. HISTORICAL REGIME / EVENT TABLE
# =============================================================================

historical_events <- data.frame(
  
  start_year = c(
    1967, 1972, 1973, 1974, 1974, 1979, 1979, 1980
  ),
  
  end_year = c(
    1973, 1973, 1974, 1979, 1979, 1980, 1982, 1980
  ),
  
  event_id = c(
    "economic_miracle",
    "sovereign_bond_return",
    "first_oil_shock",
    "II_PND",
    "growth_cum_debt_strategy",
    "second_oil_shock",
    "world_interest_rate_shock",
    "external_adjustment_tightening"
  ),
  
  description = c(
    "High-growth pre-first-oil-shock period.",
    "Brazil resumed sovereign bond placements abroad before the first oil shock.",
    "Major foreign-price/oil shock; sharp deterioration in import bill.",
    "Second National Development Plan; public/private projects in capital goods and basic inputs.",
    "External debt used to sustain growth, investment and current-account deficits.",
    "Second major oil shock.",
    "Sharp rise in international interest rates; worsened floating-rate debt burden.",
    "Increasing external constraint and policy tightening."
  ),
  
  main_sources = c(
    "Serra 1982; Bresser-Pereira 1983",
    "historical external-finance literature",
    "Bresser-Pereira 1983; Cardoso-Fishlow; Barbosa-Sanchez",
    "Batista 1987; Barbosa-Sanchez 1992",
    "Batista 1987; Cardoso-Fishlow",
    "Bresser-Pereira 1983; Barbosa-Sanchez 1992",
    "Cardoso-Fishlow; World Bank; Barbosa-Sanchez 1992",
    "Bresser-Pereira 1983"
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  historical_events,
  file.path(
    raw_debt_dir,
    "historical_external_debt_events.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 9. RISK-PREMIUM / BORROWING-COST EVIDENCE TABLE
# =============================================================================
#
# These are NOT annual risk-premium observations.

risk_premium_evidence <- data.frame(
  
  evidence_id = c(
    "FLOATING_RATE_SHIFT_1973",
    "LIBOR_PRIME_REFERENCE",
    "BRAZIL_SPREAD_RANGE_PRE1982",
    "EX_POST_INTEREST_COST_METHOD",
    "CARDOSO_FISHLOW_SPREAD_SOURCE"
  ),
  
  period = c(
    "from 1973",
    "1970s-early 1980s",
    "pre-1982",
    "1965-1990 in World Bank study",
    "1968-1986 cited by later IMF work"
  ),
  
  evidence = c(
    paste(
      "Commercial-bank foreign loans were generally fixed-rate until 1973;",
      "floating rates were introduced thereafter, generally repriced every six months."
    ),
    paste(
      "Resolution 63 and other foreign borrowing commonly referenced LIBOR",
      "or the U.S. prime rate plus a country-risk spread."
    ),
    paste(
      "Historical World Bank discussion reports Brazil paying roughly",
      "1 to 2 percentage points over LIBOR/prime before access was curtailed in 1982."
    ),
    paste(
      "Historical study constructs an ex-post nominal external borrowing cost",
      "from interest payments relative to the external-debt stock."
    ),
    paste(
      "A later IMF data appendix identifies Cardoso & Fishlow as the source",
      "for a Brazilian external-interest-rate spread series over 1968-1986."
    )
  ),
  
  source_id = c(
    "BARBOSA_SANCHEZ_1992_TABLE2",
    "BARBOSA_SANCHEZ_1992_TABLE2",
    "WORLD_BANK_BRAZIL_1964_90",
    "WORLD_BANK_BRAZIL_1964_90",
    "CARDOSO_FISHLOW_1989"
  ),
  
  usable_as_time_series_now = c(
    FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  
  action_for_next_stage = c(
    "Use as regime/institutional evidence.",
    "Identify benchmark rate consistent with Script 01.",
    "Do not assign annual values from a range.",
    "Test alternative lag/current/average debt denominators before using.",
    "Locate and verify original annual observations before estimation."
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  risk_premium_evidence,
  file.path(
    raw_debt_dir,
    "risk_premium_borrowing_cost_evidence.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 10. HELPER FOR YEAR MATCHING
# =============================================================================

get_by_year <- function(df, value_col, years) {
  
  idx <- match(
    years,
    df$year
  )
  
  out <- rep(
    NA_real_,
    length(years)
  )
  
  good <- !is.na(idx)
  
  out[good] <- df[
    idx[good],
    value_col
  ]
  
  out
}

# =============================================================================
# 11. CROSS-SOURCE COMPARISON
# =============================================================================

comparison_years <- 1967:1980

cross_source <- data.frame(
  year = comparison_years
)

cross_source$serra_gross_debt <-
  get_by_year(
    serra_t7,
    "gross_external_debt_usd_million",
    comparison_years
  )

cross_source$bresser1983_gross_debt <-
  get_by_year(
    bresser_1983_t4,
    "gross_external_debt_usd_million",
    comparison_years
  )

cross_source$bresser1987_broader_debt <-
  get_by_year(
    bresser_1987_t2,
    "external_debt_usd_million",
    comparison_years
  )

cross_source$bcb_registered_debt <-
  if (nrow(bcb_3560)) {
    get_by_year(
      bcb_3560,
      "registered_external_debt_usd_million",
      comparison_years
    )
  } else {
    rep(
      NA_real_,
      length(comparison_years)
    )
  }

cross_source$serra_minus_bresser1983 <-
  cross_source$serra_gross_debt -
  cross_source$bresser1983_gross_debt

cross_source$broader_minus_serra <-
  cross_source$bresser1987_broader_debt -
  cross_source$serra_gross_debt

cross_source$bcb_minus_serra <-
  cross_source$bcb_registered_debt -
  cross_source$serra_gross_debt

write.csv(
  cross_source,
  file.path(
    processed_dir,
    "external_debt_cross_source_comparison_1967_1980.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 12. BUILD MASTER RESEARCH PANEL, 1964-1980
# =============================================================================

master <- data.frame(
  year = 1964:1980
)

master$bcb_registered_external_debt_usd_million <-
  if (nrow(bcb_3560)) {
    get_by_year(
      bcb_3560,
      "registered_external_debt_usd_million",
      master$year
    )
  } else {
    rep(
      NA_real_,
      nrow(master)
    )
  }

for (
  nm in c(
    "gross_external_debt_usd_million",
    "international_reserves_usd_million",
    "net_external_debt_usd_million",
    "current_account_deficit_usd_million",
    "net_interest_payments_usd_million",
    "debt_service_usd_million"
  )
) {
  
  master[[paste0("serra_", nm)]] <- get_by_year(serra_t7, nm, master$year)
}

for (
  nm in c(
    "exports_usd_million",
    "imports_usd_million",
    "trade_balance_usd_million",
    "interest_payments_usd_million",
    "current_account_balance_usd_million",
    "gross_external_debt_usd_million",
    "international_reserves_usd_million"
  )
) {
  
  master[[paste0("bresser1983_", nm)]] <- get_by_year(bresser_1983_t4, nm, master$year)
}

for (
  nm in c(
    "external_debt_to_gdp_pct",
    "net_debt_to_exports_ratio",
    "terms_of_trade_index_1970_100",
    "import_coefficient_pct",
    "import_coefficient_ex_oil_pct"
  )
) {
  
  master[[paste0("bresser1983_", nm)]] <- get_by_year(bresser_1983_t5, nm, master$year)
}

master$bresser1987_broader_external_debt_usd_million <-
  get_by_year(
    bresser_1987_t2,
    "external_debt_usd_million",
    master$year
  )

master$bresser1987_interest_payments_usd_million <-
  get_by_year(
    bresser_1987_t2,
    "interest_payments_usd_million",
    master$year
  )

master$bresser1987_real_resource_transfer_pct_gdp <-
  get_by_year(
    bresser_1987_t2,
    "real_resource_transfer_pct_gdp",
    master$year
  )

for (
  nm in c(
    "resolution_63_usd_million",
    "law_4131_usd_million",
    "commercial_bank_external_debt_total_usd_million"
  )
) {
  
  master[[paste0("barbosa_sanchez_", nm)]] <- get_by_year(barbosa_sanchez_t2, nm, master$year)
}

master$pre_first_oil_baseline <-
  as.integer(
    master$year >= 1968 &
      master$year <= 1972
  )

master$first_oil_shock <-
  as.integer(
    master$year %in%
      c(1973, 1974)
  )

master$II_PND <-
  as.integer(
    master$year >= 1974 &
      master$year <= 1979
  )

master$second_oil_shock <-
  as.integer(
    master$year %in%
      c(1979, 1980)
  )

# =============================================================================
# 13. ACCOUNTING DIAGNOSTICS (NOT DSGE PARAMETERS)
# =============================================================================

master$serra_net_debt_growth_pct <- c(
  NA,
  100 *
    diff(
      log(
        master$serra_net_external_debt_usd_million
      )
    )
)

master$serra_interest_to_lag_gross_debt_pct <-
  100 *
  master$serra_net_interest_payments_usd_million /
  c(
    NA,
    head(
      master$serra_gross_external_debt_usd_million,
      -1
    )
  )

master$serra_interest_to_current_gross_debt_pct <-
  100 *
  master$serra_net_interest_payments_usd_million /
  master$serra_gross_external_debt_usd_million

master$serra_debt_service_to_exports_pct <-
  100 *
  master$serra_debt_service_usd_million /
  master$bresser1983_exports_usd_million

write.csv(
  master,
  file.path(
    processed_dir,
    "brazil_external_debt_master_1964_1980.csv"
  ),
  row.names = FALSE,
  na = ""
)

# =============================================================================
# 14. DATA QUALITY / RECONCILIATION FLAGS
# =============================================================================

quality <- data.frame(
  year = 1967:1980
)

quality$serra_net_identity_error_usd_million <-
  get_by_year(
    serra_t7,
    "net_debt_table_minus_check",
    quality$year
  )

quality$serra_vs_bresser1983_gross_debt_difference <-
  get_by_year(
    serra_t7,
    "gross_external_debt_usd_million",
    quality$year
  ) -
  get_by_year(
    bresser_1983_t4,
    "gross_external_debt_usd_million",
    quality$year
  )

quality$serra_vs_bresser1983_reserves_difference <-
  get_by_year(
    serra_t7,
    "international_reserves_usd_million",
    quality$year
  ) -
  get_by_year(
    bresser_1983_t4,
    "international_reserves_usd_million",
    quality$year
  )

quality$broader_debt_concept_gap <-
  get_by_year(
    bresser_1987_t2,
    "external_debt_usd_million",
    quality$year
  ) -
  get_by_year(
    serra_t7,
    "gross_external_debt_usd_million",
    quality$year
  )

write.csv(
  quality,
  file.path(
    processed_dir,
    "external_debt_data_quality_flags_1967_1980.csv"
  ),
  row.names = FALSE,
  na = ""
)


# =============================================================================
# 15. CROSS-ERA EXTERNAL-FINANCING / RISK-PREMIUM ARCHITECTURE
# =============================================================================
#
# IMPORTANT CONCEPTUAL CHANGE
# ---------------------------
#
# We no longer force one observed financial instrument to represent rp_t over
# 1968-1994.
#
# The STRUCTURAL model object remains:
#
#   rp_t = phiD * d_t + eps_rp,t
#
# but the best historical observable changes with Brazil's financing regime.
#
# This section builds the measurement map needed for Papers I-III.
#
# No phiD or rp parameter is estimated here.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 15.1 Cross-era measurement map
# -----------------------------------------------------------------------------

rp_cross_era_map <- data.frame(
  
  era_id = c(
    "PAPER1_BANK_LOANS",
    "DEBT_CRISIS_BOND_BRIDGE",
    "DEBT_CRISIS_SECONDARY_MARKET",
    "BOND_MARKET_REENTRY",
    "BRADY_REAL_PLAN"
  ),
  
  start_year = c(
    1968,
    1980,
    1985,
    1991,
    1994
  ),
  
  end_year = c(
    1981,
    1985,
    1990,
    1993,
    2000
  ),
  
  financing_regime = c(
    "Voluntary syndicated Eurocurrency bank lending",
    "Debt-crisis transition; normal bank lending impaired; sovereign bonds provide market-risk evidence",
    "Restructuring, moratorium, concerted lending and secondary bank-debt market",
    "Return to voluntary international bond-market financing",
    "Brady restructuring plus tradable sovereign bond / Eurobond market"
  ),
  
  preferred_observable = c(
    "Brazil spread on new syndicated loans over LIBOR",
    "Brazil sovereign-bond yield spread in the secondary market",
    "Secondary-market price of Brazilian bank claims; restructuring/new-money spread as supporting evidence",
    "Brazil EMBI / international sovereign-bond spread",
    "Brazil EMBI plus Brady/Eurobond stripped spreads"
  ),
  
  model_mapping = c(
    "rp_model = (LIBOR - TB3MS) + Brazil loan spread over LIBOR",
    "Use market sovereign-bond spread as external-financing-stress observable; do not equate forced bank-loan spreads with free-market risk",
    "Use debt-price collapse as stress indicator; do not convert mechanically into an interest spread without maturity/coupon assumptions",
    "Use market sovereign spread directly, adjusting benchmark only if needed for consistency with model istar",
    "Use market sovereign spread directly; Brady/Eurobond spreads become especially informative around the Real Plan"
  ),
  
  paper_role = c(
    "Paper I baseline",
    "Paper II early-debt-crisis bridge",
    "Paper II debt-crisis stress regime",
    "Paper II late / Paper III pre-Real bridge",
    "Paper III baseline"
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  rp_cross_era_map,
  file.path(
    processed_dir,
    "external_financing_cross_era_measurement_map.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.2 New source registry for cross-era financing evidence
# -----------------------------------------------------------------------------

rp_source_registry <- data.frame(
  
  source_id = c(
    "BACHA_DIAZ_ALEJANDRO_1982_T4",
    "SACHS_FISHLOW_1981_SPREADS",
    "CEPAL_1972_BRAZIL_SPREAD",
    "AIOLFI_CATAO_TIMMERMANN_2010",
    "EDWARDS_1985_BOND_SPREADS",
    "IMF_SECONDARY_DEBT_PRICES",
    "WORLD_BANK_EMBI_COVERAGE",
    "BRAZIL_BRADY_1994",
    "SCHMITT_GROHE_URIBE_2003",
    "QUARTERLY_SOE_RISK_PREMIUM_0001"
  ),
  
  role = c(
    "Paper I baseline marginal financing terms",
    "Paper I robustness marginal loan spread",
    "Approximate pre-shock spread anchor",
    "Published effective-cost spread validation",
    "Paper II early debt-crisis market-risk evidence",
    "Paper II secondary-market stress evidence",
    "Paper II/III market-spread source definition",
    "Paper III Brady-regime source definition",
    "Structural debt-elastic-premium foundation",
    "Quarterly phiD calibration reference"
  ),
  
  coverage = c(
    "1974-1980",
    "1974-1980",
    "around mid-1972",
    "Brazil Ext.Spread 1968-1986",
    "monthly 1980-10 to 1985-03",
    "annual 1985-1989 used here; richer 1986-90 market data exist",
    "Brazil EMBI coverage begins 1991-12-31 in cited research dataset",
    "Brady restructuring completed / bonds issued April 1994",
    "generic SOE calibration",
    "quarterly SOE calibration"
  ),
  
  definition = c(
    "Annual LIBOR plus contractual Brazil spread over LIBOR; Brazil spread for 1974 covers last three quarters",
    "Direct Brazil new-loan spread over LIBOR reproduced by Sachs from Fishlow",
    "Brazil commercial-money spread around 1.5 percentage points over LIBOR; not a full-year average",
    "Effective average external-debt cost spread sourced to Cardoso & Fishlow; not the same as marginal loan spread",
    "Brazil sovereign-bond yield spread relative to comparable high-grade benchmark; market measure during debt crisis",
    "Price of Brazilian external bank debt in cents per dollar of face value",
    "Market sovereign spread series; use actual underlying EMBI data when Paper II/III database is built",
    "Tradable restructured sovereign claims; useful for Real Plan-era market risk",
    "Debt-elastic premium p(d)=psi2*(exp(d-dbar)-1); psi2 baseline 0.000742 in original calibration",
    "Quarterly SOE literature uses risk-premium elasticity 0.001 as small stationarity parameter comparable to SGU"
  ),
  
  url = c(
    "https://ies.princeton.edu/pdf/E147.pdf",
    "https://www.brookings.edu/wp-content/uploads/1981/01/1981a_bpea_sachs_cooper_fischer.pdf",
    "https://repositorio.cepal.org/server/api/core/bitstreams/7518a6ae-13fa-401a-8e24-9d226dcd7a53/content",
    "https://rady.ucsd.edu/_files/faculty-research/timmermann/LAC4_database_Aiolfi-Catao-Timmermann_1870-2004.xls",
    "https://www.nber.org/papers/w1689",
    "https://www.elibrary.imf.org/view/journals/024/1993/001/article-A006-en.xml",
    "https://documents1.worldbank.org/curated/en/510191468335456127/pdf/773870JRN020020g0Market0Instability.pdf",
    "https://documents.worldbank.org/curated/en/967171469672182286/pdf/700810ESW0P1160Brazilian0Experience.pdf",
    "https://www.columbia.edu/~mu2166/closing_jie.pdf",
    "https://onlinelibrary.wiley.com/doi/full/10.1111/obes.12628"
  ),
  
  use_now = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE
  ),
  
  measurement_warning = c(
    "Marginal loan pricing; includes global LIBOR benchmark and Brazil-specific contractual spread",
    "Robustness source; 1980 differs materially from some revised sources",
    "Approximate mid-year observation only; never label as annual average",
    "Inherited-debt average cost measure; legacy fixed/floating composition creates repricing lag",
    "Do not splice mechanically to bank-loan spread; instrument and market regime differ",
    "A debt price is not an interest spread unless coupon, maturity, recovery and payment timing are modeled",
    "EMBI composition changes over time; document index definition when Paper III is built",
    "Brady and Eurobond instruments can have different collateral/maturity; prefer standardized spread measures when available",
    "Original calibration is not Brazil-specific and uses a different model frequency/normalization",
    "Chosen as parsimonious quarterly stationarity calibration, not as an estimated Brazilian causal elasticity"
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  rp_source_registry,
  file.path(
    raw_debt_dir,
    "external_financing_cross_era_source_registry.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.3 Paper-I baseline: Bacha & Diaz-Alejandro (1982), Table 4
# -----------------------------------------------------------------------------
#
# Indicators of lending conditions in the Eurocurrency market.
#
# Brazil spread is the contractual spread over LIBOR on new borrowing.
# This is a MARGINAL borrowing-price measure and is therefore much closer to
# the model's UIP/external-financing wedge than interest/debt accounting ratios.
#
# 1974 Brazil spread refers to the last three quarters only.
# 1979-1980 figures incorporate revisions reported by the authors.
#

bacha_diaz_1982_t4 <- data.frame(
  
  source_id = "BACHA_DIAZ_ALEJANDRO_1982_T4",
  
  year = 1974:1980,
  
  libor_annual_pct = c(
    11.32,
    7.74,
    6.26,
    6.54,
    9.48,
    12.12,
    14.28
  ),
  
  brazil_spread_over_libor_pp = c(
    1.23,
    1.70,
    1.91,
    1.96,
    1.59,
    1.08,
    1.18
  ),
  
  measurement = c(
    "last_three_quarters",
    rep(
      "annual",
      6
    )
  ),
  
  stringsAsFactors = FALSE
)


bacha_diaz_1982_t4$brazil_marginal_borrowing_rate_annual_pct <-
  bacha_diaz_1982_t4$libor_annual_pct +
  bacha_diaz_1982_t4$brazil_spread_over_libor_pp


write.csv(
  bacha_diaz_1982_t4,
  file.path(
    raw_debt_dir,
    "bacha_diaz_alejandro_1982_table4_brazil_libor_spreads_1974_1980.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.4 Paper-I robustness: Sachs/Fishlow direct Brazil spreads over LIBOR
# -----------------------------------------------------------------------------
#
# 1974-1979 form the clean comparison block used for robustness.
# 1980 = 1.75 pp is retained because it is reported in the source discussion,
# but it differs from revised estimates in Bacha-Diaz-Alejandro and should NOT
# be silently substituted for the baseline 1980 observation.
#

sachs_fishlow_spreads <- data.frame(
  
  source_id = "SACHS_FISHLOW_1981_SPREADS",
  
  year = 1974:1980,
  
  brazil_spread_over_libor_pp = c(
    1.10,
    1.80,
    2.00,
    2.00,
    1.70,
    0.90,
    1.75
  ),
  
  status = c(
    rep(
      "clean_robustness",
      6
    ),
    "1980_source_sensitive"
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  sachs_fishlow_spreads,
  file.path(
    raw_debt_dir,
    "sachs_fishlow_brazil_spread_over_libor_1974_1980.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.5 Approximate pre-shock anchor
# -----------------------------------------------------------------------------
#
# Contemporary CEPAL evidence states that Brazil paid roughly 1.5 percentage
# points over LIBOR around mid-1972.
#
# This is NOT a full-year annual average and must never be treated as one.
# It is retained only as a transparent pre-first-oil-shock centering anchor.
#
# The 1972 annual LIBOR value of 6.33 percent is retained from the historical
# effective-interest-rate/LIBOR compilation discussed in the project.
#

paper1_pre_shock_rp_anchor <- data.frame(
  
  year = 1972,
  
  brazil_spread_over_libor_pp = 1.50,
  
  libor_annual_pct = 6.33,
  
  spread_timing = "approximate_midyear",
  
  use = "centering_anchor_and_sensitivity_only",
  
  source_spread = "CEPAL historical commercial-bank borrowing study",
  
  source_libor = "historical LIBOR compilation used in project",
  
  stringsAsFactors = FALSE
)


write.csv(
  paper1_pre_shock_rp_anchor,
  file.path(
    raw_debt_dir,
    "paper1_pre_shock_marginal_rp_anchor_1972.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.6 Aiolfi-Catao-Timmermann / Cardoso-Fishlow effective-cost spread
# -----------------------------------------------------------------------------
#
# This is deliberately kept SEPARATE from the marginal spread.
#
# The published Ext.Spread series behaves like an average/effective cost of
# inherited external debt relative to a U.S. Treasury benchmark. It validates
# the old interest-cost logic but is not the preferred UIP marginal rp measure.
#
# If the downloaded Aiolfi CSV is present in data/raw/historical_external_debt,
# the script extracts the series automatically. Otherwise it writes the
# verified 1968-1980 values as a transparent fallback transcription.
#

aiolfi_local_file <- file.path(
  raw_debt_dir,
  "LAC4_database_Aiolfi-Catao-Timmermann_1870-2004.csv"
)


extract_aiolfi_ext_spread <- function(path) {
  
  raw_aiolfi <- read.csv(
    path,
    header = FALSE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  year_header_row <- which(
    apply(
      raw_aiolfi,
      1,
      function(z) any(trimws(as.character(z)) == "Year", na.rm = TRUE)
    )
  )
  
  if (!length(year_header_row)) {
    stop("Could not identify the Aiolfi header row containing 'Year'.")
  }
  
  h <- year_header_row[1]
  header_values <- trimws(as.character(raw_aiolfi[h, ]))
  
  year_col <- which(header_values == "Year")[1]
  spread_col <- which(header_values == "Ext.Spread")[1]
  
  if (is.na(year_col) || is.na(spread_col)) {
    stop("Could not identify Year and Ext.Spread columns in the Aiolfi CSV.")
  }
  
  dat <- raw_aiolfi[(h + 2):nrow(raw_aiolfi), c(year_col, spread_col)]
  names(dat) <- c("year", "effective_external_cost_spread_pp")
  
  dat$year <- suppressWarnings(as.integer(trimws(as.character(dat$year))))
  dat$effective_external_cost_spread_pp <- suppressWarnings(as.numeric(trimws(as.character(dat$effective_external_cost_spread_pp))))
  
  dat <- dat[
    is.finite(dat$year) &
      dat$year >= 1968 &
      dat$year <= 1986,
    ,
    drop = FALSE
  ]
  
  dat$source_id <- "AIOLFI_CATAO_TIMMERMANN_2010"
  dat$concept <- "published_effective_cost_spread_not_marginal_loan_spread"
  
  dat
}


if (file.exists(aiolfi_local_file)) {
  
  aiolfi_ext_spread <- tryCatch(
    extract_aiolfi_ext_spread(aiolfi_local_file),
    error = function(e) {
      warning(
        "Aiolfi CSV found but automatic extraction failed: ",
        conditionMessage(e),
        ". Using verified fallback values for 1968-1980."
      )
      NULL
    }
  )
  
} else {
  
  aiolfi_ext_spread <- NULL
}


if (is.null(aiolfi_ext_spread)) {
  
  aiolfi_ext_spread <- data.frame(
    
    year = 1968:1980,
    
    effective_external_cost_spread_pp = c(
      -0.65,
      -1.49,
      -0.24,
      3.06,
      3.23,
      2.57,
      2.73,
      6.78,
      5.61,
      5.53,
      3.68,
      3.16,
      4.09
    ),
    
    source_id = "AIOLFI_CATAO_TIMMERMANN_2010",
    
    concept = "published_effective_cost_spread_not_marginal_loan_spread",
    
    stringsAsFactors = FALSE
  )
}


write.csv(
  aiolfi_ext_spread,
  file.path(
    raw_debt_dir,
    "aiolfi_catao_timmermann_brazil_effective_external_cost_spread.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.7 Edwards (1985/1986): Brazilian sovereign-bond spread evidence
# -----------------------------------------------------------------------------
#
# We do NOT digitize the chart in this script.
#
# Only values explicitly described in the text are stored.
#

edwards_brazil_bond_evidence <- data.frame(
  
  source_id = "EDWARDS_1985_BOND_SPREADS",
  
  period = c(
    "1980-10_to_1982-07",
    "1982-07_to_1982-10",
    "1982-10_to_1983-04",
    "1983-04_to_1984-05",
    "1984-05_to_1985-01",
    "1985-03_to_1985-04"
  ),
  
  statistic = c(
    "level_description",
    "change_bps",
    "level_range_bps",
    "average_level_bps",
    "change_bps",
    "level_description"
  ),
  
  value_low = c(
    NA,
    418,
    280,
    285,
    490,
    NA
  ),
  
  value_high = c(
    NA,
    418,
    300,
    285,
    NA,
    NA
  ),
  
  note = c(
    "Brazilian bond spread was close to zero / sometimes negative before the debt crisis",
    "Brazil spread increased by 418 bp between July and October 1982",
    "Brazil spread stabilized around 280-300 bp",
    "Brazil spread averaged about 285 bp",
    "Brazil spread climbed by more than 490 bp",
    "Spread dropped sharply back toward its earlier 1982-84 range"
  ),
  
  use = "Paper II source evidence; do not mechanically splice with Paper-I bank-loan spreads",
  
  stringsAsFactors = FALSE
)


write.csv(
  edwards_brazil_bond_evidence,
  file.path(
    raw_debt_dir,
    "edwards_brazil_sovereign_bond_spread_evidence_1980_1985.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.8 Negotiated Brazilian bank restructuring / new-money terms
# -----------------------------------------------------------------------------
#
# These are CONTRACTUAL negotiated financing terms, not free-market sovereign
# risk prices.
#
# They are useful in Paper II to document the terms of concerted lending and
# restructuring, but they should not be mechanically equated with the market
# spread measured by Edwards' secondary-market bond data.
#

brazil_restructuring_terms <- data.frame(
  
  source_id = c(
    "IMF_BANK_RESTRUCTURING_TERMS",
    "IMF_BANK_RESTRUCTURING_TERMS",
    "IMF_BANK_RESTRUCTURING_TERMS",
    "IMF_BANK_RESTRUCTURING_TERMS",
    "IMF_BANK_RESTRUCTURING_TERMS"
  ),
  
  year = c(
    1983,
    1983,
    1984,
    1984,
    1986
  ),
  
  transaction = c(
    "Restructuring",
    "New financing",
    "Restructuring",
    "New financing",
    "Restructuring"
  ),
  
  spread_over_libor_low_pp = c(
    2.00,
    1.875,
    1.75,
    1.75,
    1.125
  ),
  
  spread_over_libor_high_pp = c(
    2.25,
    2.125,
    2.00,
    2.00,
    1.125
  ),
  
  grace_years = c(
    2.5,
    2.5,
    5,
    5,
    5
  ),
  
  maturity_years = c(
    8,
    8,
    9,
    9,
    7
  ),
  
  model_use = "Paper II contractual-financing validation; not baseline market-risk price",
  
  stringsAsFactors = FALSE
)


write.csv(
  brazil_restructuring_terms,
  file.path(
    raw_debt_dir,
    "brazil_bank_restructuring_terms_1983_1986.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.9 1985-1989 secondary-market prices of Brazilian external bank debt
# -----------------------------------------------------------------------------
#
# Prices are in cents per dollar / percent of face value.
#
# These are stress indicators, NOT interest-rate spreads.
#

brazil_secondary_debt_prices <- data.frame(
  
  source_id = "IMF_SECONDARY_DEBT_PRICES",
  
  year = 1985:1989,
  
  price_pct_face_value = c(
    77.2,
    75.6,
    56.6,
    47.8,
    30.3
  ),
  
  model_use = "external_financing_stress_indicator_only",
  
  conversion_warning = "Do not convert to a spread without explicit coupon, maturity, recovery and payment-timing assumptions.",
  
  stringsAsFactors = FALSE
)


write.csv(
  brazil_secondary_debt_prices,
  file.path(
    raw_debt_dir,
    "brazil_secondary_market_external_debt_prices_1985_1989.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.10 Future Paper-II / Paper-III market-spread source definitions
# -----------------------------------------------------------------------------

future_market_spread_sources <- data.frame(
  
  period = c(
    "1991-1993",
    "1994_onward"
  ),
  
  preferred_measure = c(
    "Brazil EMBI / voluntary international-bond spread",
    "Brazil EMBI plus Brady/Eurobond spreads"
  ),
  
  verified_start_or_event = c(
    "Brazil EMBI coverage begins 1991-12-31 in cited research dataset",
    "Brazil Brady restructuring and bond issuance completed in April 1994"
  ),
  
  use_when = c(
    "Build Paper-II late-period / Paper-III pre-Real database",
    "Build Paper-III Real Plan database"
  ),
  
  warning = c(
    "Do not backcast EMBI into the 1980s.",
    "Document instrument composition and benchmark; prefer standardized spread series where available."
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  future_market_spread_sources,
  file.path(
    raw_debt_dir,
    "future_brazil_market_spread_sources_1991_1994plus.csv"
  ),
  row.names = FALSE,
  na = ""
)


# -----------------------------------------------------------------------------
# 15.11 Optional official quarterly BCB debt series: SGS 3563
# -----------------------------------------------------------------------------
#
# Saved for future historical simulations and debt dynamics.
# It is NOT used to manufacture quarterly spread observations from annual data.
#

bcb3563_url <- paste0(
  "https://api.bcb.gov.br/dados/serie/bcdata.sgs.3563/dados?",
  "formato=json&dataInicial=01/01/1970&dataFinal=31/12/1980"
)


bcb_3563 <- NULL


try({
  
  tmp3563 <- tempfile(fileext = ".json")
  
  utils::download.file(
    bcb3563_url,
    tmp3563,
    method = "libcurl",
    mode = "wb",
    quiet = TRUE
  )
  
  raw3563 <- jsonlite::fromJSON(tmp3563)
  
  unlink(tmp3563)
  
  if (
    is.data.frame(raw3563) &&
    all(c("data", "valor") %in% names(raw3563))
  ) {
    
    bcb_3563 <- data.frame(
      date = as.Date(raw3563$data, format = "%d/%m/%Y"),
      registered_external_debt_usd_million = parse_bcb_value(raw3563$valor),
      stringsAsFactors = FALSE
    )
    
    bcb_3563$year <- as.integer(format(bcb_3563$date, "%Y"))
    month_num <- as.integer(format(bcb_3563$date, "%m"))
    bcb_3563$quarter_num <- as.integer((month_num - 1) / 3) + 1
    bcb_3563$quarter <- paste0(bcb_3563$year, "Q", bcb_3563$quarter_num)
    
    bcb_3563 <- bcb_3563[
      bcb_3563$year >= 1970 &
        bcb_3563$year <= 1980,
      ,
      drop = FALSE
    ]
    
    write.csv(
      bcb_3563,
      file.path(
        raw_debt_dir,
        "bcb_sgs3563_registered_external_debt_quarterly_1970_1980.csv"
      ),
      row.names = FALSE,
      na = ""
    )
  }
  
}, silent = TRUE)


# -----------------------------------------------------------------------------
# 15.12 Cross-era methodological note
# -----------------------------------------------------------------------------

cross_era_note <- c(
  
  "# Cross-era measurement architecture for Brazil's external financing wedge",
  
  "",
  
  "## Structural model object",
  
  "Keep the same equation across the research program:",
  
  "rp_t = phiD * d_t + eps_rp,t",
  
  "",
  
  "phiDD is removed from the baseline model.",
  
  "",
  
  "## Measurement principle",
  
  paste(
    "The theoretical object is kept constant while the best historical",
    "observable changes when Brazil's external financing regime changes."
  ),
  
  "",
  
  "## Paper I",
  
  paste(
    "Use the marginal syndicated-loan financing wedge:",
    "LIBOR minus TB3MS plus Brazil's contractual spread over LIBOR."
  ),
  
  "",
  
  "## Paper II",
  
  paste(
    "After the 1982 debt crisis, normal voluntary bank-loan spreads are not",
    "always valid market prices because restructuring and concerted lending",
    "can dominate. Use sovereign-bond spreads where available and secondary",
    "debt prices as stress evidence; do not manufacture a continuous spread",
    "from debt prices without a pricing model."
  ),
  
  "",
  
  "## Paper III",
  
  paste(
    "From the bond-market re-entry / Brady era onward, use market sovereign",
    "spreads such as EMBI and Brady/Eurobond spreads."
  ),
  
  "",
  
  "## Published effective-cost spread",
  
  paste(
    "Aiolfi-Catao-Timmermann / Cardoso-Fishlow Ext.Spread is retained as",
    "validation of the inherited-debt financing burden. It is not treated as",
    "the same object as the marginal syndicated-loan spread."
  )
)


writeLines(
  cross_era_note,
  file.path(
    docs_dir,
    "external_financing_cross_era_methodology.md"
  )
)



# =============================================================================
# 16. READING / CITATION LIST
# =============================================================================

reading_list <- data.frame(
  
  priority = c(
    1, 1, 1, 1, 2, 2, 2, 3
  ),
  
  source = c(
    "Serra (1982)",
    "Bresser-Pereira (1983)",
    "Batista (1987)",
    "Cardoso & Fishlow (1989)",
    "Bresser-Pereira (1987)",
    "Barbosa & Sánchez de la Cal (1992)",
    "World Bank, Macroeconomic crises, policies, and growth in Brazil 1964-90",
    "Banco Central do Brasil historical external-debt series documentation"
  ),
  
  why_read = c(
    paste(
      "Core historical chronology and Table 7 debt/reserve/current-account",
      "data; links debt dynamics to structural changes."
    ),
    paste(
      "Excellent 1970s interpretation plus independent BOP/debt tables;",
      "important for the first/second oil-shock narrative."
    ),
    paste(
      "Direct analysis of the II PND as a 1974-1979 growth-cum-debt",
      "external-adjustment strategy."
    ),
    paste(
      "Core macro treatment of Brazilian external debt;",
      "directly relevant to the DSGE debt block and historical simulations."
    ),
    paste(
      "Alternative broader debt concept and link between external finance,",
      "investment, interest payments, and real resource transfer."
    ),
    paste(
      "Institutional structure of Resolution 63 / Law 4131 and the",
      "fixed-to-floating-rate transition after 1973."
    ),
    paste(
      "Borrowing-cost evidence, LIBOR/prime spread discussion,",
      "and ex-post interest-cost construction."
    ),
    paste(
      "Official series definition and revisions; needed to document why",
      "the baseline uses a particular debt concept."
    )
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  reading_list,
  file.path(
    docs_dir,
    "external_debt_reading_citation_list.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 17. DATABASE README / METHODOLOGY NOTE
# =============================================================================

readme <- c(
  
  "# Brazil historical external-debt database",
  
  "",
  
  "## Purpose",
  
  paste(
    "This database is constructed before any DSGE debt/risk-premium",
    "parameter is chosen. It preserves competing historical debt concepts",
    "rather than silently reconciling them."
  ),
  
  "",
  
  "## Core empirical distinction",
  
  paste(
    "Registered external debt, gross external debt, net external debt,",
    "and broader total external debt are not treated as interchangeable."
  ),
  
  "",
  
  "## Candidate model mapping",
  
  paste(
    "The DSGE variable d_t is currently interpreted as a consolidated",
    "public-sector external-debt / net-external-liability state.",
    "The next stage must decide whether gross debt, gross debt minus reserves,",
    "or another historical concept is the best empirical counterpart."
  ),
  
  "",
  
  "## Risk premium",
  
  paste(
    "No synthetic risk-premium series is created in Script 008.",
    "Historical evidence indicates that much Brazilian borrowing after 1973",
    "used floating LIBOR/prime-linked loans with country-risk spreads.",
    "Interest-payment/debt ratios are stored only as diagnostics."
  ),
  
  "",
  
  "## Reproducibility",
  
  paste(
    "The official BCB SGS 3560 series is downloaded directly when internet",
    "access is available. Historical paper tables are manually transcribed",
    "into explicit source-specific data frames so they can be audited against",
    "the original published tables."
  ),
  
  "",
  
  "## Next step",
  
  paste(
    "Review the source comparison and definitions before selecting dbar or",
    "calibrating phiD. phiDD is not a live model parameter. Do not update the Dynare model from Script 008."
  ),
  
  ""
)

writeLines(
  readme,
  file.path(
    docs_dir,
    "external_debt_database_methodology.md"
  )
)

# =============================================================================
# 18. CONSOLE REPORT
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "SCRIPT 008: EXTERNAL-DEBT DATABASE BUILD COMPLETE\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nNo DSGE parameter has been estimated or calibrated.\n"
)

cat(
  "No single debt concept has been selected yet.\n"
)

cat(
  "\nBCB SGS 3560 observations downloaded: ",
  nrow(bcb_3560),
  "\n",
  sep = ""
)

cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "SERRA 1982 CORE TABLE, 1967-1980\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  serra_t7[
    serra_t7$year <= 1980,
    c(
      "year",
      "gross_external_debt_usd_million",
      "international_reserves_usd_million",
      "net_external_debt_usd_million",
      "current_account_deficit_usd_million",
      "net_interest_payments_usd_million",
      "debt_service_usd_million"
    )
  ],
  row.names = FALSE
)

cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "REPORTED DEBT/GDP, BRESSER-PEREIRA 1983\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  bresser_1983_t5[
    bresser_1983_t5$year <= 1980,
    c(
      "year",
      "external_debt_to_gdp_pct",
      "net_debt_to_exports_ratio",
      "terms_of_trade_index_1970_100"
    )
  ],
  row.names = FALSE
)

cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "CROSS-SOURCE DEBT COMPARISON\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  cross_source,
  row.names = FALSE
)

cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "DATA QUALITY FLAGS\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  quality,
  row.names = FALSE
)

cat(
  "\nFiles created:\n"
)

files_created <- c(
  file.path(raw_debt_dir, "source_registry.csv"),
  file.path(raw_debt_dir, "bcb_sgs3560_registered_external_debt_1964_1980.csv"),
  file.path(raw_debt_dir, "serra_1982_table7_external_debt.csv"),
  file.path(raw_debt_dir, "bresser_pereira_1983_table4_bop_debt.csv"),
  file.path(raw_debt_dir, "bresser_pereira_1983_table5_external_ratios.csv"),
  file.path(raw_debt_dir, "bresser_pereira_1987_table2_debt_resource_transfer.csv"),
  file.path(raw_debt_dir, "barbosa_sanchez_1992_table2_commercial_bank_debt.csv"),
  file.path(raw_debt_dir, "historical_external_debt_events.csv"),
  file.path(raw_debt_dir, "risk_premium_borrowing_cost_evidence.csv"),
  file.path(processed_dir, "bcb3560_historical_source_check.csv"),
  file.path(processed_dir, "external_debt_cross_source_comparison_1967_1980.csv"),
  file.path(processed_dir, "brazil_external_debt_master_1964_1980.csv"),
  file.path(processed_dir, "external_debt_data_quality_flags_1967_1980.csv"),
  file.path(docs_dir, "external_debt_reading_citation_list.csv"),
  file.path(docs_dir, "external_debt_database_methodology.md")
)


files_created <- c(
  files_created,
  file.path(processed_dir, "external_financing_cross_era_measurement_map.csv"),
  file.path(raw_debt_dir, "external_financing_cross_era_source_registry.csv"),
  file.path(raw_debt_dir, "bacha_diaz_alejandro_1982_table4_brazil_libor_spreads_1974_1980.csv"),
  file.path(raw_debt_dir, "sachs_fishlow_brazil_spread_over_libor_1974_1980.csv"),
  file.path(raw_debt_dir, "paper1_pre_shock_marginal_rp_anchor_1972.csv"),
  file.path(raw_debt_dir, "aiolfi_catao_timmermann_brazil_effective_external_cost_spread.csv"),
  file.path(raw_debt_dir, "edwards_brazil_sovereign_bond_spread_evidence_1980_1985.csv"),
  file.path(raw_debt_dir, "brazil_bank_restructuring_terms_1983_1986.csv"),
  file.path(raw_debt_dir, "brazil_secondary_market_external_debt_prices_1985_1989.csv"),
  file.path(raw_debt_dir, "future_brazil_market_spread_sources_1991_1994plus.csv"),
  file.path(docs_dir, "external_financing_cross_era_methodology.md")
)

if (!is.null(bcb_3563)) {
  files_created <- c(
    files_created,
    file.path(raw_debt_dir, "bcb_sgs3563_registered_external_debt_quarterly_1970_1980.csv")
  )
}


cat(
  "\\n------------------------------------------------------------\\n"
)

cat(
  "CROSS-ERA EXTERNAL-FINANCING MAP\\n"
)

cat(
  "------------------------------------------------------------\\n"
)

print(
  rp_cross_era_map,
  row.names = FALSE
)


cat(
  "\\n------------------------------------------------------------\\n"
)

cat(
  "PAPER-I MARGINAL LOAN DATA: BACHA-DIAZ-ALEJANDRO\\n"
)

cat(
  "------------------------------------------------------------\\n"
)

print(
  bacha_diaz_1982_t4,
  row.names = FALSE
)


for (f in files_created) {
  cat(" - ", f, "\n", sep = "")
}

cat(
  "\nIMPORTANT NEXT STEP\n"
)

cat(
  paste(
    "Review the source definitions and the cross-source comparison.",
    "Only after that should Script 08 decide the empirical counterpart to d_t,",
    "calculate dbar, calibrate phiD, and construct borrowing-cost proxies.\n"
  )
)

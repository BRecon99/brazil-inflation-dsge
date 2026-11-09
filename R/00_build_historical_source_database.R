# =============================================================================
# 00_build_historical_source_database.R
#
# Purpose:
#   Rebuild machine-readable historical Brazilian external-sector datasets from
#   version-controlled manual transcriptions of published IPEA / Serra tables.
#
# Important design choice:
#   We do NOT OCR the historical PDFs at runtime. OCR/table extraction from old
#   scanned documents is not stable enough for a reproducible research pipeline.
#   Instead, exact source-checked transcriptions live under data/raw and are
#   version controlled. This script validates and transforms them.
#
# This script DOES NOT modify:
#   - the Dynare .mod file
#   - docs/parameter_registry.csv
#   - docs/data_dictionary.csv
#   - docs/sources.md
#   - R/05_foreign_price_process.R
# =============================================================================

options(stringsAsFactors = FALSE)

raw_dir <- file.path("data", "raw", "historical_sources")
processed_dir <- file.path("data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

files_required <- c(
  "doellinger_1974_table_VI2_imports_by_use.csv",
  "ipea_1978_dossie_import_structure_1960_1977.csv",
  "serra_1982_table13_import_price_index.csv",
  "source_registry.csv"
)

missing_files <- files_required[!file.exists(file.path(raw_dir, files_required))]
if (length(missing_files) > 0) {
  stop(
    "Missing historical source transcription file(s): ",
    paste(missing_files, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# 1. Doellinger et al. (1974), Table VI.2
# -----------------------------------------------------------------------------

doel <- read.csv(
  file.path(raw_dir, "doellinger_1974_table_VI2_imports_by_use.csv"),
  check.names = FALSE
)

doel <- doel[order(doel$year), ]

if (!identical(doel$year, 1967:1972)) {
  stop("Doellinger Table VI.2 years are not exactly 1967-1972.")
}

required_doel <- c(
  "capital_goods_pct",
  "intermediate_goods_pct",
  "consumer_nondurable_pct",
  "consumer_durable_pct",
  "other_pct",
  "published_total_pct"
)
if (!all(required_doel %in% names(doel))) {
  stop("Doellinger transcription has unexpected columns.")
}

doel$consumer_goods_total_pct <-
  doel$consumer_nondurable_pct + doel$consumer_durable_pct

doel$computed_total_pct <-
  doel$capital_goods_pct +
  doel$intermediate_goods_pct +
  doel$consumer_nondurable_pct +
  doel$consumer_durable_pct +
  doel$other_pct

doel$total_gap_pct <- doel$computed_total_pct - doel$published_total_pct

if (any(abs(doel$total_gap_pct) > 0.11)) {
  stop("Doellinger Table VI.2 components fail the 100% sum check.")
}

doel$source_id <- "doellinger_1974_vi2"

write.csv(
  doel[, c(
    "year",
    "capital_goods_pct",
    "intermediate_goods_pct",
    "consumer_nondurable_pct",
    "consumer_durable_pct",
    "consumer_goods_total_pct",
    "other_pct",
    "computed_total_pct",
    "published_total_pct",
    "source_id"
  )],
  file.path(processed_dir, "brazil_import_composition_doellinger_1967_1972.csv"),
  row.names = FALSE
)

baseline <- doel[doel$year >= 1968 & doel$year <= 1972, ]

baseline_out <- data.frame(
  year = as.character(baseline$year),
  capital_goods_pct_of_imports = baseline$capital_goods_pct,
  intermediate_goods_pct_of_imports = baseline$intermediate_goods_pct,
  consumer_goods_pct_of_imports = baseline$consumer_goods_total_pct,
  other_pct_of_imports = baseline$other_pct,
  source_id = baseline$source_id
)

baseline_mean <- data.frame(
  year = "MEAN_1968_1972",
  capital_goods_pct_of_imports = mean(baseline$capital_goods_pct),
  intermediate_goods_pct_of_imports = mean(baseline$intermediate_goods_pct),
  consumer_goods_pct_of_imports = mean(baseline$consumer_goods_total_pct),
  other_pct_of_imports = mean(baseline$other_pct),
  source_id = "doellinger_1974_vi2"
)

baseline_out <- rbind(baseline_out, baseline_mean)

write.csv(
  baseline_out,
  file.path(processed_dir, "brazil_import_composition_baseline_1968_1972.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 2. IPEA/IPLAN (1978), Dossiê sobre Energia
# -----------------------------------------------------------------------------

dossie <- read.csv(
  file.path(raw_dir, "ipea_1978_dossie_import_structure_1960_1977.csv"),
  check.names = FALSE,
  na.strings = c("", "NA")
)

dossie <- dossie[order(dossie$year), ]

if (!identical(dossie$year, 1960:1977)) {
  stop("IPEA Dossiê years are not exactly 1960-1977.")
}

full <- dossie$comparability_flag == "full_categories"

dossie$computed_component_sum_pct <- NA_real_
dossie$computed_component_sum_pct[full] <-
  dossie$fuel_pct[full] +
  dossie$capital_goods_pct[full] +
  dossie$raw_materials_pct[full] +
  dossie$consumer_goods_pct[full]

dossie$sum_gap_pct <-
  dossie$computed_component_sum_pct - dossie$published_total_pct

dossie$sum_check_flag <- "not_applicable_aggregated"
dossie$sum_check_flag[full & abs(dossie$sum_gap_pct) <= 0.5] <- "within_0_5pp"
dossie$sum_check_flag[full & abs(dossie$sum_gap_pct) > 0.5] <-
  "published_components_do_not_sum_to_total"

# Do NOT stop on historical source inconsistencies. They are part of the source.
# We preserve them and flag them for the paper / robustness discussion.

write.csv(
  dossie,
  file.path(processed_dir, "brazil_import_structure_ipea_dossie_1960_1977.csv"),
  row.names = FALSE,
  na = ""
)

# -----------------------------------------------------------------------------
# 3. Serra (1982), Table 13
# -----------------------------------------------------------------------------

serra <- read.csv(
  file.path(raw_dir, "serra_1982_table13_import_price_index.csv"),
  check.names = FALSE
)

serra <- serra[order(serra$year), ]

if (!identical(serra$year, 1963:1981)) {
  stop("Serra Table 13 years are not exactly 1963-1981.")
}

price_cols <- grep("_price_index$", names(serra), value = TRUE)

if (length(price_cols) != 7) {
  stop("Expected exactly seven Serra import-price-index columns.")
}

# Base-year validation: Table 13 uses 1963 = 100.
base_row <- serra[serra$year == 1963, price_cols, drop = FALSE]
if (any(abs(as.numeric(unlist(base_row[1, ], use.names = FALSE)) - 100) > 1e-12)) {
  stop("Serra Table 13 base-year check failed: not all 1963 indexes equal 100.")
}

for (col in price_cols) {
  new_name <- paste0("log_inflation_", sub("_price_index$", "", col))
  serra[[new_name]] <- c(NA_real_, diff(log(serra[[col]])))
}

serra$usable_full_year_for_rhoF_validation <- serra$period_coverage == "full_year"

write.csv(
  serra,
  file.path(processed_dir, "brazil_import_price_serra_1963_1981.csv"),
  row.names = FALSE,
  na = ""
)

# -----------------------------------------------------------------------------
# 4. Build a long provenance-preserving database
# -----------------------------------------------------------------------------

long_parts <- list()
idx <- 1

add_long <- function(source_id, year, variable, original_label, value,
                     unit, frequency, period_coverage, role) {
  data.frame(
    source_id = source_id,
    year = year,
    variable = variable,
    original_label = original_label,
    value = value,
    unit = unit,
    frequency = frequency,
    period_coverage = period_coverage,
    role = role,
    stringsAsFactors = FALSE
  )
}

for (i in seq_len(nrow(doel))) {
  y <- doel$year[i]
  long_parts[[idx]] <- add_long("doellinger_1974_vi2", y, "capital_goods", "Bens de Capital", doel$capital_goods_pct[i], "percent_of_FOB_import_value", "annual", "full_year", "PRIMARY"); idx <- idx + 1
  long_parts[[idx]] <- add_long("doellinger_1974_vi2", y, "intermediate_goods", "Bens Intermediários", doel$intermediate_goods_pct[i], "percent_of_FOB_import_value", "annual", "full_year", "PRIMARY"); idx <- idx + 1
  long_parts[[idx]] <- add_long("doellinger_1974_vi2", y, "consumer_nondurable", "Bens de Consumo não-Durável", doel$consumer_nondurable_pct[i], "percent_of_FOB_import_value", "annual", "full_year", "PRIMARY"); idx <- idx + 1
  long_parts[[idx]] <- add_long("doellinger_1974_vi2", y, "consumer_durable", "Bens de Consumo Durável", doel$consumer_durable_pct[i], "percent_of_FOB_import_value", "annual", "full_year", "PRIMARY"); idx <- idx + 1
  long_parts[[idx]] <- add_long("doellinger_1974_vi2", y, "other", "Outros", doel$other_pct[i], "percent_of_FOB_import_value", "annual", "full_year", "PRIMARY"); idx <- idx + 1
}

for (i in seq_len(nrow(dossie))) {
  y <- dossie$year[i]
  candidates <- list(
    c("fuel", "Combustíveis", "fuel_pct"),
    c("capital_goods", "Bens de Capital", "capital_goods_pct"),
    c("raw_materials", "Matérias Primas", "raw_materials_pct"),
    c("consumer_goods", "Bens de Consumo", "consumer_goods_pct"),
    c("raw_materials_plus_consumer", "Matérias Primas e Bens de Consumo", "combined_raw_materials_consumer_pct"),
    c("capital_raw_materials_consumer_combined", "Bens de Capital, Matérias Primas e Bens de Consumo", "combined_capital_raw_materials_consumer_pct")
  )
  for (z in candidates) {
    val <- dossie[[z[3]]][i]
    if (!is.na(val)) {
      long_parts[[idx]] <- add_long("ipea_1978_dossie_imports", y, z[1], z[2], val, "percent_of_FOB_import_value", "annual", "full_year", "SECONDARY")
      idx <- idx + 1
    }
  }
}

serra_map <- c(
  general_import_price_index = "Geral",
  crude_oil_price_index = "Petróleo em Bruto",
  wheat_price_index = "Trigo em Grãos",
  nonindustrialized_products_price_index = "Produtos não Industrializados",
  capital_goods_price_index = "Bens de Capital",
  intermediate_goods_price_index = "Bens de Consumo Intermed.",
  final_consumer_goods_price_index = "Bens de Consumo Final"
)

for (i in seq_len(nrow(serra))) {
  for (col in names(serra_map)) {
    variable_name <- paste0("import_price_", sub("_price_index$", "", col))
    long_parts[[idx]] <- add_long(
      "serra_1982_table13",
      serra$year[i],
      variable_name,
      serra_map[[col]],
      serra[[col]][i],
      "price_index_1963_eq_100",
      "annual",
      serra$period_coverage[i],
      "PRIMARY_VALIDATION"
    )
    idx <- idx + 1
  }
}

long_db <- do.call(rbind, long_parts)
write.csv(
  long_db,
  file.path(processed_dir, "historical_external_sector_source_database_long.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 5. Validation report
# -----------------------------------------------------------------------------

validation <- data.frame(
  source_id = character(0),
  year = integer(0),
  computed_component_sum_pct = numeric(0),
  published_total_pct = numeric(0),
  gap_pct = numeric(0),
  validation_status = character(0),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(doel))) {
  validation <- rbind(validation, data.frame(
    source_id = "doellinger_1974_vi2",
    year = doel$year[i],
    computed_component_sum_pct = doel$computed_total_pct[i],
    published_total_pct = doel$published_total_pct[i],
    gap_pct = doel$total_gap_pct[i],
    validation_status = ifelse(abs(doel$total_gap_pct[i]) <= 0.11, "PASS", "REVIEW")
  ))
}

for (i in which(full)) {
  gap <- dossie$sum_gap_pct[i]
  validation <- rbind(validation, data.frame(
    source_id = "ipea_1978_dossie_imports",
    year = dossie$year[i],
    computed_component_sum_pct = dossie$computed_component_sum_pct[i],
    published_total_pct = dossie$published_total_pct[i],
    gap_pct = gap,
    validation_status = ifelse(
      abs(gap) <= 0.5,
      "PASS_WITHIN_0_5PP",
      "SOURCE_INCONSISTENCY_RETAIN_AS_PUBLISHED"
    )
  ))
}

write.csv(
  validation,
  file.path(processed_dir, "historical_source_validation.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 6. Console report
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("HISTORICAL SOURCE DATABASE BUILD COMPLETE\n")
cat("============================================================\n\n")

cat("Doellinger 1968-1972 mean shares of total FOB imports:\n")
cat(sprintf("  Capital goods:       %.2f%%\n", mean(baseline$capital_goods_pct)))
cat(sprintf("  Intermediate goods:  %.2f%%\n", mean(baseline$intermediate_goods_pct)))
cat(sprintf("  Consumer goods:      %.2f%%\n", mean(baseline$consumer_goods_total_pct)))
cat(sprintf("  Other:               %.2f%%\n\n", mean(baseline$other_pct)))

bad_dossie <- validation[
  validation$source_id == "ipea_1978_dossie_imports" &
    validation$validation_status == "SOURCE_INCONSISTENCY_RETAIN_AS_PUBLISHED",
]

if (nrow(bad_dossie) > 0) {
  cat("IPEA Dossiê rows with published component-sum inconsistencies:\n")
  print(bad_dossie[, c("year", "computed_component_sum_pct", "published_total_pct", "gap_pct")], row.names = FALSE)
  cat("These are retained exactly as published and are NOT silently corrected.\n\n")
}

cat("Serra Table 13 coverage: ", min(serra$year), "-", max(serra$year), "\n", sep = "")
cat("1981 coverage: Jan-Oct; exclude it from full-year rhoF validation.\n\n")

cat("Files written:\n")
cat(" - data/processed/brazil_import_composition_doellinger_1967_1972.csv\n")
cat(" - data/processed/brazil_import_composition_baseline_1968_1972.csv\n")
cat(" - data/processed/brazil_import_structure_ipea_dossie_1960_1977.csv\n")
cat(" - data/processed/brazil_import_price_serra_1963_1981.csv\n")
cat(" - data/processed/historical_external_sector_source_database_long.csv\n")
cat(" - data/processed/historical_source_validation.csv\n")

# Historical external-sector source database

## Purpose

This folder turns historical tables from IPEA and Serra into version-controlled machine-readable data while preserving the distinction between **published evidence** and **derived variables**. No historical category is silently harmonized into a different concept.

## Why the raw CSV files are manual transcriptions

The source documents are historical PDFs / rendered tables. Fully automated OCR/table extraction would be less reproducible because layout recognition can change across software versions and can silently alter commas, decimal points, footnotes, or column alignment. The reproducible object is therefore a **manual, source-checked transcription** stored in Git, accompanied by exact table locators, URLs, access dates, and validation tests.

The processed files are generated from those raw transcriptions by `R/00_build_historical_source_database.R`.

## Source hierarchy

1. **Doellinger et al. (1974), Table VI.2 — PRIMARY for pre-oil import composition.** It directly reports import composition by use for 1967-1972, including capital goods, intermediate goods, nondurable consumer goods, durable consumer goods, and other goods. This is the cleanest source for the 1968-1972 steady-state window.
2. **Serra (1982), Table 13 — PRIMARY VALIDATION for rhoF.** It reports Brazil-specific import-price indexes by category from 1963, including intermediate goods, final consumer goods, capital goods, and crude oil. It is annual, so it validates the quarterly B639 proxy rather than replacing the quarterly rhoF estimator.
3. **IPEA/IPLAN (1978), Dossiê sobre Energia — SECONDARY cross-check.** It gives a longer import-structure series, 1960-1977, but its category definitions differ from Doellinger Table VI.2, 1976-77 categories are aggregated, and several published rows do not sum exactly to 100. Those inconsistencies are deliberately retained and flagged.

## Important methodological rule

Do not average or splice the Doellinger and Dossiê series as though they were the same classification. Keep them source-specific. The Doellinger series is the baseline for `cf_y` / `z_y` evidence; the Dossiê is a robustness / historical-context source.

## Files

### Raw transcriptions
- `data/raw/historical_sources/doellinger_1974_table_VI2_imports_by_use.csv`
- `data/raw/historical_sources/ipea_1978_dossie_import_structure_1960_1977.csv`
- `data/raw/historical_sources/serra_1982_table13_import_price_index.csv`
- `data/raw/historical_sources/source_registry.csv`
- `data/raw/historical_sources/checksums_sha256.csv`

### Generated processed data
- `data/processed/brazil_import_composition_doellinger_1967_1972.csv`
- `data/processed/brazil_import_composition_baseline_1968_1972.csv`
- `data/processed/brazil_import_structure_ipea_dossie_1960_1977.csv`
- `data/processed/brazil_import_price_serra_1963_1981.csv`
- `data/processed/historical_external_sector_source_database_long.csv`
- `data/processed/historical_source_validation.csv`

## Baseline values visible from Doellinger, 1968-1972

The arithmetic mean of the annual shares of total FOB imports is:

- capital goods: **37.90%**
- intermediate goods: **47.62%**
- consumer goods (durable + nondurable): **13.70%**
- other: **0.78%**

These are **shares of total imports**, not shares of GDP. To calibrate `cf_y` or `z_y`, combine them with the annual total-import-to-GDP ratio using consistent nominal valuation.

## rhoF use

`brazil_import_price_serra_1963_1981.csv` contains both the published indexes and log annual inflation computed as `log(P_t) - log(P_{t-1})`. Script 05 should use the intermediate-goods series as the principal Brazil-specific annual validation target for the quarterly B639 proxy. 1981 is Jan-Oct and should be excluded from full-year validation.

## Rebuilding

From the repository root in RStudio:

```r
source("R/00_build_historical_source_database.R")
```

The script only reads the version-controlled transcriptions and rewrites the processed files. It does not change the Dynare model, parameter registry, or existing data dictionary. Suggested dictionary rows are provided separately in `docs/data_dictionary_additions.csv`.

## Corrections

If a source value is corrected, edit only the corresponding raw transcription, document the reason in `source_registry.csv` or the Git commit, rerun the build script, and commit both the raw correction and regenerated outputs.

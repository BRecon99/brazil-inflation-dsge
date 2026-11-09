# Integration into the existing repository

Copy the contents of this package into the repository root, preserving paths. It intentionally does **not** overwrite `docs/data_dictionary.csv`, `docs/sources.md`, `docs/parameter_registry.csv`, or the current Script 05.

First run:

```r
source("R/00_build_historical_source_database.R")
```

Then inspect:

```r
read.csv("data/processed/brazil_import_composition_baseline_1968_1972.csv")
read.csv("data/processed/brazil_import_price_serra_1963_1981.csv")
read.csv("data/processed/historical_source_validation.csv")
```

After that, revise `R/05_foreign_price_process.R` so the Brazilian intermediate-goods price index in `brazil_import_price_serra_1963_1981.csv` becomes the primary annual validation target. The quarterly B639 AR(1) remains the estimator of `rhoF`.

For the later trade-share script, use the Doellinger 1968-1972 baseline composition and combine it with your annual total imports/GDP series.

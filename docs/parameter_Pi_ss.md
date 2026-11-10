# Pi_ss calibration

## Parameter

- Dynare/model name: `Pi_ss`
- Derived inverse: `invPi = 1 / Pi_ss`

## Data

- Source series: IPEA/FGV IGPC
- IPEA series code: `GAMMA12_IGPCMTB12`
- Raw file: `data/raw/IGPC_ipeadata.csv`
- Processed quarterly file: `data/processed/brazil_igpc_quarterly.csv`

## Baseline

- Sample: 1968Q1-1972Q4
- Pi_ss is the geometric mean of quarterly gross IGPC inflation.

- Pi_ss = 1.048995251092
- invPi = 0.953293162156
- Quarterly inflation = 4.899525%
- Annualized compound inflation = 21.086043%

## Robustness

- 1967Q1-1972Q4
- 1964Q1-1972Q4

## Pipeline rule

This script does not write `docs/parameter_registry.csv`.
Script 99 is the sole registry writer.

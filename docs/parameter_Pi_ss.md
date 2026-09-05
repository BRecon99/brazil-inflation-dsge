# Parameter Decision: Pi_ss

## Parameter definition

`Pi_ss` is gross quarterly steady-state inflation.

The related parameter `invPi` is defined mechanically as:

```text
invPi = 1 / Pi_ss
```

## Classification

`Pi_ss`: Calibrated.

`invPi`: Fixed / derived.

## Data source

Historical IGPC-Mtb consumer price index.

Series code: `GAMMA12_IGPCMTB12`.

Raw file used by the repository:

`data/raw/IGPC_ipeadata.csv`

The raw variable is a monthly price-index level, not an inflation rate.

## Transformation

Monthly gross inflation is constructed as:

```text
Pi_m,t = P_t / P_(t-1)
```

Quarterly gross inflation is obtained by compounding monthly gross inflation:

```text
Pi_q = Pi_m,1 * Pi_m,2 * Pi_m,3
```

The steady-state value is calibrated using the geometric mean:

```text
Pi_ss = exp(mean(log(Pi_q)))
```

## Baseline window

1968Q1-1972Q4.

This window represents the post-PAEG, pre-first-oil-shock inflation regime.

1973 is excluded because it marks the beginning of the external shock period
that the model is intended to explain.

## Robustness windows

- 1967Q1-1972Q4
- 1964Q1-1972Q4

The 1948Q1-1972Q4 calculation is retained only as a descriptive historical
comparison and is not the preferred steady-state calibration.

## Final calibration

`Pi_ss = 1.04899525109`

`invPi = 0.953293162156`

Implied quarterly steady-state inflation: 4.8995%.

Implied annual compounded steady-state inflation: 21.086%.

## Dynare implementation

```text
Pi_ss = 1.04899525109;
invPi = 1/Pi_ss;
```

## Reproducibility

The complete transformation and calibration are reproduced by:

`R/03_brazil_inflation_steady_state.R`

Processed quarterly data:

`data/processed/brazil_igpc_quarterly.csv`

Calibration results:

`output/estimates/Pi_ss_calibration.csv`

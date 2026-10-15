# Parameter Decision: rhoF

## Model parameter

`rhoF` governs persistence of foreign-price inflation:

```text
pif_t = rhoF * pif_(t-1) + eps_pf,t
```

## Economic interpretation

The model treats imported goods as intermediate production inputs. There is no capital stock in the model.

The empirical foreign-price proxy therefore targets prices of industrial supplies and materials rather than consumer or capital goods.

## Baseline empirical proxy

`B639RG3Q086SBEA`

U.S. exports of goods: industrial supplies and materials, chain-type price index.

## Historical validation

The quarterly proxy is validated against Brazil's annual historical import implicit-price index.

The Brazilian annual series is not interpolated to quarterly frequency and is not used directly to estimate rhoF.

## Transformation

Quarterly foreign-price inflation is:

```text
piF_t = log(P_t) - log(P_(t-1))
```

## Baseline estimation

The empirical AR(1) is:

```text
piF_t = a + rhoF * piF_(t-1) + u_t
```

Baseline regression sample:

`1967Q3-1980Q4`

## Estimate

`rhoF = 0.723969948147`

Conventional OLS SE: 0.09475676

HC1 robust SE: 0.10080754

Newey-West SE: 0.12046136

Estimated quarterly shock SD: 0.024292183

Implied half-life: 2.146 quarters.

Residual-bootstrap 95% interval: [0.4295, 0.8316].

## Robustness

- B686: durable industrial supplies/materials.
- B687: nondurable industrial supplies/materials.
- B640: capital-goods diagnostic only.
- Alternative sample ending in 1978Q4.
- Pre-1973 and 1973-1980 regime diagnostics.
- Annual-average and Q4/Q4 proxy validation.
- Pearson and Spearman validation correlations.
- AR(1) versus AR(2) diagnostic.
- ADF and KPSS stationarity diagnostics.

## Reproducibility

Estimation script:

`R/05_foreign_price_process.R`

Processed quarterly data:

`data/processed/foreign_price_quarterly.csv`

Historical validation data:

`data/processed/foreign_price_annual_validation.csv`

Main estimation output:

`output/estimates/rhoF_ar1.csv`

Proxy-validation output:

`output/estimates/rhoF_proxy_validation.csv`

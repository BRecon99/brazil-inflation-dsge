# Parameter rhoF

## Model equation

`pif_t = rhoF * pif_{t-1} + eps_pf,t`

## Empirical interpretation

The quarterly persistence parameter is estimated from the BEA/FRED B639 U.S. export-price index for industrial supplies and materials. The observable is quarterly log inflation, not the price-index level.

Serra (1982), Table 13 provides Brazil-specific annual import-price indexes. The intermediate-goods series is used as the primary historical validation target because it is the closest empirical counterpart to the model's imported-intermediate-input price concept.

The Brazilian annual series is not used directly to estimate the quarterly Dynare rhoF because temporal aggregation changes the stochastic process. The annual AR coefficient is therefore treated only as a diagnostic.

## Baseline estimate

- Sample: 1967Q3-1980Q4
- rhoF: 0.723969948147
- OLS SE: 0.09475676
- HC1 robust SE: 0.10080754
- Newey-West SE: 0.12046136
- eps_pf residual shock SD: 0.024292183316
- Half-life: 2.146 quarters
- Residual-bootstrap 95% CI: [0.4189, 0.8367]

## Primary historical proxy validation: B639 vs Serra intermediate goods

- 1968-1972: n= 5, Pearson=0.7833, Spearman=0.6000
- 1973-1980: n= 8, Pearson=0.9264, Spearman=0.8810
- 1968-1980: n=13, Pearson=0.9396, Spearman=0.8187

## Robustness

The script reports alternative samples, durable and nondurable industrial-supply proxies, a capital-goods diagnostic, an optional broad-export benchmark, ADF and KPSS stationarity diagnostics, Ljung-Box residual diagnostics, ACF/PACF, AR(1) versus AR(2) AIC/BIC, and a residual bootstrap.

## Dynare mapping

`rhoF = 0.723969948147;`
`stderr eps_pf = 0.024292183316;`

## Source hierarchy

1. Quarterly estimator: BEA/FRED B639 industrial supplies and materials price index.
2. Primary historical validation: Serra (1982), Table 13, Brazilian intermediate-goods import-price index.
3. Secondary historical validation: Serra (1982), Table 13, general Brazilian import-price index.
4. Proxy robustness: B686 and B687.
5. Negative/control diagnostic: B640 capital goods.
6. Optional broad benchmark: broad U.S. export-price series.


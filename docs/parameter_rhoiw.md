# Parameter Decision: World Interest-Rate Persistence

## Parameter

**Dynare name:** `rhoiw`  
**Symbol:** `rho_i*`  
**Classification:** Estimated outside the DSGE; then fixed during structural DSGE estimation  
**Model equation:**

`i_t^* = rho_i* i_(t-1)^* + epsilon_(i*,t)`

## Economic meaning

`rho_i*` measures how persistent deviations in global financing conditions are.
A value near zero means the world-rate shock fades quickly. A value near one
means deviations persist strongly from quarter to quarter.

## Data

- Raw series: FRED `TB3MS`
- Raw frequency: monthly
- Raw units: annualized percent
- Local raw file: `data/raw/TB3MS.csv`
- Historical raw window: January 1964–December 1980

## Transformation

1. Average the three monthly observations within each quarter.
2. Convert the annual percentage rate to an effective quarterly decimal:
   `(1 + annual_percent/100)^(1/4) - 1`.
3. For the recommended baseline, estimate the AR(1) on 1964Q1–1972Q4.

## Method

OLS regression:

`r_t = a + rho*r_(t-1) + u_t`

The implied long-run quarterly rate is:

`a / (1-rho)`.

## Result

- Recommended estimate: `0.8629051575`
- Standard error: `0.0809055829`
- Innovation standard deviation: `0.0012977727`
- Implied long-run quarterly rate: `0.0125217283`
- Sample: `1964Q1-1972Q4`

## Robustness

See `output/estimates/world_interest_rate_ar1.csv` for:
- 1964Q1–1978Q4
- 1964Q1–1980Q4

The longer samples produce higher persistence because the late-1970s/1980
global rate shift becomes part of the fitted process.

## Dynare implication

The frozen baseline currently contains:

`rhoiw = 0.85;`

Do not overwrite the frozen baseline. In the future estimation model, the
first-stage value can be entered approximately as:

`rhoiw = 0.862905;`

The shock standard deviation can also inform the future `stderr` assigned to
`eps_iw`, subject to the final measurement convention.

# External debt and external-financing wedge: final Paper-I methodology

## Structural equation
rp_t = phiD*d_t + eps_rp,t

The previous phiDD term has been removed from the live model architecture.

## dbar
dbar is calibrated as mean net external debt/GDP over 1970-1972. Gross debt/GDP is retained as robustness.

## phiD
phiD is calibrated to 0.001 at quarterly frequency. Its role is stationarity and long-run debt discipline. It is not estimated from the short Brazilian 1970s spread sample.

## Paper-I historical rp
The historical marginal financing wedge is constructed as (LIBOR - TB3MS) + Brazil's contractual spread over LIBOR. This ensures TB3MS + rp equals the historical marginal syndicated-loan rate.

## 1972 anchor
The 1972 spread is an approximate mid-year observation and is used only as a pre-shock normalization / sensitivity anchor.

## eps_rp
Observed financing-wedge movements relative to the 1972 anchor are decomposed into the small structural debt component phiD*d_gap and an implied exogenous financing-wedge component eps_rp. The annual history is not falsely expanded into quarterly observations in this script.

## Cross-paper continuity
The equation remains unchanged in Papers II and III. Only the historical observable used to discipline eps_rp changes with Brazil's financing regime: bank-loan margins, debt-crisis bond/secondary market evidence, then EMBI/Brady/Eurobond spreads.

# Script 08: external debt and risk-premium parameter methodology

## Baseline dbar
Baseline dbar is the arithmetic mean of net external debt/GDP over 1970-1972. Net debt/GDP is constructed as reported gross debt/GDP multiplied by net debt divided by gross debt.

## Risk-premium proxy
The baseline effective external borrowing rate equals annual net interest payments divided by average gross external debt. The annual rate and the annual average TB3MS world rate are each converted to effective quarterly rates; their difference is the risk-premium wedge proxy.

## phiD and phiDD
The baseline regression uses 1971-1980 annual observations and estimates the quarterly-equivalent risk-premium gap on the net debt/GDP gap and the annual change in net debt/GDP, without an intercept.

## Identification warning
The sample is very small and contemporaneous external debt and financing costs may be jointly determined. The coefficients are therefore treated as historically disciplined reduced-form calibration candidates rather than clean causal structural estimates.

## Shock volatility
The annual regression residual standard deviation is not used directly as the quarterly eps_rp shock standard deviation.

## Domestic debt and seigniorage
bbar and seigniorage are not estimated here. They require distinct historical domestic-debt and monetary-base/fiscal-financing data.

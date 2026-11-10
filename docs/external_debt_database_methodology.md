# Brazil historical external-debt database

## Purpose
This database is constructed before any DSGE debt/risk-premium parameter is chosen. It preserves competing historical debt concepts rather than silently reconciling them.

## Core empirical distinction
Registered external debt, gross external debt, net external debt, and broader total external debt are not treated as interchangeable.

## Candidate model mapping
The DSGE variable d_t is currently interpreted as a consolidated public-sector external-debt / net-external-liability state. The next stage must decide whether gross debt, gross debt minus reserves, or another historical concept is the best empirical counterpart.

## Risk premium
No synthetic risk-premium series is created in Script 008. Historical evidence indicates that much Brazilian borrowing after 1973 used floating LIBOR/prime-linked loans with country-risk spreads. Interest-payment/debt ratios are stored only as diagnostics.

## Reproducibility
The official BCB SGS 3560 series is downloaded directly when internet access is available. Historical paper tables are manually transcribed into explicit source-specific data frames so they can be audited against the original published tables.

## Next step
Review the source comparison and definitions before selecting dbar or calibrating phiD. phiDD is not a live model parameter. Do not update the Dynare model from Script 008.


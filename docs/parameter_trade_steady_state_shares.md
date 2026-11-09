# Trade steady-state shares

## Baseline sample

1968-1972.

## Source mapping

- `x_y` uses `Exportacao de bens e servicos / Produto Interno Bruto` from IBGE historical national accounts, sheet I.2.1.
- `M/Y` uses `Menos: Importacao de bens e servicos / Produto Interno Bruto` from the same table and valuation system.
- `cf_y = (M/Y) x consumer-goods import share` from Doellinger et al. (1974).
- `z_y = (M/Y) x intermediate-goods import share` from Doellinger et al. (1974).

## Candidate values

- x_y = 0.066856
- cf_y = 0.010458
- z_y = 0.035854
- total M/Y diagnostic = 0.075881
- capital-import bridge diagnostic = 0.028969
- bridge residual = 0.029569

## Statistical treatment

These are accounting calibrations rather than dynamic-process estimates. The baseline is the arithmetic mean of annual ratios. Median, annual standard deviation, min/max, and inclusion of 1967 are reported as descriptive robustness checks.

## Caveat

Doellinger reports merchandise-import composition, while national-accounts imports are a broader aggregate. Therefore cf_y and z_y are calibration bridges, not exact accounting identities.
Capital-goods imports remain explicit diagnostics and are not silently reclassified as imported intermediate inputs.

## Derived cH_y

cH_y = 1 - x_y - g_y - ig_y = 0.781299.


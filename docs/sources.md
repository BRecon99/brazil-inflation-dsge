# Sources

This file is a human-readable source log. The machine-readable links between
parameters/data and sources belong in the CSV registries.

## World interest rate

- Series: FRED TB3MS — 3-Month Treasury Bill Secondary Market Rate, Discount Basis
- Local raw file: `data/raw/TB3MS.csv`
- Role: proxy for global/base financing conditions `i_t^*`
- First-stage sample: 1964Q1–1972Q4
- Transformation: monthly average to quarterly; annual percent to effective quarterly decimal


## Brazilian consumer inflation

 - índice (jan. 1967 = 100) - INATIVA - - - Outras fontes, inclusive compilação de vários autores - G - 
- Institution: Ipea
- Series: IGPC-Mtb - Índice Geral de Preços ao Consumidor
- Series code: GAMMA12_IGPCMTB12
- Frequency: Monthly
- Raw unit: [INDEX LEVEL / MONTHLY % CHANGE]
- First observation: [DATE]
- Local file: data/raw/IPC_FGV.csv
- Download date: YYYY-MM-DD
- Source URL: [SOURCE]
- Geographic coverage during 1968–1972: [VERIFY FROM METADATA]
- Purpose: Calibration of Pi_ss and inflation observable
- Redistribution: [VERIFY]

## Future sources

Add one subsection per raw dataset. Record:
- official series name;
- institution;
- series code;
- direct source page;
- date downloaded;
- license/redistribution note if relevant;
- any historical coverage caveat.

<!-- BEGIN RHO_F SOURCES -->

## Foreign-price process (`rhoF`)

- `B639RG3Q086SBEA`: BEA/FRED quarterly export price index for industrial supplies and materials. Baseline foreign-price proxy.

- `B686RG3Q086SBEA`: durable industrial supplies/materials. Proxy robustness.

- `B687RG3Q086SBEA`: nondurable industrial supplies/materials. Proxy robustness.

- `B640RG3Q086SBEA`: capital goods except automotive. Diagnostic only because the DSGE does not contain capital.

- `07_setor_externo_1901_00.xls`: historical Brazil external-sector dataset containing the annual import implicit-price index. Used for proxy validation.

<!-- END RHO_F SOURCES -->

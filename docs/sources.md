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
## rhoF — foreign intermediate-input price persistence

- **Quarterly estimator:** BEA/FRED B639RG3Q086SBEA, U.S. exports of industrial supplies and materials, chain-type price index.
- **Proxy robustness:** BEA/FRED B686RG3Q086SBEA and B687RG3Q086SBEA.
- **Control diagnostic:** BEA/FRED B640RG3Q086SBEA, capital goods except automotive.
- **Primary Brazilian historical validation:** Serra, José (1982), Table 13, annual Brazilian import-price index for intermediate goods.
- **Secondary Brazilian validation:** Serra (1982), Table 13, general import-price index.
- Serra's table reports 1963=100 indexes and identifies Conjuntura Econômica and Mendonça de Barros (1981) as underlying sources; 1981 is Jan-Oct and is excluded from full-year validation.

Methodological rule: Serra annual data validate the quarterly B639 proxy but do not replace the quarterly rhoF estimator.
<!-- END RHO_F SOURCES -->

# Parameter Research Workflow

Use this document every time you start a new parameter.

## A. Before touching data

1. Find the parameter in `docs/parameter_registry.csv`.
2. Read the exact model equation in which it appears.
3. State in one sentence what the parameter means economically.
4. Confirm whether it is:
   - Estimated
   - Calibrated
   - Fixed / derived
5. Decide whether it is estimated:
   - outside the DSGE from an observable exogenous process, or
   - jointly inside Dynare as a structural parameter.

## B. If ESTIMATED outside Dynare

Create a numbered R script:

`R/NN_parameter_name.R`

The script must:
1. read only from `data/raw/` or earlier machine-generated processed files;
2. document the original units;
3. document every transformation;
4. state the estimation sample;
5. estimate the parameter;
6. report uncertainty;
7. save residual/shock volatility if relevant;
8. save robustness estimates;
9. write a small CSV into `output/estimates/`;
10. update the parameter registry only through code.

## C. If CALIBRATED

Create an R script whenever the number comes from data.

The script must save:
- historical sample;
- raw ratio/series;
- calculation;
- baseline value;
- alternative windows/sensitivity values.

If the calibration comes from literature rather than data, document:
- source;
- page/table/equation;
- value in source;
- why it maps into this model;
- sensitivity interval.

## D. If FIXED / DERIVED

Do not search for an independent empirical estimate.

Document:
1. formula;
2. primitive parameters;
3. resulting numerical value;
4. script or model line that recalculates it.

## E. Before changing Dynare

The registry row must contain:
- value;
- status;
- source/method;
- sample if applicable;
- uncertainty if estimated;
- notes.

Then copy the value into a NEW estimation model version. Never overwrite the frozen baseline.

## F. Commit discipline

A useful commit pattern is:

- `Add raw world interest rate data`
- `Estimate world rate persistence`
- `Document rhoiw parameter decision`
- `Calibrate pre-1973 inflation steady state`
- `Add external price dataset`

Small commits are easier to review and reproduce than one giant commit.

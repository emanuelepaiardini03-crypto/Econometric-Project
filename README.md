# Econometric-Project
Forecasting on France GDP
The target variable is year-on-year real GDP growth in France, measured at quarterly frequency. The main script, [`May22_metrics_II.R`](May22_metrics_II.R), performs the following steps:
1. Imports macroeconomic and financial series from local CSV and Excel exports.
2. Converts monthly observations to quarterly averages and aligns series by quarter.
3. Constructs growth rates and lagged variables, and examines descriptive statistics and correlations.
4. Estimates nine regression specifications, including autoregressive distributed lag (ADL) models.
5. Selects a preferred specification using BIC and checks model diagnostics.
6. Compares fixed-coefficient and expanding-window predictions with AR(2) and VAR(2) forecasts.
7. Exports forecast results and explores a COVID-period dummy specification.

# Bank of Canada Governor's Challenge: Impulse Response Analysis in R

R code from our team's work in the Bank of Canada Governor's Challenge (September to November 2024, York University). The code explores how Canadian inflation, real GDP and unemployment respond to an interest rate shock using local projections, as part of the analysis behind our monetary policy recommendation. It is a working version of that analysis, shared for demonstration purposes (see the note at the end).

## Project summary

The Governor's Challenge asks student teams to assess Canadian economic conditions, build a macroeconomic outlook, and defend a monetary policy recommendation before a panel of judges. Our team used **local linear impulse response functions** to trace the dynamic effect of an interest rate shock on CPI, real GDP and unemployment over a 12-quarter horizon, and presented our recommendations to the judges. This repository contains working scripts from that analysis, not the final version of the code or data we used.

## What this code demonstrates

- **Time-series estimation:** linear local projections with `lp_lin` from the `lpirfs` package, including endogenous and exogenous lags, additional controls, and 95% confidence bands.
- **Data preparation:** cleaning and merging monthly and quarterly macroeconomic series with `tidyverse` and `lubridate`, aggregating monthly data to quarters, and constructing lags, leads, growth rates and year-over-year changes.
- **Specification choices:** separate models for each outcome with the other variables as exogenous regressors, controls for financial-market and commodity-price changes and the exchange rate, and robust regression (`lm_robust`) as a complement.
- **Visualization:** impulse response plots with confidence ribbons built in `ggplot2`.

## Repository contents

Despite their names, `Final Code.R` and `Finaldataset.csv` are working versions, not the final versions used in the competition.

| File | Description |
|---|---|
| `Final Code.R` | Working script that estimates local projections of CPI, real GDP and unemployment to an interest rate shock and plots the impulse responses with confidence bands. Reads `BOCdatatrim.csv`. |
| `Updated Dataset Projections.R` | Working script that builds the quarterly dataset from raw series (money supply, exchange rates, real GDP, CPI) from 1996 onward, and runs local projections of CPI as an illustration of inflation projections for 2024. |
| `BOCdatatrim.csv` | Trimmed dataset read by `Final Code.R`. |
| `Finaldataset.csv` | Assembled quarterly dataset: CPI, unemployment, interest rate, real GDP, money growth, exchange rate, lags and leads, and changes in the VIX, a commodity index, the S&P 500 and crude oil. |
| `Data with Crude Oil.csv` | Version of the estimation dataset with a crude oil price change series added. |

## Data

Quarterly Canadian series covering 1996 Q1 to 2024 Q2 (114 observations in the estimation dataset): CPI, unemployment rate, interest rate, real GDP, money growth rate and exchange rate, plus changes in the VIX, a commodity index, the S&P 500 and crude oil prices.

> Data sources: Statistics Canada, the Bank of Canada, FRED (Federal Reserve Bank of St. Louis) and Yahoo Finance.

## Method (as specified in these scripts)

- **Estimator:** linear local projections (`lp_lin`, `lpirfs`)
- **Shock:** interest rate, scaled to basis points
- **Outcomes:** CPI, real GDP and unemployment, each in its own model with the other two variables as exogenous regressors
- **Controls:** lagged changes in the VIX, commodity prices and the S&P 500, plus the exchange rate
- **Specification:** 4 lags of the endogenous and exogenous variables, 12-quarter horizon, 95% confidence bands

## Running the code

1. Install the R packages `tidyverse`, `lubridate`, `lpirfs` and `ggview`.
2. Update the file path near the top of each script to the folder containing the CSV files.
3. Run `Final Code.R` to run the impulse response estimation and generate the plots.

## Note

This repository is shared for demonstration purposes and does not present the final results or the final version of the code and data used in the competition. These are working versions of the scripts, not a cleaned-up, end-to-end pipeline. `Updated Dataset Projections.R` reads raw source series that are not included in the repository, so it documents how the dataset was built rather than running from raw data to results.

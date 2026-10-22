# Bank of Canada Governor's Challenge: Impulse Response Analysis in R

R code from our team's work in the Bank of Canada Governor's Challenge (September to November 2024, York University). The code explores how Canadian inflation, real GDP and unemployment respond to an interest rate shock using local projections, as part of the analysis behind our monetary policy recommendation. It is a working version of that analysis, shared for demonstration purposes (see the note at the end).

## Project summary

The Governor's Challenge asks student teams to assess Canadian economic conditions, build a macroeconomic outlook, and defend a monetary policy recommendation before a panel of judges. Our team used **local linear impulse response functions** to trace the dynamic effect of an interest rate shock on CPI, real GDP and unemployment over a 12-quarter horizon, and presented our recommendations to the judges. This repository contains working scripts from that analysis, not the final version of the code or data we used.

## Repository structure

```
.
├── README.md
├── code/
│   ├── 01_dataset_construction_working.R   how the dataset was assembled (documentation, not runnable end to end)
│   ├── 02_impulse_responses.R              local projection estimation and impulse response plots
│   └── 03_confidence_bands.R               impulse responses with 95% and 90% bands superimposed
└── data/
    ├── BOCdatatrim.csv                     estimation dataset read by 02_impulse_responses.R
    ├── Data with Crude Oil.csv             version of the dataset with a crude oil price change series added
    └── Finaldataset.csv                    another assembled version of the dataset (not read by the scripts)
```

Despite its name, `Finaldataset.csv` is a working version of the dataset, not the final version used in the competition.

## How the pieces fit together

1. **Dataset (`01_...`, `data/`):** quarterly Canadian series are assembled from raw sources and merged. Script 01 documents the construction of the macroeconomic series. The market variables (VIX, commodity index, S&P 500 and crude oil) were added separately, so the finished datasets are provided in `data/`.
2. **Estimation (`02_...`):** reads `data/BOCdatatrim.csv`, estimates a local projection for each outcome (CPI, real GDP, unemployment) and plots the impulse responses.
3. **Confidence bands (`03_...`):** re-estimates the models with 90% bands and plots them together with the 95% bands. Run it after script 02, in the same R session.

## What this code demonstrates

- **Time-series estimation:** linear local projections with `lp_lin` from the `lpirfs` package, including endogenous and exogenous lags, additional controls, and confidence bands.
- **Data preparation:** cleaning and merging monthly and quarterly macroeconomic series with `tidyverse` and `lubridate`, aggregating monthly data to quarters, and constructing lags, leads, growth rates and year-over-year changes.
- **Specification choices:** separate models for each outcome with the other variables as exogenous regressors, controls for financial-market and commodity-price changes and the exchange rate, and robust regression (`lm_robust`) as a complement.
- **Visualization:** impulse response plots with confidence ribbons built in `ggplot2`.

## Data

Quarterly Canadian series covering 1996 Q1 to 2024 Q2 (114 observations in the estimation dataset): CPI, unemployment rate, interest rate, real GDP, money growth rate and exchange rate, plus changes in the VIX, a commodity index, the S&P 500 and crude oil prices.

> Data sources: Statistics Canada, the Bank of Canada, FRED (Federal Reserve Bank of St. Louis) and Yahoo Finance.

## Method (as specified in these scripts)

- **Estimator:** linear local projections (`lp_lin`, `lpirfs`)
- **Shock:** interest rate, rescaled so that one unit equals 25 basis points. The plots show the response to a 25 basis point rate cut (the estimated responses are multiplied by -1).
- **Outcomes:** CPI, real GDP and unemployment, each in its own model with the other two variables as exogenous regressors
- **Controls:** contemporaneous exchange rate and two-quarter-lagged changes in the VIX, commodity prices and the S&P 500
- **Specification:** 4 lags of the endogenous and exogenous variables, 12-quarter horizon, 95% confidence bands (and 90% in script 03)

## Running the code

1. Install the R packages: `install.packages(c("tidyverse", "lpirfs", "ggview", "scales"))`.
2. Set the R working directory to the root of this repository, so that the scripts can find `data/BOCdatatrim.csv`.
3. Run `code/02_impulse_responses.R` to estimate the models and generate the plots.
4. Optionally, run `code/03_confidence_bands.R` in the same session for the plots with superimposed bands.

## Note

This repository is shared for demonstration purposes and does not present the final results or the final version of the code and data used in the competition. These are working versions of the scripts, not a cleaned-up, end-to-end pipeline. `01_dataset_construction_working.R` reads raw source series that are not included in the repository, so it documents how the dataset was built rather than running from raw data to results.

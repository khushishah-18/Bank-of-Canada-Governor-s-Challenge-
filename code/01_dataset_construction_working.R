# =============================================================================
# 01_dataset_construction_working.R
#
# Working script used to assemble the quarterly dataset (1996 Q1 to 2024 Q2)
# from raw series: money supply, exchange rates, real GDP and CPI. It builds on
# unemployment and interest rate series assembled in earlier exploratory
# scripts, and runs exploratory local projections of CPI.
#
# This script documents how the dataset was built. It does not run from start
# to finish:
#   * the raw source files are not included in the repository, and the file
#     paths below point to the original author's computer;
#   * some objects (e.g., merged_quarterly5, newdata3, test) were created in
#     earlier exploratory scripts that are not part of this repository;
#   * the VIX, commodity index, S&P 500 and crude oil series in the files in
#     data/ were added separately and are not created here.
#
# The estimation datasets in data/ are the result of this process. Start from
# code/02_impulse_responses.R to run the analysis.
# =============================================================================

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tseries)
library(ggview)
library(ggfortify)
library(vars)
library(lpirfs)
library(estimatr) ### for robust linear regression estimation
library(sandwich) ### for robust SE
library(stats)


#### Converting Money Supply into Quarterly ####

moneysupply <- read.csv("C:/Users/shahk/Downloads/STATIC_ATABLE_V37151,STATIC_ATABLE_V37152,STATIC_ATABLE_V41552801-sd-1996-01-01-ed-2024-07-02.csv")

moneysupply$date <- parse_date_time(moneysupply$date, orders = "y%-m%-d", tz = "UTC")

moneysupply$date<- ymd(moneysupply$date)

moneysupply<- moneysupply %>% 
  filter(date < "2023-12-01")




quarterly_ms <- moneysupply %>%
  filter(month(date) %in% c(1, 4, 7, 10)) %>%
  mutate(Quarter = case_when(
    month(date) == 1 ~ "Q1",
    month(date) == 4 ~ "Q2",
    month(date) == 7 ~ "Q3",
    month(date) == 10 ~ "Q4"
  )) %>%
  group_by(Quarter, Year = year(date)) %>%
  summarise(moneygrowthrate = last(moneygrowthrate), .groups = 'drop') %>%
  arrange(Year, Quarter) %>% 
  arrange(Year, Quarter) %>%
  mutate(Date = case_when(
    Quarter == "Q1" ~ as.Date(paste(Year, "01", "01", sep = "-")),
    Quarter == "Q2" ~ as.Date(paste(Year, "04", "01", sep = "-")),
    Quarter == "Q3" ~ as.Date(paste(Year, "07", "01", sep = "-")),
    Quarter == "Q4" ~ as.Date(paste(Year, "10", "01", sep = "-"))  # Add 1 to the year for Q4
  ))



quarterly_ms <- quarterly_ms %>% 
  subset(select = c(Date, moneygrowthrate))

quarterly_ms <- quarterly_ms %>% 
  rename(date = Date)






#### Projection ####

newdata3 <- newdata3 %>% 
  inner_join(quarterly_ms, by = "date")


newdata <- merged_quarterly5 %>% 
  filter(date >= "1996-01-01") %>% 
  rename(intrates = rates)

endog_newdata <- newdata %>% 
  subset(select = c(quartcpi, unemprate, qgdp, intrates))   ### use shock variables as endogenous here

exog_newdata <- newdata %>%  ### use control variable as exogenous here 
  subset(select = c(moneygrowthrate))

results_lin3 <- lp_lin(endog_newdata,                              ### code for linear projection
                       lags_endog_lin = 0,                       ### no lag in endog (shock) variables
                       trend = 0,
                       shock = 1,
                       exog_data = exog_newdata,
                       lags_exog = 4,                ### lag only exog (control) variables
                       confint = 1.96,
                       hor = 12)
plot(results_lin3)


linear_plots[[3]]

summary(results_lin3)

endog_newdata <- as.data.frame(endog_newdata)

exog_newdata <- as.data.frame(exog_newdata)

str(newdata)





#### Exchange Rates ####


exrates <- read.csv("C:/Users/shahk/Desktop/BOC/Exchange Rates.csv")

exrates <- exrates %>% 
  rename(date = X90.01.31,
         ex_rate = X1.172038)

colnames(exrates)

exrates$date <- parse_date_time(exrates$date, orders = "y%-m%-d", tz = "UTC")

exrates <- exrates %>%
  mutate(date = date + days(1)) ## since ex rates are as of month end, modifying it to make 
                                 #it the opening of next month to align with data set

exrates <- exrates %>% 
  filter(date > "1995-11-01")


exrates$date <- ymd(exrates$date)


qexrates <- exrates %>%
  filter(month(date) %in% c(4, 7, 8, 12)) %>%  # Keep only last months of each quarter
  group_by(Year = year(date), Quarter = quarter(date)) %>%
  summarise(ex_rate = last(ex_rate), .groups = 'drop') %>%  # Use 'ex_rate' here
  mutate(date = as.Date(paste(Year, Quarter * 3, "01", sep = "-")))# Create quarterly date
  
qexrates <- qexrates %>% 
  subset(select = c(date, ex_rate)) %>%  # Select correct columns
  arrange(date)



qexrates <- exrates %>%
  filter(month(date) %in% c(1, 4, 7, 10)) %>%  # Keep only the specified months
  mutate(Quarter = case_when(
    month(date) == 1 ~ 1,
    month(date) == 4 ~ 2,
    month(date) == 7 ~ 3,
    month(date) == 10 ~ 4
  )) %>%
  group_by(Year = year(date), Quarter) %>%
  summarise(ex_rate = last(ex_rate), .groups = 'drop')


qexrates <- qexrates %>% 
  mutate(date = case_when(
    Quarter == 1 ~ as.Date(paste(Year, "01", "01", sep = "-")),  # April 1st
    Quarter == 2 ~ as.Date(paste(Year, "04", "01", sep = "-")),  # July 1st
    Quarter == 3 ~ as.Date(paste(Year, "07", "01", sep = "-")),  # October 1st
    Quarter == 4 ~ as.Date(paste(Year, "10", "01", sep = "-")))) %>%
  subset(select = c(date, ex_rate)) %>% 
  arrange(date)
      
    






str(qexrates)

qexrates$date <- ymd(qexrates$date)

qexrates <- qexrates %>%
  filter(month(date) %in% c(1, 4, 7, 10))


qexrates <- exrates %>%
  filter(month(date) %in% c(1, 4, 7, 10))


qexrates <- qexrates %>% 
  filter(date <= "2024-04-01")

qexrates <- qexrates %>% 
  filter(date >= "1996-01-01")


qexrates <- qexrates %>%
  arrange(date) %>%
  mutate(
    percent_change = (ex_rate - lag(ex_rate)) / lag(ex_rate) * 100  # Percentage change
  )


test <- test %>% 
  inner_join(qexrates, by = "date")


newdata3 <- newdata3 %>% 
  subset(select = c(date,quartcpi,unemprate, rates, realgdpgrowthrate,moneygrowthrate, percent_change))



colnames(newdata3)
#####

newdata2 <- newdata %>% 
  inner_join(qexrates, by = "date")


endog_newdata2 <- newdata2 %>% 
  subset(select = c(quartcpi, unemprate, qgdp, intrates))   ### use shock variables as endogenous here

exog_newdata2 <- newdata2 %>%  ### use control variable as exogenous here 
  subset(select = c(moneygrowthrate, percent_change))

results_lin4 <- lp_lin(endog_newdata2,                              ### code for linear projection
                       lags_endog_lin = 0,                       ### no lag in endog (shock) variables
                       trend = 0,
                       shock = 1,
                       exog_data = exog_newdata2,
                       lags_exog = 4,                ### lag only exog (control) variables
                       confint = 1.96,
                       hor = 12)

summary(results_lin4)


plot(results_lin4)


irf_values4 <- results_lin4$irf_lin_mean


irf_ts4 <- ts(irf_values4, frequency = 4, start = c(1996, 2))

yoy_cpiprojections <- (irf_ts - stats::lag(irf_ts, 4))/stats::lag(irf_ts, 4) * 100

yoy_cpiprojections <- na.omit(yoy_cpiprojections)


print(yoy_cpiprojections)

plot(yoy_cpiprojections)
  
  
  
#### REAL GDP ####

qrealgdp <- read.csv("C:/Users/shahk/Downloads/Quarterly Real GDP.csv")


qrealgdp <- qrealgdp %>% 
  filter(Estimates == "Gross domestic product at market prices")

qrealgdp <- qrealgdp %>% 
  rename(date = REF_DATE)

qrealgdp <- qrealgdp %>% 
  mutate(date = as.Date(paste0(date, "-01")))


qrealgdp$date <- ymd(qrealgdp$date)



#### Dataset with CPI, UNEMP, REAL GDP, INTRATES, MS, EXRATES ####

newdata2 <- newdata2 %>% 
  inner_join(qrealgdp, by = "date")

newdata2 <- newdata2 %>% 
  subset(select = c(date, quartcpi, unemprate, realgdpgrowthrate, intrates, moneygrowthrate, percent_change))


newdata3 <- newdata3 %>% 
  rename(exratepercent = percent_change,
         qrealgdp = realgdpgrowthrate)



newdata3 <- newdata3 %>% 
  mutate(lag_intrates = lag(rates, n = 4),          ### manually lagging variables of interest, lead cpi to make cpi forward looking
         lag_unemprates = lag(unemprate, n = 4),
         lag_qrealgdp = lag(qrealgdp, n = 4),
         lead_quartcpi = lead(quartcpi, n = 3))



?lp_lin
#### Linear Impulse Response with Local Projections ####

endog_newdata3 <- newdata3 %>% 
  subset(select =  c(lead_quartcpi, lag_unemprates, lag_qrealgdp, lag_intrates))  ### manually lagged variables of interest (lag = 4), CPI lead = 3

exog_newdata3 <- newdata3 %>% 
  subset(select = c(moneygrowthrate, exratepercent))

contemp_data <- newdata3 %>% 
  subset(select = c(quartcpi, unemprate, qrealgdp, intrates))

results <- lp_lin(endog_newdata3,                              ### code for linear projection
                       lags_endog_lin = 0,                       ### no lag in endog (shock) variables
                       trend = 0,
                       shock = 1,
                       exog_data = exog_newdata3,
                       lags_exog = 3,                           ### lag only exog (control) variables
                      contemp_data = contemp_data,             ### using current period data for variables of interest                         
                       confint = 1.96,
                       hor = 12)

## need to manually lag intrates, unemp, qgdp. make cpi forward looking. cpi forward looking by 3 quarts, variables of interest lagged by 4

plot(results)
summary(results)


write.csv(newdata3, file = "BOCDataset.csv")


results_summary <- summary(results)



results_robust <- lm_robust(lead_quartcpi ~ lag_intrates + lag_unemprates + lag_qrealgdp, data = newdata2)#needs to include the lags

results_robust

summary(results_robust)




irfnewvalues <- results$irf_lin_mean

irfnewvalues <- as.numeric(irfnewvalues)

length(irfnewvalues)

irfnew_ts <- ts(irfnewvalues, start = c(2024, 1), frequency = 4)

irfnew_df <- data.frame(date = time(irfnew_ts), value = irfnew_ts)


irfnewvalues <- results$irf_lin_mean  # Ensure this has the correct length
irfnew_ts <- ts(as.numeric(irfnewvalues), start = c(2024, 1), frequency = 4)

newyoy_cpiprojections <- irfnew_df %>%
  mutate(yoy_change = (value - lag(value, 4)) / lag(value, 4) * 100) %>%
  filter(!is.na(yoy_change) & !is.infinite(yoy_change)) 

irfnewvalues <- results$irf_lin_mean[1:12]  # Take only first 4 values for one year
irfnew_ts <- ts(as.numeric(irfnewvalues), start = c(2024, 1), frequency = 4)



newyoy_cpiprojections <- (irfnew_ts - stats::lag(irfnew_ts, 4))/stats::lag(irfnew_ts, 4) * 100

newyoy_cpiprojections <- na.omit(newyoy_cpiprojections)


print(newyoy_cpiprojections)

plot(yoy_cpiprojections)



write.csv(newdata2, file = "BOCDataset.csv")



data24 <- data.frame(Date = as.Date(ymd("2024-01-01") : ymd("2024-04-01")))


data24 <- data24 %>% 
  rename(date = Date)


test <- newdata3

test <- test %>% 
  full_join(data24)
 

test <- test[c(1:112, 113, 204), ]       ### will have to create lagged variables again after adding 2024 data


qcpi_new <- read.csv("C:/Users/shahk/Desktop/BOC/QUARTERLY CPI NEW.csv")


qcpi_new$date <- parse_date_time(qcpi_new$date, orders = "y%-m%-d", tz = "UTC")

qcpi_new$date <- ymd(qcpi_new$date)

qcpi_new <- qcpi_new %>% 
  filter(month(date) %in% c(1, 4, 7, 10))


qcpi_new <- qcpi_new %>% 
  filter(date >= "1996-01-01")

qcpi_new <- qcpi_new %>% 
  filter(date != "2024-07-01")



test <- test %>% 
  left_join(qcpi_new, by = "date") %>% 
  mutate(quartcpi = qcpi_new)



test <- test %>% 
  dplyr::select(-c(7,8))



test <- test %>% 
  rename(quartcpi = STATIC_TOTALCPICHANGE)

test <- test %>% 
  rename(ex_ratepercent = percent_change)





test <- test %>% 
  dplyr::select(date, quartcpi, unemprate, intrates, qrealgdp, moneygrowthrate, ex_ratepercent)


test <- test %>% 
  mutate(unemprate = ifelse(date == "2024-01-01", 5.87, unemprate),
         unemprate = ifelse(date == "2024-04-01", 6.23, unemprate),
         intrates = ifelse(date == "2024-01-01", 4.75, intrates),
         intrates = ifelse(date == "2024-04-01", 4.5, intrates),
         qrealgdp = ifelse(date == "2024-01-01", 0.4, qrealgdp),
         qrealgdp = ifelse(date == "2024-04-01", 0.5, qrealgdp))

finaldata <- test


finaldata <- finaldata %>% 
  mutate(lag1_intrates = lag(intrates, n = 1),
         lag2_intrates = lag(intrates, n = 2),
         lag3_intrates = lag(intrates, n = 3),### manually lagging variables of interest, lead cpi to make lag1_unemprates = lag(unemprate, n = 1),
         lag2_unemprates = lag(unemprate, n = 2),
         lag3_unemprates = lag(unemprate, n = 3),
         lag1_qrealgdp = lag(qrealgdp, n = 1),
         lag2_qrealgdp = lag(qrealgdp, n = 2),
         lag3_qrealgdp = lag(qrealgdp, n = 3),
         lag1_moneysupply = lag(moneygrowthrate, n =1),
         lag1_exrates = lag(ex_ratepercent, n = 1),
         lag1_quartcpi = lag(quartcpi, n =1 ),
         lag2_quartcpi = lag(quartcpi, n = 2),
         lag3_quartcpi = lag(quartcpi, n =3),
         lead_quartcpi = lead(quartcpi, n = 3))


View(finaldata$quartcpi)

endog_datafinal <- finaldata %>% 
  subset(select = c(quartcpi,intrates)) 



exog_datafinal <- finaldata %>%
  subset(select = c(unemprate, qrealgdp)) ## lagged by 3

         
contemp_data  <- finaldata %>% 
  subset(select = c(quartcpi, unemprate, intrates, qrealgdp, lag1_moneysupply, lag1_exrates, moneygrowthrate, exratepercent))

results <- lp_lin(endog_datafinal,
                  lags_endog_lin = 3,
                  trend = 0,
                  shock = 1,
                  exog_data = exog_datafinal,
                  lags_exog = 1,
                  contemp_data = contemp_data,
                  confint = 1.96,
                  hor = 12)

plot(results)         

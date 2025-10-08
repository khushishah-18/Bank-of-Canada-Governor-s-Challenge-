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



finaldata <- read.csv("C:/Users/shahk/Desktop/BOC/BOCdatatrim.csv")

finaldata <- finaldata %>% 
  dplyr::select(c(, 2:15))

finaldata <- finaldata %>% 
  mutate(intratesbps = 4*intrates)

finaldata <- finaldata %>% 
  mutate(lag_vixchange = lag(VIX_Change, n = 2),
         lag_comind = lag(ComInd_Change, n = 2),
         lag_sp500 = lag(SP500_Change, n = 2))




#### CPI ####

LHS <- finaldata %>% 
  subset(select = c(quartcpi,intratesbps)) 



RHS <- finaldata %>%
  subset(select = c(unemprate, qrealgdp)) 


contemp_data  <- finaldata %>% 
  subset(select = c(unemprate, intratesbps, qrealgdp, ex_ratepercent, lag_vixchange, lag_comind, lag_sp500 )) ## moneysupply and exrates lagged by 1

results <- lp_lin(LHS,
                  lags_endog_lin = 4,
                  trend = 0,
                  shock = 1,
                  exog_data = RHS,
                  lags_exog = 4,
                  contemp_data = contemp_data,
                  confint = 1.96,
                  hor = 12)

plot(results)

linear_plots <- plot_lin(results)

linear_plots[[2]]

View(results)




irf_datafinal <- results$irf_lin_mean[1, ,2]
lower_datafinal <- results$irf_lin_low[1, ,2]
upper_datafinal <- results$irf_lin_up[1, ,2]


n_aheaddf <- 1:length(irf_datafinal)




irf_datafinal <- data.frame(
  n_aheaddf = n_aheaddf,
  irf_datafinal = irf_datafinal,
  lower_datafinal = lower_datafinal,
  upper_datafinal = upper_datafinal)


  
ggplot(irf_datafinal, aes(x = n_aheaddf))+
  geom_ribbon(aes(ymin = -upper_datafinal, ymax = -lower_datafinal), fill = "#faab5c", alpha = 0.2) +
  geom_line(aes(y = -irf_datafinal), color = "orange")+
  labs(title = "Impulse Response Function with Impulse (Interest Rates)",
       x = "Quarters",
       y = "CPI") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_x_continuous(breaks = seq(0, 12, by = 2), labels = c("2024Q2", "2024Q4", "2025Q2", "2025Q4", "2026Q2", "2026Q4", "2027Q2"),
                     expand = expansion(add = c(0, 0)))+
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),  
    labels = function(x) { 
      1.6 + x  
    }
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(r = 10), vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 9, hjust = 1),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))+
  canvas(1920, 1080, units = "px")



#### Real GDP ####
LHSgdp <- finaldata %>% 
  subset(select = c(qrealgdp,intratesbps)) 



RHSgdp <- finaldata %>%
  subset(select = c(unemprate, quartcpi)) 


contemp_datagdp  <- finaldata %>% 
  subset(select = c(unemprate, intratesbps, quartcpi, ex_ratepercent,  lag_vixchange, lag_comind, lag_sp500)) ## moneysupply and exrates lagged by 1

resultsgdp <- lp_lin(LHSgdp,
                     lags_endog_lin = 4,
                     trend = 0,
                     shock = 1,
                     exog_data = RHSgdp,
                     lags_exog = 4,
                     contemp_data = contemp_datagdp,
                     confint = 1.96,
                     hor = 12)

plot(resultsgdp)

linear_plotgdp <- plot_lin(resultsgdp)

linear_plotgdp[[2]]

print(resultsgdp)





irf_datagdp <- resultsgdp$irf_lin_mean[1, ,2]
lower_datagdp <- resultsgdp$irf_lin_low[1, ,2]
upper_datagdp <- resultsgdp$irf_lin_up[1, ,2]


n_aheadgdp <- 1:length(irf_datagdp)




irf_datagdp <- data.frame(
  n_aheadgdp = n_aheadgdp,
  irf_datagdp = irf_datagdp,
  lower_datagdp = lower_datagdp,
  upper_datagdp = upper_datagdp)




ggplot(irf_datagdp, aes(x = n_aheadgdp))+
  geom_ribbon(aes(ymin = -upper_datagdp, ymax = -lower_datagdp), fill = "#ff6876", alpha = 0.2) +
  geom_line(aes(y = -irf_datagdp), color = "red")+
  labs(title = "Impulse Response Function with Impulse (Interest Rates)",
       x = "Quarters",
       y = "Real GDP (Percentage Change)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  scale_x_continuous(breaks = seq(0, 12, by = 2), labels = c("2024Q2", "2024Q4", "2025Q2", "2025Q4", "2026Q1", "2027Q3", "2027Q1"),
                     expand = expansion(add = c(0, 0)))+
  scale_y_continuous(breaks = seq(-1, 1, by = 0.50)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(r = 10), vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 9, hjust = 1),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))+
  canvas(1920, 1080, units = "px")



#### Unemployment Data ####


LHSunemp <- finaldata %>% 
  subset(select = c(unemprate,intratesbps)) 



RHSunemp <- finaldata %>%
  subset(select = c(qrealgdp, quartcpi)) 


contemp_dataunemp  <- finaldata %>% 
  subset(select = c(qrealgdp, intratesbps, quartcpi, ex_ratepercent, lag_vixchange, lag_comind, lag_sp500)) ## moneysupply and exrates lagged by 1

resultsunemp <- lp_lin(LHSunemp,
                       lags_endog_lin = 4,
                       trend = 0,
                       shock = 1,
                       exog_data = RHSunemp,
                       lags_exog = 4,
                       contemp_data = contemp_dataunemp,
                       confint = 1.96,
                       hor = 12)

irf_dataunemp <- resultsunemp$irf_lin_mean[1, ,2]
lower_dataunemp <- resultsunemp$irf_lin_low[1, ,2]
upper_dataunemp <- resultsunemp$irf_lin_up[1, ,2]

plot(resultsunemp)

n_aheadunemp <- 1:length(irf_dataunemp)




irf_dataunemp <- data.frame(
  n_aheadunemp = n_aheadunemp,
  irf_dataunemp = irf_dataunemp,
  lower_dataunemp = lower_dataunemp,
  upper_dataunemp = upper_dataunemp)



ggplot(irf_dataunemp, aes(x = n_aheadunemp)) +
  geom_ribbon(aes(ymin = -upper_dataunemp, ymax = -lower_dataunemp), fill = "lightgreen", alpha = 0.2) +  # Invert ribbon bounds
  geom_line(aes(y = -irf_dataunemp), color = "darkgreen") +  
  labs(
    title = "Impulse Response Function with Impulse (Interest Rates)",
    x = "Quarters",
    y = "Unemployment Rate (Percentage Points Change)"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(
    breaks = seq(0, 12, by = 2),
    labels = c("2024Q2", "2024Q4", "2025Q2", "2025Q4", "2026Q1", "2027Q3", "2027Q1"),
    expand = expansion(add = c(0, 0))
  ) +
  scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 0.25)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 10, margin = margin(r = 10), vjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 9, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  canvas(1920, 1080, units = "px")









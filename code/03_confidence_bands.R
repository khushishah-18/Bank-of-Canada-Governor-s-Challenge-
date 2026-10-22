# =============================================================================
# 03_confidence_bands.R
#
# Re-plots the impulse responses from 02_impulse_responses.R with 95% and 90%
# confidence bands superimposed. The responses are shown for a 25 basis point
# interest rate cut.
#
# Requires: run code/02_impulse_responses.R first, in the same R session. This
#           script reuses the objects it creates (finaldata, irf_datafinal,
#           irf_datagdp, irf_dataunemp).
# Output  : plots (nothing is saved to disk)
#
# Note    : working version of the competition script.
# =============================================================================

library(tidyverse)   # data manipulation (dplyr) and plotting (ggplot2)
library(scales)      # label_number()
library(lpirfs)      # local projections: lp_lin()
library(ggview)      # canvas() for setting plot size


#### CPI ####

# 90% bands: re-estimate the CPI model with a 1.645 critical value
LHS90 <- finaldata %>%
  subset(select = c(quartcpi, intratesbps))

RHS90 <- finaldata %>%
  subset(select = c(unemprate, qrealgdp))

contemp_data90 <- finaldata %>%
  subset(select = c(unemprate, intratesbps, qrealgdp, ex_ratepercent,
                    lag_vixchange, lag_comind, lag_sp500))

results90 <- lp_lin(LHS90,
                    lags_endog_lin = 4,
                    trend = 0,
                    shock = 1,
                    exog_data = RHS90,
                    lags_exog = 4,
                    contemp_data = contemp_data90,
                    confint = 1.645,
                    hor = 12)

irf_datafinal90   <- results90$irf_lin_mean[1, , 2]
lower_datafinal90 <- results90$irf_lin_low[1, , 2]
upper_datafinal90 <- results90$irf_lin_up[1, , 2]

n_aheaddf90 <- 1:length(irf_datafinal90)

irf_datafinal90 <- data.frame(
  n_aheaddf90 = n_aheaddf90,
  irf_datafinal90 = irf_datafinal90,
  lower_datafinal90 = lower_datafinal90,
  upper_datafinal90 = upper_datafinal90)

# Combine the 95% and 90% results into one data frame and label them
irf_data_combined <- rbind(
  data.frame(
    n_ahead = irf_datafinal$n_aheaddf,
    irf = irf_datafinal$irf_datafinal,
    lower = irf_datafinal$lower_datafinal,
    upper = irf_datafinal$upper_datafinal,
    CI = "95% CI",
    stringsAsFactors = FALSE
  ),
  data.frame(
    n_ahead = irf_datafinal90$n_aheaddf90,
    irf = irf_datafinal90$irf_datafinal90,
    lower = irf_datafinal90$lower_datafinal90,
    upper = irf_datafinal90$upper_datafinal90,
    CI = "90% CI",
    stringsAsFactors = FALSE
  )
)

# Responses are negated to show a rate cut; y-axis labels are shifted by a
# constant baseline (1.6)
ggplot(irf_data_combined, aes(x = n_ahead)) +
  geom_ribbon(data = subset(irf_data_combined, CI == "95% CI"),
              aes(ymin = -upper, ymax = -lower, fill = CI),
              alpha = 0.8, show.legend = TRUE) +
  geom_ribbon(data = subset(irf_data_combined, CI == "90% CI"),
              aes(ymin = -upper, ymax = -lower, fill = CI),
              alpha = 1, show.legend = TRUE) +
  geom_line(data = irf_data_combined, aes(y = -irf, color = CI),
            linewidth = 0.5, show.legend = FALSE) +
  labs(title = "Impulse Response of CPI to a 25 Basis Points Interest Rate Cut",
       x = "Quarters",
       y = "CPI") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 12, by = 2),
                     labels = c("2024Q2", "2024Q4", "2025Q2", "2025Q4",
                                "2026Q2", "2026Q4", "2027Q2"),
                     expand = expansion(add = c(0, 0))) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) label_number(accuracy = 0.01)(1.6 + x)) +
  scale_fill_manual(values = c("95% CI" = "#1f77b4", "90% CI" = "#7ec8e3"),
                    labels = c("90% CI", "95% CI")) +
  scale_color_manual(values = c("95% CI" = "navy", "90% CI" = "blue")) +
  guides(guide_legend(title = "Confidence Intervals", title.position = "top",
                      title.hjust = 0.5, label.size = 10)) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(r = 10), vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 9, hjust = 1),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white")) +
  canvas(1920, 1080, units = "px")


#### Real GDP ####

LHSgdp90 <- finaldata %>%
  subset(select = c(qrealgdp, intratesbps))

RHSgdp90 <- finaldata %>%
  subset(select = c(unemprate, quartcpi))

contemp_datagdp90 <- finaldata %>%
  subset(select = c(unemprate, intratesbps, quartcpi, ex_ratepercent,
                    lag_vixchange, lag_comind, lag_sp500))

resultsgdp90 <- lp_lin(LHSgdp90,
                       lags_endog_lin = 4,
                       trend = 0,
                       shock = 1,
                       exog_data = RHSgdp90,
                       lags_exog = 4,
                       contemp_data = contemp_datagdp90,
                       confint = 1.645,
                       hor = 12)

irf_datagdp90   <- resultsgdp90$irf_lin_mean[1, , 2]
lower_datagdp90 <- resultsgdp90$irf_lin_low[1, , 2]
upper_datagdp90 <- resultsgdp90$irf_lin_up[1, , 2]

n_aheadgdp90 <- 1:length(irf_datagdp90)

irf_datagdp90 <- data.frame(
  n_aheadgdp90 = n_aheadgdp90,
  irf_datagdp90 = irf_datagdp90,
  lower_datagdp90 = lower_datagdp90,
  upper_datagdp90 = upper_datagdp90)

# Combine the 95% and 90% results into one data frame and label them
irf_datagdp_combined <- rbind(
  data.frame(
    n_ahead = irf_datagdp$n_aheadgdp,
    irf = irf_datagdp$irf_datagdp,
    lower = irf_datagdp$lower_datagdp,
    upper = irf_datagdp$upper_datagdp,
    CI = "95% CI",
    stringsAsFactors = FALSE
  ),
  data.frame(
    n_ahead = irf_datagdp90$n_aheadgdp90,
    irf = irf_datagdp90$irf_datagdp90,
    lower = irf_datagdp90$lower_datagdp90,
    upper = irf_datagdp90$upper_datagdp90,
    CI = "90% CI",
    stringsAsFactors = FALSE
  )
)

ggplot(irf_datagdp_combined, aes(x = n_ahead)) +
  geom_ribbon(data = subset(irf_datagdp_combined, CI == "95% CI"),
              aes(ymin = -upper, ymax = -lower, fill = CI),
              alpha = 0.8, show.legend = TRUE) +
  geom_ribbon(data = subset(irf_datagdp_combined, CI == "90% CI"),
              aes(ymin = -upper, ymax = -lower, fill = CI),
              alpha = 1, show.legend = TRUE) +
  geom_line(data = irf_datagdp_combined, aes(y = -irf, color = CI),
            linewidth = 0.5, show.legend = FALSE) +
  labs(title = "Impulse Response of Real GDP to a 25 Basis Points Interest Rate Cut",
       x = "Quarters",
       y = "Real GDP (Percentage Change)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 12, by = 2),
                     labels = c("2024Q2", "2024Q4", "2025Q2", "2025Q4",
                                "2026Q2", "2026Q4", "2027Q2"),
                     expand = expansion(add = c(0, 0))) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.50)) +
  scale_fill_manual(values = c("95% CI" = "#b94e4e", "90% CI" = "#f8c0c0"),
                    labels = c("90% CI", "95% CI")) +
  scale_color_manual(values = c("95% CI" = "#800000", "90% CI" = "blue")) +
  guides(guide_legend(title = "Confidence Intervals", title.position = "top",
                      title.hjust = 0.5, label.size = 10)) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(r = 10), vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 9, hjust = 1),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white")) +
  canvas(1920, 1080, units = "px")


#### Unemployment rate ####

LHSunemp90 <- finaldata %>%
  subset(select = c(unemprate, intratesbps))

RHSunemp90 <- finaldata %>%
  subset(select = c(qrealgdp, quartcpi))

contemp_dataunemp90 <- finaldata %>%
  subset(select = c(qrealgdp, intratesbps, quartcpi, ex_ratepercent,
                    lag_vixchange, lag_comind, lag_sp500))

resultsunemp90 <- lp_lin(LHSunemp90,
                         lags_endog_lin = 4,
                         trend = 0,
                         shock = 1,
                         exog_data = RHSunemp90,
                         lags_exog = 4,
                         contemp_data = contemp_dataunemp90,
                         confint = 1.645,
                         hor = 12)

irf_dataunemp90   <- resultsunemp90$irf_lin_mean[1, , 2]
lower_dataunemp90 <- resultsunemp90$irf_lin_low[1, , 2]
upper_dataunemp90 <- resultsunemp90$irf_lin_up[1, , 2]

n_aheadunemp90 <- 1:length(irf_dataunemp90)

irf_dataunemp90 <- data.frame(
  n_aheadunemp90 = n_aheadunemp90,
  irf_dataunemp90 = irf_dataunemp90,
  lower_dataunemp90 = lower_dataunemp90,
  upper_dataunemp90 = upper_dataunemp90)

# Combine the 95% and 90% results into one data frame and label them
irf_dataunemp_combined <- rbind(
  data.frame(
    n_ahead = irf_dataunemp$n_aheadunemp,
    irf = irf_dataunemp$irf_dataunemp,
    lower = irf_dataunemp$lower_dataunemp,
    upper = irf_dataunemp$upper_dataunemp,
    CI = "95% CI",
    stringsAsFactors = FALSE
  ),
  data.frame(
    n_ahead = irf_dataunemp90$n_aheadunemp90,
    irf = irf_dataunemp90$irf_dataunemp90,
    lower = irf_dataunemp90$lower_dataunemp90,
    upper = irf_dataunemp90$upper_dataunemp90,
    CI = "90% CI",
    stringsAsFactors = FALSE
  )
)

# Responses are negated to show a rate cut; y-axis labels are shifted by a
# constant baseline (6.5)
ggplot(irf_dataunemp_combined, aes(x = n_ahead)) +
  geom_ribbon(data = subset(irf_dataunemp_combined, CI == "95% CI"),
              aes(ymin = -upper, ymax = -lower, fill = CI),
              alpha = 0.8, show.legend = TRUE) +
  geom_ribbon(data = subset(irf_dataunemp_combined, CI == "90% CI"),
              aes(ymin = -upper, ymax = -lower, fill = CI),
              alpha = 1, show.legend = TRUE) +
  geom_line(data = irf_dataunemp_combined, aes(y = -irf, color = CI),
            linewidth = 0.5, show.legend = FALSE) +
  labs(title = "Impulse Response of Unemployment Rate to a 25 Basis Points Interest Rate Cut",
       x = "Quarters",
       y = "Unemployment Rate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(0, 12, by = 2),
                     labels = c("2024Q2", "2024Q4", "2025Q2", "2025Q4",
                                "2026Q2", "2026Q4", "2027Q2"),
                     expand = expansion(add = c(0, 0))) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) label_number(accuracy = 0.01)(6.5 + x)) +
  scale_fill_manual(values = c("95% CI" = "#388e3c", "90% CI" = "#a8e6a1"),
                    labels = c("90% CI", "95% CI")) +
  scale_color_manual(values = c("95% CI" = "darkgreen", "90% CI" = "blue")) +
  guides(guide_legend(title = "Confidence Intervals", title.position = "top",
                      title.hjust = 0.5, label.size = 10)) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(r = 10), vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 9, hjust = 1),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(t = 10), face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white")) +
  canvas(1920, 1080, units = "px")

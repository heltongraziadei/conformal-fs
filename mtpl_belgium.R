rm(list=ls())

library(tidyverse)
library(magrittr)
library(ranger)
library(gridExtra)
library(insuranceData)

set.seed(12345)
mtpl_belgium <- readRDS("mtpl_data.rds")
mtpl_belgium <- mtpl_belgium %>% dplyr::select(-c(amount))

# Data treatments 
mtpl_belgium <- mtpl_belgium[-92668, ]
mtpl_belgium %<>% 
  tidyr::replace_na(list(average = 0))

# EDA
g1 <- mtpl_belgium %>% 
  ggplot(aes(x = nclaims)) + geom_bar(
    aes(y = after_stat(count / sum(count))),
    colour = "black", fill = "gray") + 
  theme_bw() + 
  xlab("Frequency") + 
  ylab("") + 
  theme(axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
  ) + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5))

g2 <- mtpl_belgium %>% 
  ggplot(aes(x = average)) + 
  geom_histogram(
    aes(y = after_stat(count / sum(count))), 
    colour = "black", fill = "gray") + 
  theme_bw() + 
  xlab("Severity") + 
  ylab("") + 
  xlim(NA, 1) + 
  theme(axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
  ) + 
  scale_x_continuous(labels = scales::comma)

g_sim <- grid.arrange(g1, g2, nrow = 1)

ggsave("mtpl_distributions.pdf", g_sim, height = 6, width = 16)

# Split data into training, calibration and test
n <- nrow(mtpl_belgium)
n_trn <- round(n * 0.7)
n_cal <- round(n * 0.15)
n_tst <- n - n_trn - n_cal

idx <- sample(1:3, size = n, prob =
                c(n_trn, n_cal, n_tst),
              replace = TRUE)

training <- mtpl_belgium[idx == 1, ]
calibration <- mtpl_belgium[idx == 2, ]
test <- mtpl_belgium[idx == 3, ]
alpha <- 0.05
features_freq_glm <- c('coverage', 'fuel', 'sex', 'use', 
                       'fleet', 'ageph', 'power', 'agec', 'bm',
                       'long', 'lat',  "offset(log(expo))")
features_freq <- c('coverage', 'fuel', 'sex', 'use', 
                   'fleet', 'ageph', 'power', 'agec', 'bm',
                   'long', 'lat')

features_sev <- c('coverage', 'fuel', 'sex', 'use', 
                  'fleet', 'ageph', 'power', 'agec', 'bm',
                  'long', 'lat',
                  "nclaims")

formula_be_freq_glm <- paste("nclaims ~", paste(features_freq_glm, 
                                                collapse = ' + '))

formula_be_freq <- paste("nclaims / expo ~", paste(features_freq, 
                                                   collapse = ' + '))

formula_be_sev <- paste('log(average) ~',
                        paste(features_sev, 
                              collapse = ' + '))
                              
# Model 1 - gamma regression for severity stage 
rf1 <- ranger(formula_be_freq,
              data = training, 
              num.trees = 10^3)

glm2 <- glm(average ~ coverage + fuel + sex + use + fleet + ageph + power + agec + 
              bm + long + lat, data = training[training$nclaims > 0, ], 
            family = Gamma(link = "log")
            )

d_hat_cal <- round(calibration$expo *
                     predict(rf1, 
                             data = calibration
                     )$predictions)

mae_rf_d <- mean(abs(d_hat_cal - calibration$nclaims))

cal_dgt0 <- calibration[d_hat_cal > 0, ]
cal_dgt0$nclaims <- d_hat_cal[d_hat_cal > 0]
y_hat_cal <- as.numeric(predict(glm2, 
                                newdata = cal_dgt0, 
                                type = "response"))

R <- abs(cal_dgt0$average - y_hat_cal)
s_hat_glm <- sort(R)[round((1 - alpha)*(nrow(cal_dgt0) + 1))]
d_hat_tst <- round(test$expo * predict(rf1, data = test)$predictions)
test_dgt0_glm <- test[d_hat_tst > 0, ]
test_dgt0_glm$nclaims_obs <- test_dgt0_glm$nclaims
test_dgt0_glm$nclaims <- d_hat_tst[d_hat_tst > 0]
y_hat_tst <- predict(glm2, newdata = test_dgt0_glm, type = "response")

mae_glm <- mean(abs(test_dgt0_glm$average - y_hat_tst))

test_dgt0_glm$lower_glm <- pmax(0, y_hat_tst - s_hat_glm)
test_dgt0_glm$upper_glm <- y_hat_tst + s_hat_glm

coverage_glm <- mean(test_dgt0_glm$lower_glm <= test_dgt0_glm$average & test_dgt0_glm$average <= test_dgt0_glm$upper_glm)

features_sev <- c('coverage', 'fuel', 'sex', 'use', 
                  'fleet', 'ageph', 'power', 'agec', 'bm',
                  'long', 'lat', "nclaims")

# Model 2 - random forest for severity stage
rf2 <- ranger(formula_be_sev, 
              data = training[training$nclaims > 0,],
              num.trees = 10^3, 
              mtry = 2)

cal_dgt0 <- calibration[d_hat_cal > 0, ]
cal_dgt0$nclaims <- d_hat_cal[d_hat_cal > 0]
y_hat_cal <- exp(predict(rf2, 
                         data = cal_dgt0)$predictions)

R <- abs(cal_dgt0$average - y_hat_cal)
s_hat_rf <- sort(R)[round((1 - alpha)*
                            (nrow(cal_dgt0) + 1))]

test_dgt0_rf <- test[d_hat_tst > 0, ]
test_dgt0_rf$nclaims_obs <- test_dgt0_rf$nclaims
test_dgt0_rf$nclaims <- d_hat_tst[d_hat_tst > 0]

y_hat_tst <- exp(predict(rf2, data = test_dgt0_rf)$predictions)

mae_rf <- mean(as.numeric(abs(test_dgt0_rf$average - y_hat_tst)))

test_dgt0_rf$lower_rf <- pmax(0, y_hat_tst - s_hat_rf)
test_dgt0_rf$upper_rf <- y_hat_tst + s_hat_rf

coverage_rf <- mean(test_dgt0_rf$lower_rf <= test_dgt0_rf$average & test_dgt0_rf$average <= test_dgt0_rf$upper_rf)

# Plot a sample of prediction intervals
test_dgt0_glm <- test_dgt0_glm[, c("lower_glm", "upper_glm", "average")]
test_dgt0_rf <- test_dgt0_rf[, c("lower_rf", "upper_rf")]

test_dgt0_merged <- merge(test_dgt0_glm, test_dgt0_rf,
                          by = "row.names")

set.seed(123456)

test_sample <- test_dgt0_merged  %>% 
  filter(average > 200) %>% 
  slice(1:50)

idx_sample <- sample(nrow(test_sample), size =  50, replace = FALSE)

test_sample <- test_sample[idx_sample, ]

options(scipen=999)

g1 <- test_sample %>% 
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = average)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_glm, ymax = upper_glm), color = "black") +
  labs(x = "test sample unit", y = "severity", 
       title = "Gamma regression") +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4e+3)) 


g2 <- test_sample %>% 
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = average)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_rf, ymax = upper_rf), color = "black") +
  labs(x = "test sample unit", y = "severity", 
       title = "Random forest") +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4e+3)) 

g_sim_be <- grid.arrange(g1, g2, nrow = 1)
ggsave("mtpl_tsscp_intervals.pdf", g_sim_be, height = 5, width = 14)
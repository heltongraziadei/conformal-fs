rm(list=ls())

library(tidyverse)
library(magrittr)
library(ranger)
library(gridExtra)
library(insuranceData)

set.seed(123456)

# Read data and select relevant variables
db <- read.csv("rural_data_pca.csv") %>% 
  dplyr::select(-c(sum_total_premios, 
                   sum_indenizacao_rel)) %>% 
  filter(n_apolices > 0) %>% 
  mutate(tipo_solo = fct_lump(tipo_solo, n = 20)) %>% 
  dplyr::select(-starts_with("Tmin_"), 
                -starts_with("Tmax_"),
                -starts_with("Tmp_"),
                -starts_with("Vap_"),
                -starts_with("Pet_"),
                -starts_with("Cld_"),
                -starts_with("N_Days_"), 
                -starts_with("n_apo"), 
  )  %>% 
  mutate(sum_indenizacao_rel = ifelse(sum_indenizacao_total > 0, sum_indenizacao_total / n_sinistros, 0))  %>%
  dplyr::select(-c(sum_indenizacao_total))

# EDA
g1 <- db %>% 
  ggplot(aes(x = n_sinistros)) + geom_histogram(
    bins = 20, 
    aes(y = after_stat(count / sum(count))),
    colour = "black", fill = "gray") + 
  theme_bw() + 
  xlab("Frequency") + 
  ylab("") + 
  xlim(NA, 100) + 
  theme(axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
  )

g2 <- db %>% 
  filter(n_sinistros > 0) %>% 
  ggplot(aes(x = sum_indenizacao_rel)) + 
  geom_histogram(
    aes(y = after_stat(count / sum(count))), 
    colour = "black", fill = "gray") + 
  theme_bw() + 
  xlab("Relative loss") + 
  ylab("") + 
  xlim(NA, 1) + 
  theme(axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
  ) + 
  scale_x_continuous(labels = scales::comma)

g_sim <- grid.arrange(g1, g2, nrow = 1)

ggsave("rural_distributions.pdf", g_sim, height = 6, width = 16)

# Split dataset for conformal prediction
n <- nrow(db)
n_trn <- round(n * 0.7)
n_cal <- round(n * 0.15)
n_tst <- n - n_trn - n_cal

idx <- sample(1:3, size = n, prob = c(n_trn, n_cal, n_tst), replace = TRUE)

training <- db[idx == 1, ]
calibration <- db[idx == 2, ] 
test <- db[idx == 3, ] 

alpha <- 0.05

glm1 <- glm(n_sinistros ~ . - sum_indenizacao_rel,
            data = training,
            offset = log(area_total + 1),
            family = poisson)
rf1 <- ranger(n_sinistros ~ . - sum_indenizacao_rel, data = training, 
              num.trees = 10^3)

glm2 <- glm(sum_indenizacao_rel ~  . - n_sinistros,
            data = training[training$n_sinistros > 0, ],
	    family = Gamma(link = "log"))

d_hat_cal <- round(predict(glm1, newdata = 
                             calibration, 
                           type = "response"))
d_hat_cal <- round(predict(rf1, data = calibration)$predictions)
cal_dgt0 <- calibration[d_hat_cal > 0, ]
cal_dgt0$n_sinistros <- d_hat_cal[d_hat_cal > 0]
y_hat_cal <- as.numeric(predict(glm2, 
                                newdata = cal_dgt0, 
                                type = "response"))

R <- abs(cal_dgt0$sum_indenizacao_rel - y_hat_cal)
s_hat_glm <- sort(R)[round((1 - alpha)*(nrow(cal_dgt0) + 1))]

d_hat_tst <- round(predict(glm1, newdata = test, 
                           type = "response"))
test_dgt0_glm <- test[d_hat_tst > 0, ]
test_dgt0_glm$n_sinistros_obs <- test_dgt0_glm$n_sinistros
test_dgt0_glm$n_sinistros <- d_hat_tst[d_hat_tst > 0]
y_hat_tst <- predict(glm2, newdata = test_dgt0_glm, type = "response")

mae_glm <- mean(abs(test_dgt0_glm$sum_indenizacao_rel - y_hat_tst))

test_dgt0_glm$lower_glm <- pmax(0, y_hat_tst - s_hat_glm)
test_dgt0_glm$upper_glm <- y_hat_tst + s_hat_glm

coverage_glm <- mean(test_dgt0_glm$lower_glm <= test_dgt0_glm$sum_indenizacao_rel & test_dgt0_glm$sum_indenizacao_rel <= test_dgt0_glm$upper_glm)

# Model 2 - random forest for severity stage
rf1 <- ranger(n_sinistros ~ . - sum_indenizacao_rel, data = training, num.trees = 10^3)
rf2 <- ranger(log(sum_indenizacao_rel) ~  . - n_sinistros, 
              data = 
                training[training$n_sinistros > 0, ], 
              num.trees = 10^3)

d_hat_cal <- round(predict(rf1, data = calibration)$predictions) 

cal_dgt0 <- calibration[d_hat_cal > 0, ]
y_hat_cal <- exp(predict(rf2, data = cal_dgt0)$predictions)
R <- abs(cal_dgt0$sum_indenizacao_rel - y_hat_cal)
s_hat_rf <- sort(R)[round((1 - alpha)*
                            (nrow(cal_dgt0) + 1))]
cat("Interval length (RF):", s_hat_rf)

d_hat_tst <- round(predict(rf1, data = test)$predictions)
test_dgt0_rf <- test[d_hat_tst > 0, ]
test_dgt0_rf$n_sinistros_obs <- 
  test_dgt0_rf$n_sinistros
test_dgt0_rf$n_sinistros <- d_hat_tst[d_hat_tst > 0]
y_hat_tst <- exp(predict(rf2, data = test_dgt0_rf)$predictions)

mae_rf <- mean(abs(test_dgt0_rf$sum_indenizacao_rel - y_hat_tst))
mae_rf

test_dgt0_rf$lower_rf <- pmax(0, y_hat_tst - s_hat_rf)
test_dgt0_rf$upper_rf <- y_hat_tst + s_hat_rf

coverage_rf <- mean(test_dgt0_rf$lower_rf <= test_dgt0_rf$sum_indenizacao_rel & test_dgt0_rf$sum_indenizacao_rel <= test_dgt0_rf$upper_rf)

# Plot sample of prediction intervals
test_dgt0_glm <- test_dgt0_glm[, c("lower_glm", "upper_glm", "sum_indenizacao_rel")]
test_dgt0_rf <- test_dgt0_rf[, c("lower_rf", "upper_rf")]

test_dgt0_merged <- merge(test_dgt0_glm, test_dgt0_rf,
                          by = "row.names")

test_sample <- test_dgt0_merged %>% 
  slice(1:50)

g1 <- test_sample %>% 
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = sum_indenizacao_rel)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_glm, ymax = upper_glm), color = "black") +
  labs(x = "test sample unit", y = "relative loss", 
       title = "Gamma regression") +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 25, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)
                                    ), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 1.2e+6))

g2 <- test_sample %>%
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = sum_indenizacao_rel)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_rf, ymax = upper_rf), color = "black") +
  labs(x = "test sample unit", y = "relative loss", 
       title = "Random forest") + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)
                                    ), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 1.2e+6))

g_rural_cp <- grid.arrange(g1, g2, nrow = 1)
ggsave("rural_tsscp_intervals.pdf", g_rural_cp, height = 5, width = 14)

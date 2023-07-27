rm(list = ls())

library(tidyverse)
library(magrittr)
library(ranger)
library(gridExtra)
library(insuranceData)

set.seed(123456)

n <- 10^4

# Simulate synthetic data
simulate_data <- function(n, p = 10) {
  X <- matrix(10*runif(n*p), 
              nrow = n, 
              ncol = p, 
              dimnames = list(1:n, paste0("x_", 1:p)))
  d <- ifelse(runif(n) > 1/2, 0, 
              rpois(n, lambda = exp(0.01*X[, 1])))
  y <- ifelse(d == 0, 0,
              rexp(n, rate = 
                     1 / (4*exp(X[, 2]) + sin(X[, 3]* X[, 4]) +
                     5*X[, 5]^3)))
  data.frame(cbind(X, d, y))
}

db <- simulate_data(n)

summary(db$y)
mean(db$y == 0)

# EDA
g1 <- db %>% 
  ggplot(aes(x = d)) + geom_bar(
    aes(y = after_stat(count / sum(count))),
    colour = "black", fill = "gray") + 
  theme_bw() + 
  xlab("Frequency") + 
  ylab("") + 
  theme(axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
  ) + scale_x_continuous(breaks = 0:6)

g2 <- db %>% 
  filter(d > 0) %>% 
  mutate(s =  y / d) %>% 
  ggplot(aes(x = s)) + 
  geom_histogram(
    bins = 100, 
    aes(y = after_stat(count / sum(count))), 
    colour = "black", fill = "gray") + 
  theme_bw() + 
  xlab("Severity") + 
  ylab("") + 
  # coord_cartesian(xlim = c(-5, 5e+4)) + 
  # xlim(NA, 1e+4) + 
  theme(axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
  ) + 
  scale_x_continuous(labels = scales::comma) + 
  coord_cartesian(xlim = c(0, 2e+5))

g_sim <- grid.arrange(g1, g2, nrow = 1)

# write.csv(db, "synthetic.csv",  row.names = FALSE)

ggsave("synthetic_distributions.pdf", g_sim, height = 6, width = 16)

# Split data in training, calibration and test
n_trn <- round(n * 0.5)
n_cal <- round(n * 0.25)
n_tst <- n - n_trn - n_cal

idx <- sample(1:3, size = n, prob = c(n_trn, n_cal, n_tst), replace = TRUE)

training <- db[idx == 1, ]
calibration <- db[idx == 2, ]
test <- db[idx == 3, ]

alpha <- 0.05

# Model 1 - gamma regression for severity stage
rf1 <- ranger(d ~ . - y, data = training, num.trees = 10^3)
glm2 <- glm(y ~ ., data = training[training$d > 0, ],
            family = Gamma(link = "log"), 
            offset = log(d))

d_hat_cal <- round(predict(rf1, data = calibration)$predictions)

cal_dgt0 <- calibration[d_hat_cal > 0, ]

cal_dgt0$d <- d_hat_cal[d_hat_cal > 0]

y_hat_cal <- as.numeric(predict(glm2, newdata = cal_dgt0, type = "response"))

R <- abs(cal_dgt0$y - y_hat_cal)

s_hat_glm <- sort(R)[round((1 - alpha)*
                             
                             (nrow(cal_dgt0) + 1))]

d_hat_tst <- round(predict(rf1, data = test)$predictions)

test_dgt0_glm <- test[d_hat_tst > 0, ]

test_dgt0_glm$d_obs <- test_dgt0_glm$d

test_dgt0_glm$d <- d_hat_tst[d_hat_tst > 0]

y_hat_tst <- predict(glm2, newdata = test_dgt0_glm, type = "response")

mae_glm <- mean(abs(test_dgt0_glm$y - y_hat_tst))

test_dgt0_glm$lower_glm <- pmax(0, y_hat_tst - s_hat_glm)

test_dgt0_glm$upper_glm <- y_hat_tst + s_hat_glm

coverage_glm <- mean(test_dgt0_glm$lower_glm <= test_dgt0_glm$y & test_dgt0_glm$y <= test_dgt0_glm$upper_glm)

# Model 2 - random forest for severity stage
rf2 <- ranger(log(y) ~ ., data = training[training$d > 0, ], num.trees = 10^3)

d_hat_cal <- round(predict(rf1, data = calibration)$predictions)

cal_dgt0 <- calibration[d_hat_cal > 0, ]
cal_dgt0$d <- d_hat_cal[d_hat_cal > 0]
y_hat_cal <- exp(predict(rf2, data = cal_dgt0)$predictions)

R <- abs(cal_dgt0$y - y_hat_cal)
s_hat_rf <- sort(R)[round((1 - alpha)*(nrow(cal_dgt0) + 1))]

d_hat_tst <- round(predict(rf1, data = test)$predictions)
test_dgt0_rf <- test[d_hat_tst > 0, ]
test_dgt0_rf$d_obs <- test_dgt0_rf$d
test_dgt0_rf$d <- d_hat_tst[d_hat_tst > 0]
y_hat_tst <- exp(predict(rf2, data = test_dgt0_rf)$predictions)

mae_rf <- mean(abs(test_dgt0_rf$y - y_hat_tst))

test_dgt0_rf$lower_rf <- pmax(0, y_hat_tst - s_hat_rf)
test_dgt0_rf$upper_rf <- y_hat_tst + s_hat_rf

methods <- data.frame(
  id = 1:nrow(test_dgt0_glm),
  lower = test_dgt0_rf$lower, 
  upper = test_dgt0_rf$upper, 
  y = test_dgt0_rf$y, 
  method = c(rep("RF", nrow(test_dgt0_glm)), 
             rep("GLM", nrow(test_dgt0_rf))))

coverage_rf <- mean(test_dgt0_rf$lower_rf <= test_dgt0_rf$y & test_dgt0_rf$y <= test_dgt0_rf$upper_rf)

# Plot sample of prediction intervals
test_dgt0_glm <- test_dgt0_glm[, c("lower_glm", "upper_glm", "y")]
test_dgt0_rf <- test_dgt0_rf[, c("lower_rf", "upper_rf")]

test_dgt0_merged <- merge(test_dgt0_glm, test_dgt0_rf, 
                          by = "row.names")

 
test_sample <- test_dgt0_merged %>% 
  filter(y > 500) 

idx_sample <- sample(1:nrow(test_sample), size = 50, replace = FALSE)

test_sample <- test_sample[idx_sample, ]

test_sample_2 <- methods %>% 
  filter(y > 100) 

options(scipen=999)

g1 <- test_sample %>% 
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = y)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_glm, ymax = upper_glm), color = "black") +
  labs(x = "test sample unit", y = "severity", 
       title = "Gamma regression") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 3e+5)) 

g2 <- test_sample %>% 
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = y)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_rf, ymax = upper_rf), color = "black") +
  labs(x = "test sample unit", y = "severity", 
       title = "Random forest") +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3e+5)) 

g_sim_cp <- grid.arrange(g1, g2, nrow = 1 ) # , widths = c(0.6, 0.4))
ggsave("synthetic_tsscp_intervals.pdf", g_sim_cp, height = 5, width = 14)

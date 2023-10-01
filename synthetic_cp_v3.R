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
rf2 <- ranger(log(y / d) ~ ., data = training[training$d > 0, ], num.trees = 10^3)

d_hat_cal <- round(predict(rf1, data = calibration)$predictions)
y_hat_cal <- exp(predict(rf2, data = calibration)$predictions) * d_hat_cal

R <- abs(calibration$y - y_hat_cal)
s_hat_rf <- sort(R)[round((1 - alpha)*(nrow(calibration) + 1))]

d_hat_tst <- round(predict(rf1, data = test)$predictions)
y_hat_tst <- exp(predict(rf2, data = test)$predictions)*d_hat_tst

mae_rf <- mean(abs(test$y - y_hat_tst))

test$lower_rf <- pmax(0, y_hat_tst - s_hat_rf)
test$upper_rf <- y_hat_tst + s_hat_rf

coverage_rf <- mean(test$lower_rf <= test$y & test$y <= test$upper_rf)

# Plot sample of prediction intervals
# test_dgt0_glm <- test_dgt0_glm[, c("lower_glm", "upper_glm", "y")]
# test_dgt0_rf <- test_dgt0_rf[, c("lower_rf", "upper_rf")]

# test_dgt0_merged <- merge(test_dgt0_glm, test_dgt0_rf, 
#                           by = "row.names")


test_sample <- test # %>% 
  # filter(y > 500) 

idx_sample <- sample(1:nrow(test_sample), size = 50, replace = FALSE)

test_sample <- test_sample[idx_sample, ]

options(scipen=999)

g1 <- test_sample %>% 
  arrange(desc(y)) %>% 
  mutate(nrow_sample = 1:nrow(test_sample)) %>% 
  ggplot(aes(x = nrow_sample, y = y)) + 
  geom_point(color = "black", size = 1.2) +
  geom_errorbar(aes(ymin = lower_rf, ymax = upper_rf), color = "black") +
  labs(x = "test sample unit", y = "severity", 
       title = "Random Forest") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  ) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 1e+5)) 

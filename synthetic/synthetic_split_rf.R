# rm(list = ls())

library(tidyverse)
library(ranger)

simulate_data <- function(n, p = 10) {
    X <- matrix(runif(n*p, max = 10), nrow = n, ncol = p, dimnames = list(1:n, paste0("x_", 1:p)))
    d <- ifelse(runif(n) > 1/2, 0, rpois(n, lambda = exp(0.01*X[, 1])))
    y <- ifelse(d == 0, 0, rexp(n, rate =  1 / (4*exp(X[, 2]) + sin(X[, 3]*X[, 4]) + 5*X[, 5]^3)))
    data.frame(X, d, y) |> as_tibble()
}

set.seed(42)

n <- 10^4 # 5000

db <- simulate_data(n)

n_trn <- round(n * 0.5)
n_cal <- round(n * 0.25)
n_tst <- n - n_trn - n_cal

idx <- sample(1:3, size = n, prob = c(n_trn, n_cal, n_tst), replace = TRUE)

trn <- db |> filter(idx == 1)
cal <- db |> filter(idx == 2)
tst <- db |> filter(idx == 3)

alpha <- 0.1

# frequency model

rf1 <- ranger(d ~ . -y, data = trn, num.trees = 10^3)

cal$d <- predict(rf1, data = cal)$predictions |> round()
tst$d <- predict(rf1, data = tst)$predictions |> round()

# severity model

trn_pos <- trn |> filter(d > 0)

rf2 <- ranger(log(y) ~ ., data = trn_pos, num.trees = 10^3)

psi_hat_cal <- exp(predict(rf2, data = cal)$predictions)
psi_hat_tst <- exp(predict(rf2, data = tst)$predictions)

# severity variablity model

psi_hat_trn <- exp(predict(rf2, data = trn_pos)$predictions)

trn_pos_res <- trn_pos |> mutate(delta = abs(y - psi_hat_trn)) |> select(-y)

rf3 <- ranger(delta ~ ., data = trn_pos_res, num.trees = 10^3)

sig_hat_cal <- predict(rf3, data = cal)$predictions
sig_hat_tst <- predict(rf3, data = tst)$predictions

R <- abs(cal$y - psi_hat_cal) / sig_hat_cal
r_hat <- sort(R)[round((1 - alpha)*(nrow(cal) + 1))]

nrow(cal) - length(unique(R))

mean(abs(tst$y - psi_hat_tst)) # 5050.60

lower <- pmax(0, psi_hat_tst - r_hat * sig_hat_tst)
upper <- psi_hat_tst + r_hat * sig_hat_tst

mean(lower <= tst$y & tst$y <= upper) # 0.9017

mean(upper - lower) # 12154.00

tst_rf <- tst |> mutate(lower, upper)

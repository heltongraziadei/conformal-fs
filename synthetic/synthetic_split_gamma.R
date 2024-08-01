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

n <- 10^4

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

gamma_reg1 <- glm(y ~ ., data = trn_pos, family = Gamma(link = "log"), control = list(epsilon = 1e-4, maxit = 100))

psi_hat_cal <- predict(gamma_reg1, newdata = cal, type = "response")
psi_hat_tst <- predict(gamma_reg1, newdata = tst, type = "response")

# severity variablity model

psi_hat_trn <- predict(gamma_reg1, newdata = trn_pos, type = "response")

trn_pos_res <- trn_pos |> mutate(delta = abs(y - psi_hat_trn)) |> select(-y)

gamma_reg2 <- glm(delta ~ ., data = trn_pos_res, family = Gamma(link = "log"), control = list(epsilon = 1e-4, maxit = 100))

sig_hat_cal <- predict(gamma_reg2, newdata = cal, type = "response")
sig_hat_tst <- predict(gamma_reg2, newdata = tst, type = "response")

R <- abs(cal$y - psi_hat_cal) / sig_hat_cal
r_hat <- sort(R)[round((1 - alpha)*(nrow(cal) + 1))]

nrow(cal) - length(unique(R))

mean(abs(tst$y - psi_hat_tst)) # 11225.85

lower <- pmax(0, psi_hat_tst - r_hat * sig_hat_tst)
upper <- psi_hat_tst + r_hat * sig_hat_tst

mean(lower <= tst$y & tst$y <= upper) # 0.9025

mean(upper - lower) # 23717.68

tst_glm <- tst |> mutate(lower, upper)

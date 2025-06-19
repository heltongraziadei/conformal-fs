# rm(list = ls())

library(tidyverse)
library(ranger)
library(ciTools)

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

trn <- db |> filter(idx %in% c(1, 2))
tst <- db |> filter(idx == 3)

B <- 10^4 # number of bootstrap samples

alpha <- 0.1

# frequency model

rf1 <- ranger(d ~ . -y, data = trn, num.trees = 10^3)

tst$d <- predict(rf1, data = tst)$predictions |> round()

# severity model

fit <- glm(y ~ ., data = trn |> filter(d > 0),
           family = Gamma(link = "log"),
           control = list(epsilon = 1e-4, maxit = 100))

boot <- add_pi(tst, fit, names = c("lower", "upper"), alpha = alpha, nSims = B)

lower <- pmax(0, boot$lower)

lower <- boot$lower
upper <- boot$upper

mean(lower <= tst$y & upper >= tst$y) # 0.3083

mean(upper - lower) # 50789.91

tst_boot <- tst |> mutate(lower, upper)

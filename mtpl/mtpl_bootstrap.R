library(tidyverse)
library(ranger)

db <- readRDS("mtpl_data.rds") |>
    mutate(y = replace_na(average, 0)) |> 
    rename(d = nclaims) |>
    select(-c(id, claim, amount, average))

set.seed(42)

n <- nrow(db)

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

# fit <- lm(y ~ ., data = trn)

fit <- glm(y ~ ., data = trn |> filter(d > 0),
           family = Gamma(link = "log"),
           control = list(epsilon = 1e-4, maxit = 100))

boot <- ciTools::add_pi(tst, fit, names = c("lower", "upper"), alpha = alpha, nSims = B)

lower <- pmax(0, boot$lower)
upper <- boot$upper

mean(lower <= tst$y & upper >= tst$y) # 0.1105

mean(upper - lower) # 7656.35

tst_boot <- tst |> mutate(lower, upper)

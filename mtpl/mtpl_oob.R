# rm(list = ls())

library(tidyverse)
library(ranger)

db <- readRDS("mtpl_data.rds") |>
    mutate(y = replace_na(average, 0)) |> 
    rename(d = nclaims) |>
    select(-c(id, claim, amount, average)) |>
    as_tibble()

set.seed(42)

n <- nrow(db)

n_trn <- round(n * 0.5)
n_cal <- round(n * 0.25)
n_tst <- n - n_trn - n_cal

idx <- sample(1:3, size = n, prob = c(n_trn, n_cal, n_tst), replace = TRUE)

trn <- db |> filter(idx %in% c(1, 2))
tst <- db |> filter(idx == 3)

alpha <- 0.1

# frequency model

num_trees <- 3*10^3

rf1 <- ranger(d ~ . -y, data = trn, keep.inbag = TRUE, num.trees = num_trees)
out1 <- matrix(unlist(lapply(rf1$inbag.count, \(x) x == 0)), nrow = rf1$num.trees, byrow = TRUE)

mu_hat_trn <- predict(rf1, data = trn, predict.all = TRUE)$predictions |>
    ( \(.x) sapply(1:nrow(trn), \(.i) mean(.x[.i, out1[, .i]])) )() |>
    round()

mu_hat_tst <- predict(rf1, data = tst)$predictions |> round()

rm(rf1, out1); gc()

trn$d <- mu_hat_trn
tst$d <- mu_hat_tst

# severity model

rf2 <- ranger(y ~ ., data = trn, keep.inbag = TRUE, num.trees = num_trees)

out2 <- matrix(unlist(lapply(rf2$inbag.count, \(x) x == 0)), nrow = rf2$num.trees, byrow = TRUE)
psi_hat_trn <- predict(rf2, data = trn, predict.all = TRUE)$predictions |>
    ( \(.x) sapply(1:nrow(trn), \(.i) mean(.x[.i, out2[, .i]])) )()

psi_hat_tst <- predict(rf2, data = tst)$predictions

rm(rf2, out2); gc()

# severity variability model

trn_res <- trn |> mutate(delta = abs(y - psi_hat_trn)) |> select(-y)

rf3 <- ranger(delta ~ ., data = trn_res, keep.inbag = TRUE, num.trees = num_trees)
out3 <- matrix(unlist(lapply(rf3$inbag.count, \(x) x == 0)), nrow = rf3$num.trees, byrow = TRUE)

sig_hat_trn <- predict(rf3, data = trn_res, predict.all = TRUE)$predictions |>
    ( \(.x) sapply(1:nrow(trn_res), \(.i) mean(.x[.i, out3[, .i]])) )()

sig_hat_tst <- predict(rf3, data = tst)$predictions

rm(rf3, out3); gc()

R <- abs(trn$y - psi_hat_trn) / sig_hat_trn
r_hat <- sort(R)[round((1 - alpha)*(nrow(trn) + 1))]

nrow(trn) - length(unique(R))

sqrt(mean((tst$y - psi_hat_tst)^2)) 

lower <- pmax(0, psi_hat_tst - r_hat * sig_hat_tst)
upper <- psi_hat_tst + r_hat * sig_hat_tst

mean(lower <= tst$y & tst$y <= upper) # 0.9134

mean(upper - lower) # 398.28

tst_oob <- tst |> mutate(lower, upper)

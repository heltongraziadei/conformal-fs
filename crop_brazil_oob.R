rm(list=ls())

library(tidyverse)
library(ranger)
set.seed(12345)

# Read data
db <- read.csv("rural_data_pca.csv") %>% 
  dplyr::select(-c(sum_total_premios, 
                   sum_total_premios_deflacionado, 
                   sum_vl_indenizacao_rel_nao_deflacionado,
                   sum_vl_indenizacao_total_nao_deflacionado,
                   # sum_indenizacao_total
                   )) %>% 
  mutate(tipo_solo = fct_lump(tipo_solo, n = 20)) 
  # mutate(sum_indenizacao_rel = ifelse(sum_indenizacao_total > 0, sum_indenizacao_total, 0))  %>%

# Split data for oob conformal prediction
n <- nrow(db)
n_trn <- round(n * 0.7)
n_tst <- round(n * 0.3)

idx <- sample(1:2, size = n, prob = c(n_trn, n_tst), replace = TRUE)
training <- db[idx == 1, ]
test <- db[idx == 2, ] 

rf1 <- ranger(n_sinistros ~ . - sum_indenizacao_rel - producao_segurada - sum_indenizacao_total, data = training, num.trees = 10^3)
rf2 <- ranger(log(sum_indenizacao_rel) ~  . - producao_segurada - sum_indenizacao_total, 
              data = training[training$n_sinistros > 0, ], 
              num.trees = 10^3,  keep.inbag = TRUE)

trn_dgt0 <- training[training$n_sinistros > 0, ]
y_hat_trn <- exp(predict(rf2, data = trn_dgt0, predict.all = TRUE)$predictions)

# Find OOB matrix
out <- matrix(unlist(lapply(rf2$inbag.count, \(x) x == 0)), nrow = rf2$num.trees, byrow = TRUE)

mad <- function(x) mean(abs(x - mean(x)))

mu_hat_trn <- sapply(1:nrow(trn_dgt0), \(i) mean(y_hat_trn[i, out[, i]]))
sig_hat_trn <- sapply(1:nrow(trn_dgt0), \(i) mad(y_hat_trn[i, out[, i]]))

alpha <- 0.05

R <- abs(trn_dgt0$sum_indenizacao_rel - mu_hat_trn) / sig_hat_trn
s_hat <- sort(R)[round((1 - alpha)*(nrow(trn_dgt0) + 1))]

d_hat_tst <- round(predict(rf1, data = test)$predictions)
# tst_dgt0 <- test[d_hat_tst > 0, ]
tst_dgt0 <- test %>% mutate(n_sinistros = d_hat_tst) %>% filter(n_sinistros > 0)
tst_dgt0$y_hat_tst <- exp(predict(rf2, data = tst_dgt0)$predictions)
R <- abs(tst_dgt0$sum_indenizacao_rel - tst_dgt0$y_hat_tst)

y_hat_tst <- exp(predict(rf2, data = tst_dgt0, predict.all = TRUE)$predictions)

mu_hat_tst <- rowMeans(y_hat_tst)
sig_hat_tst <- apply(y_hat_tst, 1, mad)

tst_dgt0$lower <- pmax(0, mu_hat_tst - s_hat * sig_hat_tst)*tst_dgt0$producao_segurada
tst_dgt0$upper <- (mu_hat_tst + s_hat * sig_hat_tst)*tst_dgt0$producao_segurada

coverage_oob <- mean(tst_dgt0$lower <= tst_dgt0$sum_indenizacao_rel & tst_dgt0$sum_indenizacao_rel <= tst_dgt0$upper)
coverage_oob <- mean(tst_dgt0$lower <= tst_dgt0$sum_indenizacao_total & tst_dgt0$sum_indenizacao_total <= tst_dgt0$upper)

intervals <- tst_dgt0 %>% 
  filter(sum_indenizacao_total > 1e+5 )

intervals <- intervals[sample(1:nrow(intervals), size = 100, replace = FALSE), ] %>% 
  arrange(desc(sum_indenizacao_total))

intervals$id = 1:nrow(intervals)

mean(intervals$lower <= intervals$sum_indenizacao_rel & intervals$sum_indenizacao_rel <= intervals$upper)

mean(intervals$lower <= intervals$sum_indenizacao_total & intervals$sum_indenizacao_total <= intervals$upper)

# summary((s_hat*sig_hat_tst) / (mu_hat_tst))
# alpha_dist <- data.frame(alpha = (s_hat*sig_hat_tst) / (mu_hat_tst))
# 
# alpha_dist %>% 
#   ggplot(aes(x = alpha)) + geom_histogram(aes(y = ..density..), colour = "white") +
#   # geom_density() + 
#   xlab(expression(alpha)) + 
# theme_bw() 

# Plot sample of prediction intervals
set.seed(1234)
g_oob_rural <- intervals %>%
  # arrange(desc(sum_indenizacao_rel)) %>% 
  # slice(1:100) %>%
  # filter(sum_indenizacao_rel > 0) %>% 
  ggplot(aes(x = id)) +
  geom_point(aes(y = sum_indenizacao_total), colour = "black", size = 0.8) +
  geom_point(aes(y = y_hat_tst*producao_segurada), colour = "red", size = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "black") +
  labs(x = "Test sample unit", y = "Relative Loss", 
       title = "Random forest") +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)
                                    ), 
        axis.title.x = element_text(size = 20, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)
                                    ), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        text = element_text(size = 20)
  )  #  + 
  # scale_y_continuous(labels = scales::comma, limits = c(0, 2.5e+6))

ggsave("rural_tsscp_oob_intervals.pdf", g_oob_rural, height = 5, width = 14)
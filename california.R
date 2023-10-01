rm(list=ls())

library(brms)
library(rsample)

db <- read.csv("california.csv") %>% 
  mutate(ocean_promixity = fct_lump(ocean_proximity, n = 5))

splitting <- initial_split(db, prop = 0.1)
db_train <- training(splitting)
db_test <- testing(splitting)

lm1 <- brm(
  formula = median_house_value ~ .,
  data = db_train, 
  chains = 2
)

post_pred <- brms::posterior_predict(lm1, db_test)
pred_int <- t(apply(post_pred, 2, function(x) quantile(x, c(0.05, 0.95))))
mean(pred_int[, 1] < db_test$median_house_value & db_test$median_house_value < pred_int[, 2] )
mean(pred_int[, 1] < 0)
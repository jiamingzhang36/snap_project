###############################################################################
# Step 4: Research-grade benchmarking of four models for individual exit risk
# Models: (1) Lasso Logit, (2) Random Forest, (3) XGBoost, (4) CatBoost
# Output: AUC comparison + best model prediction for 2025 ABAWD population
###############################################################################

library(dplyr)
library(readr)
library(Matrix)
library(glmnet)
library(ranger)
library(xgboost)
library(catboost)
library(pROC)

set.seed(2025)

###############################################################################
# 1. Load rollout training data (labeled exits) + county covariates
###############################################################################

indiv_rollout <- read_rds("data_clean/step4_abawd_rollout_panel.rds")
county_month  <- read_rds("data_clean/county_month_covars.rds")

# Merge and create training sample
train_df <- indiv_rollout %>%
  left_join(county_month, by = c("county_fips","month_id")) %>%
  filter(policy_exposed == 1) %>%    # exposed to work requirement
  drop_na(exit_3m)

y <- train_df$exit_3m

###############################################################################
# 2. Build model matrix for methods requiring numeric design matrix
###############################################################################

form <- ~ 
  age + I(age^2) +
  gender +
  education_level +
  prior_work_any +
  recent_earnings_log +
  disability_flag +
  has_dependents +
  unemp_rate +
  job_training_slots_pc +
  factor(region) +
  factor(exposure_year)

X <- model.matrix(form, data = train_df)[, -1]   # drop intercept

# Create DMatrix for XGBoost
dtrain <- xgb.DMatrix(data = X, label = y)

###############################################################################
# 3. Train Model 1: Lasso Logistic (glmnet)
###############################################################################

cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)
lasso_pred <- predict(cv_lasso, newx = X, type = "response", s = "lambda.min")
auc_lasso <- auc(y, as.numeric(lasso_pred))

###############################################################################
# 4. Train Model 2: Random Forest (ranger)
###############################################################################

rf_fit <- ranger(
  dependent.variable.name = "exit_3m",
  data = train_df,
  probability = TRUE,
  num.trees = 800,
  mtry = floor(sqrt(ncol(train_df))),
  min.node.size = 50
)

rf_pred <- rf_fit$predictions[, "1"]
auc_rf <- auc(y, rf_pred)

###############################################################################
# 5. Train Model 3: XGBoost
###############################################################################

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.05,
  max_depth = 5,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_fit <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  verbose = 0
)

xgb_pred <- predict(xgb_fit, dtrain)
auc_xgb <- auc(y, xgb_pred)

###############################################################################
# 6. Train Model 4: CatBoost (best for categorical tabular data)
###############################################################################

train_pool <- catboost.load_pool(
  data = train_df %>% select(
    age, gender, education_level, prior_work_any, recent_earnings_log,
    disability_flag, has_dependents, unemp_rate, job_training_slots_pc,
    region, exposure_year
  ),
  label = y
)

cat_params <- list(
  loss_function = "Logloss",
  iterations = 1500,
  depth = 6,
  learning_rate = 0.03,
  random_strength = 1,
  bootstrap_type = "Bayesian",
  verbose = FALSE
)

cat_fit <- catboost.train(train_pool, params = cat_params)
cat_pred <- catboost.predict(cat_fit, train_pool, prediction_type = "Probability")
auc_cat <- auc(y, cat_pred)

###############################################################################
# 7. Compare AUC Performance
###############################################################################

auc_table <- tibble(
  Model = c("Lasso Logit", "Random Forest", "XGBoost", "CatBoost"),
  AUC   = c(auc_lasso, auc_rf, auc_xgb, auc_cat)
) %>% arrange(desc(AUC))

print(auc_table)

best_model_name <- auc_table$Model[1]
cat("Best model selected:", best_model_name, "\n")

###############################################################################
# 8. Load 2025 ABAWD population data for final predictions
###############################################################################

abawd_2025 <- read_rds("data_clean/step4_abawd_2025_population.rds")
county_2025 <- read_rds("data_clean/county_month_covars_2025_scenario.rds")

abawd_2025 <- abawd_2025 %>%
  left_join(county_2025, by = c("county_fips","month_id_2025"="month_id"))

###############################################################################
# 9. Predict 2025 individual-level exit risk using best model
###############################################################################

if (best_model_name == "Lasso Logit") {
  
  X_new <- model.matrix(form, data = abawd_2025)[, -1]
  abawd_2025$risk_exit_3m <- as.numeric(
    predict(cv_lasso, newx = X_new, type = "response", s = "lambda.min")
  )
  
} else if (best_model_name == "Random Forest") {
  
  rf_pred2025 <- predict(
    rf_fit,
    data = abawd_2025
  )$predictions[, "1"]
  abawd_2025$risk_exit_3m <- rf_pred2025
  
} else if (best_model_name == "XGBoost") {
  
  X_new <- model.matrix(form, data = abawd_2025)[, -1]
  d2025 <- xgb.DMatrix(X_new)
  abawd_2025$risk_exit_3m <- predict(xgb_fit, d2025)
  
} else if (best_model_name == "CatBoost") {
  
  pool_2025 <- catboost.load_pool(
    data = abawd_2025 %>% select(
      age, gender, education_level, prior_work_any, recent_earnings_log,
      disability_flag, has_dependents, unemp_rate, job_training_slots_pc,
      region, exposure_year
    )
  )
  abawd_2025$risk_exit_3m <- catboost.predict(
    cat_fit, pool_2025, prediction_type = "Probability"
  )
}

###############################################################################
# 10. Output summary for reporting (optional)
###############################################################################

risk_summary <- abawd_2025 %>%
  summarise(
    mean_risk = mean(risk_exit_3m, na.rm = TRUE),
    p90_risk  = quantile(risk_exit_3m, 0.9, na.rm = TRUE)
  )

print(risk_summary)

###############################################################################
# End of research-grade Step 4 benchmarking
###############################################################################

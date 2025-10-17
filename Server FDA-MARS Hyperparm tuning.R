# ============================
# Grid–search (degree, nprune) for mda::fda(method = earth)
# Parallel, stratified CV; metric = macro-F1
# ============================

set.seed(20251016)


suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(caret)
  library(mda)
  library(earth)
  library(themis)
  library(doParallel)
  library(rsample)
  library(forcats)
  library(caret)
  
})


clean_selected_collapsed_grouped <- readRDS("~/trg-880-capstone-2025/clean_selected_collapsed_grouped_Ethan.rds")
summary(clean_selected_collapsed_grouped)

#-------------------------------
# 1) Train/test split (stratified)
#-------------------------------
idx   <- caret::createDataPartition(clean_selected_collapsed_grouped$STATUS, p = 0.70, list = FALSE)
train <- clean_selected_collapsed_grouped[idx, ] %>% droplevels()
test  <- clean_selected_collapsed_grouped[-idx, ] %>% droplevels()

#-----------------------------------------
# 2) Handle class imbalance (SMOTE-NC on train only)
#-----------------------------------------
# NOTE: themis::smotenc expects outcome column name in `var`
train <- themis::smotenc(train, var = "STATUS", k = 5, over_ratio = 1) %>% droplevels()

summary(train$STATUS)


#-----------------------------------------
# 3) Remove zero / near-zero variance predictors (fit on train, apply to both)
#-----------------------------------------
# If you know a specific column is NZV (e.g., PS_Lapse1), the NZV filter will remove it anyway.
nzv_idx <- caret::nearZeroVar(train)
if (length(nzv_idx) > 0) {
  nzv_names <- names(train)[nzv_idx]
  message("Removing NZV predictors: ", paste(nzv_names, collapse = ", "))
  train <- train[, -nzv_idx, drop = FALSE]
  # Ensure identical columns in test (keep STATUS intact)
  drop_in_test <- intersect(nzv_names, setdiff(names(test), "STATUS"))
  if (length(drop_in_test) > 0) {
    test  <- test[, setdiff(names(test), drop_in_test), drop = FALSE]
  }
}

# Make sure factor levels of predictors align between train/test (safe coercion)
common_cols <- intersect(names(train), names(test))
train <- train[, common_cols, drop = FALSE]
test  <- test[,  common_cols, drop = FALSE]

#-----------------------------------------
# 4) Custom summary function: Macro-F1 (robust under imbalance)
#-----------------------------------------
macro_f1_summary <- function(data, lev = NULL, model = NULL) {
  # data: columns = obs, pred, (and class probs if classProbs = TRUE)
  cm <- caret::confusionMatrix(data = data$pred, reference = data$obs)
  byClass <- cm$byClass
  
  # byClass is a matrix for multiclass, named numeric for binary
  if (is.matrix(byClass)) {
    precision <- byClass[, "Precision"]
    recall    <- byClass[, "Recall"]
    f1        <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
    Mean_F1   <- mean(f1, na.rm = TRUE)
  } else {
    precision <- byClass["Precision"]
    recall    <- byClass["Recall"]
    Mean_F1   <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  }
  
  out <- c(
    Mean_F1  = Mean_F1,
    Kappa    = unname(cm$overall["Kappa"]),
    Accuracy = unname(cm$overall["Accuracy"])
  )
  return(out)
}

#-----------------------------------------
# 5) Parallel backend
#-----------------------------------------
n_cores <- max(1, parallel::detectCores() - 1L)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

on.exit({
  try(stopCluster(cl), silent = TRUE)
  registerDoSEQ()
}, add = TRUE)

#-----------------------------------------
# 6) CV setup and tuning grid
#-----------------------------------------
# Repeated, stratified CV with seeds for reproducibility
cv_repeats <- 3
cv_folds   <- 5

set.seed(20251016)
seeds <- vector(mode = "list", length = cv_folds * cv_repeats + 1)
# Number of models in each resample equals rows in tuneGrid
deg_vals   <- c(1, 2)                        # try linear and pairwise interaction MARS bases
npr_vals   <- seq(5, 50, by = 5)             # keep ≤ number of terms earth can generate
tune_grid  <- expand.grid(degree = deg_vals, nprune = npr_vals)

for (i in seq_len(cv_folds * cv_repeats)) seeds[[i]] <- sample.int(1e6, nrow(tune_grid))
seeds[[cv_folds * cv_repeats + 1]] <- sample.int(1e6, 1)  # for final model

ctrl <- caret::trainControl(
  method           = "repeatedcv",
  number           = cv_folds,
  repeats          = cv_repeats,
  classProbs       = TRUE,                 # needed for some summaries; fda provides posteriors
  summaryFunction  = macro_f1_summary,     # optimise by macro-F1
  allowParallel    = TRUE,
  savePredictions  = "final",
  seeds            = seeds,
  verboseIter      = FALSE
)

#-----------------------------------------
# 7) Train: caret will call mda::fda(method = earth, degree, nprune)
#-----------------------------------------
set.seed(20251016)
fit_fda <- caret::train(
  STATUS ~ INCOME + PREMIUM + SECTOR + PAYMENT_MODE + PAYER_GENDER +
    PRODUCT_GROUP + PAYER_AGE_GROUP + TERM + PAYER_MARITAL_STATUS +
    COMPANY_NAME + PAYPOINT_NAME,
  data      = train,
  method    = "fda",
  metric    = "Mean_F1",     # choose the macro-F1 for selection
  tuneGrid  = tune_grid,
  trControl = ctrl
)

print(fit_fda)
fit_fda$bestTune

#-----------------------------------------
# 8) Final refit on full (SMOTEd) training set with best hyperparameters
#-----------------------------------------
best_deg   <- fit_fda$bestTune$degree
best_npr   <- fit_fda$bestTune$nprune

set.seed(20251016)
marsfit_insurance_best <- mda::fda(
  STATUS ~ INCOME + PREMIUM + SECTOR + PAYMENT_MODE + PAYER_GENDER +
    PRODUCT_GROUP + PAYER_AGE_GROUP + TERM + PAYER_MARITAL_STATUS +
    COMPANY_NAME + PAYPOINT_NAME,
  data   = train,
  method = earth,
  degree = best_deg,
  nprune = best_npr
)

#-----------------------------------------
# 9) Optional: quick hold-out performance on the untouched test set
#-----------------------------------------
# Predicted class labels
pred_test <- predict(marsfit_insurance_best, newdata = test, type = "class")
cm_test   <- caret::confusionMatrix(pred_test, test$STATUS)
print(cm_test)

# Macro-F1 on test to mirror the CV metric
byClass <- cm_test$byClass
if (is.matrix(byClass)) {
  precision <- byClass[, "Precision"]
  recall    <- byClass[, "Recall"]
  f1        <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  cat(sprintf("\nMacro-F1 (test): %.4f\n", mean(f1, na.rm = TRUE)))
} else {
  precision <- byClass["Precision"]; recall <- byClass["Recall"]
  cat(sprintf("\nMacro-F1 (test): %.4f\n", 2 * precision * recall / (precision + recall)))
}



saveRDS(object = train, file = "train_hyperparmtune.rds")
saveRDS(object = test, file = "test_hyperparmtune.rds")
saveRDS(object = marsfit_insurance_best , file = "fdamars_marsfit_insurance_best_hyperparmtune.rds")
saveRDS(object = fit_fda , file = "fit_fda_hyperparmtune.rds")


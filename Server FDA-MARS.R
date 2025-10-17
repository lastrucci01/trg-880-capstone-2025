clean_selected_collapsed_grouped <- readRDS("~/trg-880-capstone-2025/clean_selected_collapsed_grouped_Ethan.rds")
summary(clean_selected_collapsed_grouped)


library(rsample)
library(forcats)
library(caret)
library(dplyr)


# 70% train, 30% test; stratified on STATUS
# spl  <- initial_split(clean_selected_collapsed_grouped, prop = 0.05, strata = STATUS)
# train <- training(spl)
# test  <- testing(spl)


#idx  <- caret::createDataPartition(clean_selected_collapsed_grouped$STATUS, p = 0.70, list = FALSE)
# train <- clean_selected_collapsed_grouped[idx, ]
# test  <- clean_selected_collapsed_grouped[-idx, ]

train <- clean_selected_collapsed_grouped
train <- themis::smotenc(train, var = "STATUS", k = 5, over_ratio = 1)



nrow(train) / nrow(clean_selected_collapsed_grouped)*100
#nrow(test)/nrow(clean_selected_collapsed_grouped)*100

summary(train$STATUS)/ sum(summary(train$STATUS))*100
#summary(test$STATUS) / sum(summary(test$STATUS))*100

summary(clean_selected_collapsed_grouped$STATUS)/length(clean_selected_collapsed_grouped$STATUS)*100

#PS_Lapse1 is near zero variance predictor so remove!

library(mda)
library(earth)

names(train[,caret::nearZeroVar(train)])



marsfit_insurance <- mda::fda( 
  STATUS ~ INCOME + PREMIUM + SECTOR + PAYMENT_MODE + PAYER_GENDER + PRODUCT_GROUP + PAYER_AGE_GROUP + TERM + PAYER_MARITAL_STATUS  + COMPANY_NAME +PAYPOINT_NAME
  , data   =  train,
  method = earth
  , nprune = 50
  , degree = 2
)


plot(marsfit_insurance, data = train, group = "true")
#plot(marsfit_insurance, data = test , group = "pred")


marsfit_insurance$percent.explained
marsfit_insurance$fit$coefficients


saveRDS(object = train, file = "train.rds")
saveRDS(object = marsfit_insurance , file = "fdamars_degree_2_nprune_50_full_data.rds")



caret::confusionMatrix(marsfit_insurance$confusion)


caret::confusionMatrix(marsfit_insurance$confusion)


yhat_train <- predict(marsfit_insurance, newdata = train, type = "class")
#yhat_test  <- predict(marsfit_insurance, newdata = test,  type = "class")



cm_train <- table(Observed = train$STATUS, Predicted = yhat_train)
#cm_test  <- table(Observed = test$STATUS,  Predicted = yhat_test)


conf_train <- caret::confusionMatrix(cm_train)
# conf_test <- caret::confusionMatrix(cm_test)
# conf_test$byClass



# conf_mat <- cvms::confusion_matrix(targets = test$STATUS,
#                                    predictions = yhat_test)
# cvms::plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])

# 
# summary(train$STATUS)/ sum(summary(train$STATUS))*100
# summary(test$STATUS) / sum(summary(test$STATUS))*100




#cm <- caret::confusionMatrix(data = yhat_test, reference = test$STATUS, mode = "prec_recall")
# Then a single multiclass object has byClass as a matrix with columns "Precision" and "Recall".
macro_f1_from_byClass <- function(cm_obj) {
  bc <- cm_obj$byClass
  P  <- bc[, "Precision"]
  R  <- bc[, "Recall"]
  F1 <- 2 * P * R / (P + R)
  mean(F1, na.rm = TRUE)
}

macro_f1_from_byClass(conf_train)

#----


library(pROC)
library(yardstick)
library(dplyr)
library(tibble)
library(purrr)


# ---- Predicted class probabilities (required for AUCs) ----
# For Hand–Till and pairwise AUCs we need per-class probabilities.
# mda::fda supports type = "posterior" for class membership probabilities.
p_train <- as.data.frame(predict(marsfit_insurance, newdata = train, type = "posterior"))
p_test  <- as.data.frame(predict(marsfit_insurance, newdata = test,  type = "posterior"))

# Ensure columns are in the same order as factor levels
classes <- levels(train$STATUS)
p_train <- p_train[, classes, drop = FALSE]
p_test  <- p_test[,  classes, drop = FALSE]

# ---- Hand–Till generalised AUC and pairwise AUCs (one-vs-one) ----
# Hand–Till M = average over all unordered pairs {i,j} of 0.5*(A(i|i,j)+A(j|j,i)).
hand_till_auc <- function(truth, probs) {
  lvls <- levels(truth)
  K <- length(lvls)
  stopifnot(all(colnames(probs) %in% lvls), K >= 2)
  
  combs <- combn(lvls, 2, simplify = FALSE)
  
  pairwise_tbl <- map_dfr(
    combs,
    function(pair_ij) {
      i <- pair_ij[1]; j <- pair_ij[2]
      # Subset to the two classes
      idx_ij <- truth %in% c(i, j)
      y_ij   <- droplevels(truth[idx_ij])
      
      # A(i | i,j): use probability of class i as the score; positive = i
      roc_i <- roc(response = factor(y_ij, levels = c(j, i)), predictor = probs[idx_ij, i], quiet = TRUE)
      A_i   <- as.numeric(auc(roc_i))
      
      # A(j | j,i): use probability of class j as the score; positive = j
      roc_j <- roc(response = factor(y_ij, levels = c(i, j)), predictor = probs[idx_ij, j], quiet = TRUE)
      A_j   <- as.numeric(auc(roc_j))
      
      tibble(class_i = i, class_j = j, A_i_given_ij = A_i, A_j_given_ji = A_j, A_pair = 0.5*(A_i + A_j))
    }
  )
  
  list(
    hand_till_auc = mean(pairwise_tbl$A_pair),
    pairwise = pairwise_tbl
  )
}

# Compute Hand–Till and pairwise AUCs for train and test
ht_train <- hand_till_auc(truth = train$STATUS, probs = p_train)
ht_test  <- hand_till_auc(truth = test$STATUS,  probs = p_test)

# Access results:
# ht_train$hand_till_auc  # scalar Hand–Till AUC (train)
# ht_train$pairwise       # tibble of pairwise AUCs (train)
# ht_test$hand_till_auc   # scalar Hand–Till AUC (test)
# ht_test$pairwise        # tibble of pairwise AUCs (test)

# ---- Micro- and Macro- F1 scores ----
# Helper to compute per-class F1 from a confusion matrix
per_class_f1 <- function(cm) {
  # cm: table with rows = Observed (truth), cols = Predicted
  lvls <- rownames(cm)
  sapply(lvls, function(k) {
    TP <- cm[k, k]
    FP <- sum(cm[-match(k, lvls), k])
    FN <- sum(cm[k, -match(k, lvls)])
    if ((2*TP + FP + FN) == 0) return(NA_real_)
    2 * TP / (2 * TP + FP + FN)
  })
}

macro_f1_from_cm <- function(cm) {
  f1s <- per_class_f1(cm)
  mean(f1s, na.rm = TRUE)
}

micro_f1_from_cm <- function(cm) {
  # Micro-averaged F1 via global TP, FP, FN
  TP <- sum(diag(cm))
  FP <- sum(colSums(cm)) - TP
  FN <- sum(rowSums(cm)) - TP
  2 * TP / (2 * TP + FP + FN)
}

# Train F1s
macro_f1_train <- macro_f1_from_cm(cm_train)
micro_f1_train <- micro_f1_from_cm(cm_train)

# Test F1s
macro_f1_test <- macro_f1_from_cm(cm_test)
micro_f1_test <- micro_f1_from_cm(cm_test)

# ---- (Optional) yardstick cross-check on F1s ----
# Convert to data frames for yardstick, then compute macro/micro F1 to verify.
yard_train <- tibble(truth = factor(train$STATUS, levels = classes),
                     estimate = factor(yhat_train, levels = classes))
yard_test  <- tibble(truth = factor(test$STATUS,  levels = classes),
                     estimate = factor(yhat_test,  levels = classes))

macro_f1_train_yard <- f_meas(yard_train, truth = truth, estimate = estimate, estimator = "macro")$.estimate
micro_f1_train_yard <- f_meas(yard_train, truth = truth, estimate = estimate, estimator = "micro")$.estimate
macro_f1_test_yard  <- f_meas(yard_test,  truth = truth, estimate = estimate, estimator = "macro")$.estimate
micro_f1_test_yard  <- f_meas(yard_test,  truth = truth, estimate = estimate, estimator = "micro")$.estimate

# ---- Neat printout ----
cat("\n===== TRAIN =====\n")
cat(sprintf("Hand–Till AUC (generalised): %.4f\n", ht_train$hand_till_auc))
print(ht_train$pairwise)

cat("\nMacro F1 (train): ", round(macro_f1_train, 4), 
    " | Micro F1 (train): ", round(micro_f1_train, 4), "\n", sep = "")

cat("\n===== TEST =====\n")
cat(sprintf("Hand–Till AUC (generalised): %.4f\n", ht_test$hand_till_auc))
print(ht_test$pairwise)

cat("\nMacro F1 (test): ", round(macro_f1_test, 4), 
    " | Micro F1 (test): ", round(micro_f1_test, 4), "\n", sep = "")

cm 





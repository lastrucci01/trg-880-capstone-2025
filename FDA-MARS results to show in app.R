

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





train <- readRDS("~/Documents/Masters Math Stats/TRG880/Assignments/Group Assignment/trg-880-capstone-2025/train_70perc_SMOTED.rds")

test <- readRDS("~/Documents/Masters Math Stats/TRG880/Assignments/Group Assignment/trg-880-capstone-2025/test_30perc.rds")

final_FDA_MARS_model <- readRDS("~/Documents/Masters Math Stats/TRG880/Assignments/Group Assignment/trg-880-capstone-2025/final_FDA_MARS_model.rds")



options(scipen = 99999)

round(final_FDA_MARS_model$fit$coefficients,8) #coefficiens for each term in final model


 #confusion matrix for SMOTED train data



cm_train <- final_FDA_MARS_model$confusion

macro_f1_from_cm(cm_train )
micro_f1_from_cm(cm_train )

conf_train <- caret::confusionMatrix(cm_train , mode = "prec_recall")


yhat_test  <- predict(final_FDA_MARS_model, newdata = test,  type = "class")

cm_test<- table(Observed = test$STATUS,  Predicted = yhat_test)
conf_test <- caret::confusionMatrix(cm_test, mode = "prec_recall")

conf_test  #confusion matrix for test data

macro_f1_from_cm(cm_test)
micro_f1_from_cm(cm_test)


library(dplyr)
library(yardstick)
library(rlang)


p_test  <- as.data.frame(predict(final_FDA_MARS_model, newdata = test,  type = "posterior"))

p_train  <- as.data.frame(predict(final_FDA_MARS_model, newdata = train,  type = "posterior"))


# 1) Ensure the truth is a factor and drop unused levels (if any)
test <- test %>%
  mutate(STATUS = droplevels(factor(STATUS)))

# 2) Ensure probability column names match the levels of STATUS exactly
cls <- levels(test$STATUS)  # e.g., c("ACT","SUR","CAN_LAP")
stopifnot(all(cls %in% colnames(p_test)))

# 3) Build a single data frame with truth + probabilities in matching order
df_ht <- bind_cols(
  test %>% select(STATUS),
  as.data.frame(p_test)[, cls, drop = FALSE]  # enforce correct column order
)

# 4) Compute generalised Hand–Till AUC
ht_auc <- roc_auc(
  data      = df_ht,
  truth     = STATUS,          # bare column name, not test$STATUS
  !!!syms(cls),                # splice the class-probability columns
  estimator = "hand_till"
) %>%
  pull(.estimate) %>%
  .[[1]]

ht_auc #generalised Hand–Till AUC for test data



# Overlapped one-vs-rest ROC curves per class
# Train = blue, Test = red; facets by class; includes per-dataset AUC labels

library(dplyr)
library(ggplot2)
library(tidyr)
library(yardstick)
library(rlang)

# ---- Helper: bind truth + probs with column order = levels(truth) ----
prep_df <- function(data, probs, truth = "STATUS") {
  data <- data %>% mutate(!!truth := droplevels(factor(.data[[truth]])))
  cls  <- levels(data[[truth]])
  stopifnot(all(cls %in% colnames(probs)))
  dplyr::bind_cols(
    data %>% dplyr::select(all_of(truth)),
    as.data.frame(probs)[, cls, drop = FALSE]
  )
}

df_train <- prep_df(train, p_train, truth = "STATUS")
df_test  <- prep_df(test , p_test , truth = "STATUS")

# ---- One-vs-rest ROC points for a given dataset ----
roc_per_class <- function(df, truth = "STATUS", dataset_label = "Train") {
  cls <- levels(df[[truth]])
  dplyr::bind_rows(lapply(cls, function(cl) {
    df2 <- df %>%
      mutate(.truth_bin = factor(ifelse(.data[[truth]] == cl, cl, "other"),
                                 levels = c("other", cl)))  # positive = 'cl'
    yardstick::roc_curve(
      data        = df2,
      truth       = .truth_bin,
      !!sym(cl),
      event_level = "second"
    ) %>%
      mutate(class = cl,
             dataset = dataset_label,
             fpr = 1 - specificity) %>%
      dplyr::select(dataset, class, fpr, tpr = sensitivity, .threshold)
  }))
}

# ---- AUC per class for a given dataset (one-vs-rest) ----
auc_per_class <- function(df, truth = "STATUS", dataset_label = "Train") {
  cls <- levels(df[[truth]])
  dplyr::bind_rows(lapply(cls, function(cl) {
    df2 <- df %>%
      mutate(.truth_bin = factor(ifelse(.data[[truth]] == cl, cl, "other"),
                                 levels = c("other", cl)))
    a <- yardstick::roc_auc(
      data        = df2,
      truth       = .truth_bin,
      !!sym(cl),
      event_level = "second"
    ) %>% dplyr::pull(.estimate)
    tibble(dataset = dataset_label, class = cl, AUC = a)
  }))
}

# Compute ROC points and AUCs
roc_train <- roc_per_class(df_train, dataset_label = "Train")
roc_test  <- roc_per_class(df_test , dataset_label = "Test")
roc_all   <- dplyr::bind_rows(roc_train, roc_test)

auc_all <- dplyr::bind_rows(
  auc_per_class(df_train, dataset_label = "Train"),
  auc_per_class(df_test , dataset_label = "Test")
)

# Prepare AUC labels (two per facet; place non-overlapping)
auc_labels <- auc_all %>%
  mutate(x = 0.65,
         y = ifelse(dataset == "Train", 0.20, 0.12),
         lab = sprintf("%s AUC = %.3f", dataset, AUC))

# ---- Plot: overlap Train (blue) and Test (red) per class ----
p <- ggplot(roc_all, aes(x = fpr, y = tpr, colour = dataset)) +
  geom_path(linewidth = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~ class, ncol = length(unique(roc_all$class))) +
  coord_equal() +
  scale_colour_manual(values = c(Train = "blue", Test = "red")) +
  labs(
    title = "One-vs-rest ROC curves per class (Train vs Test)",
    x = "False positive rate (1 − specificity)",
    y = "True positive rate (sensitivity)",
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p + geom_text(data = auc_labels,
              aes(x = x, y = y, label = lab, colour = dataset),
              inherit.aes = FALSE, size = 3.6)



#new observation classification: 



# Helper to safely coerce factor levels to match training data
align_levels <- function(new_df, train_df) {
  for (nm in intersect(names(new_df), names(train_df))) {
    if (is.factor(train_df[[nm]])) {
      # coerce to factor with training levels; if unseen level, set to "Other" when available
      lv <- levels(train_df[[nm]])
      if (is.character(new_df[[nm]]) || is.factor(new_df[[nm]])) {
        val <- as.character(new_df[[nm]])
        if (!val %in% lv) {
          # try to map to "Other" if present; otherwise first level
          val <- if ("Other" %in% lv) "Other" else lv[1L]
        }
        new_df[[nm]] <- factor(val, levels = lv)
      } else {
        # if numeric but train is factor (rare), coerce to first level
        new_df[[nm]] <- factor(lv[1L], levels = lv)
      }
    }
  }
  new_df
}

new_obs <- data.frame(
  INCOME                     = 3500,     # median
  PS_LAPSE1                  = 78.60,    # modal quartile value
  SECTOR                     = "PROFESSIONAL/ENGINEERING/TECHNICAL",
  PAYMENT_MODE               = "RSO",    # most frequent
  PAYER_GENDER               = "FEMALE",
  INSURED_GENDER             = "FEMALE",
  PRODUCT_GROUP              = "FUNERAL", 
  PAYER_AGE_GROUP            = "26-35",  # largest group
  PREMIUM                    = 161.00,   # median
  TERM                       = 35,       # median
  PAYER_MARITAL_STATUS       = "S",      # largest
  INSURED_MARITAL_STATUS     = "S",      # largest
  COMPANY_NAME               = "COMP19", # most frequent
  OCCUPATION_DESC1           = "TEACHER",
  PAYPOINT_NAME              = "PAYPOINT224",  # most frequent
  PAYER_AGE_AT_COMMENCEMENT  = 32,       # median
  # STATUS is intentionally omitted for a test/prediction observation
  stringsAsFactors = FALSE
)

# ---- If a training frame exists, align factor levels to it
if (exists("train")) {
  new_obs <- align_levels(new_obs, train)
}


posterior_probs_new_observation <- predict(final_FDA_MARS_model, newdata = new_obs , type = "posterior" ) #posterior probabilities
posterior_probs_new_observation
predicted_class_new_observation  <- predict(final_FDA_MARS_model, newdata = new_obs , type = "class" ) 
predicted_class_new_observation 


library(reticulate)

# Set up Python environment
use_virtualenv("~/.virtualenvs/r-reticulate", required = TRUE)

# Import Python modules
joblib <- import("joblib")
np <- import("numpy")

# Load models
cat("Loading models...\n")
histgrad_model <- joblib$load("histgradboost_model.pkl")
label_encoder <- joblib$load("label_encoder.pkl")

# Debug label encoder
cat("Label encoder type:", class(label_encoder), "\n")
cat("Label encoder attributes:\n")
print(py_list_attributes(label_encoder))

# Try to access classes
tryCatch({
  classes <- label_encoder$classes_
  cat("Classes (direct access):", py_to_r(classes), "\n")
}, error = function(e) {
  cat("Error accessing classes_:", e$message, "\n")
})

# Test prediction with sample data
cat("\nTesting prediction...\n")

# Sample feature vector with 12 features matching training
# Order: INCOME, PS_LAPSE1, SECTOR, PAYMENT_MODE, PAYER_GENDER, PRODUCT_GROUP,
#        PAYER_AGE_GROUP, PREMIUM, TERM, PAYER_MARITAL_STATUS, COMPANY_NAME, PAYPOINT_NAME
feature_vector <- c(1500, 78.6, 1, 1, 1, 1, 2, 200, 10, 2, 1, 1)
cat("Feature vector:", feature_vector, "\n")

# Convert to numpy and reshape - SIMPLE VERSION
X_single <- r_to_py(matrix(feature_vector, nrow = 1))
cat("X_single shape:", paste(py_to_r(X_single$shape), collapse = " x "), "\n")

# Make prediction
pred <- histgrad_model$predict(X_single)
cat("Raw prediction:", py_to_r(pred), "\n")

# Get probabilities
prob <- histgrad_model$predict_proba(X_single)
cat("Probabilities:", prob, "\n")

# Try inverse transform
tryCatch({
  pred_int <- as.integer(py_to_r(pred))
  pred_label <- label_encoder$inverse_transform(r_to_py(array(pred_int)))
  cat("Predicted label:", py_to_r(pred_label), "\n")
}, error = function(e) {
  cat("Error in inverse_transform:", e$message, "\n")
})

cat("\nTest completed!\n")

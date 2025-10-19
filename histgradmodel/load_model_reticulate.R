# Load and Run HistGradientBoosting Model using reticulate (most reliable)
# Install required R packages if not already installed
required_packages <- c("reticulate", "jsonlite", "dplyr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("All R packages loaded successfully!\n")

# Configure Python environment
cat("Configuring Python environment...\n")

# Try to use system Python or create a conda environment
tryCatch({
  # Check if we can use system Python
  use_virtualenv("~/.virtualenvs/r-reticulate", required = TRUE)
}, error = function(e) {
  cat("System Python not found, trying other options...\n")
})

# Install required Python packages with multiple attempts
cat("Installing required Python packages...\n")
packages_to_install <- c("joblib", "scikit-learn", "pandas", "numpy")

for (pkg in packages_to_install) {
  cat("Installing", pkg, "...\n")
  tryCatch({
    py_install(pkg, pip = TRUE)
    cat(pkg, "installed successfully!\n")
  }, error = function(e) {
    cat("Failed to install", pkg, "via reticulate. Error:", e$message, "\n")
    cat("Please install manually in terminal: pip install", pkg, "\n")
  })
}

# Wait a moment for installations to complete
Sys.sleep(2)

# Import Python modules with error handling and retry
cat("Importing Python modules...\n")

# Try importing with retries
import_with_retry <- function(module_name, max_attempts = 3) {
  for (attempt in 1:max_attempts) {
    tryCatch({
      module <- import(module_name)
      cat(module_name, "imported successfully!\n")
      return(module)
    }, error = function(e) {
      if (attempt == max_attempts) {
        cat("Failed to import", module_name, "after", max_attempts, "attempts.\n")
        cat("Error:", e$message, "\n")
        cat("\nTroubleshooting steps:\n")
        cat("1. Install manually: pip install", module_name, "\n")
        cat("2. Check Python path: py_config()\n")
        cat("3. Restart R session\n")
        stop(paste("Cannot import", module_name))
      } else {
        cat("Attempt", attempt, "failed for", module_name, ", retrying...\n")
        Sys.sleep(1)
      }
    })
  }
}
library(reticulate)


# Import modules
joblib <- import_with_retry("joblib")
np <- import_with_retry("numpy")
pd <- import_with_retry("pandas")

cat("All Python modules imported successfully!\n")

# Load the saved model and label encoder with error handling
cat("Loading model and encoder...\n")

if (!file.exists("histgradboost_model.pkl")) {
  stop("Model file 'histgradboost_model.pkl' not found. Run the Python script first.")
}

if (!file.exists("label_encoder.pkl")) {
  stop("Label encoder file 'label_encoder.pkl' not found. Run the Python script first.")
}

tryCatch({
  model <- joblib$load("histgradboost_model.pkl")
  label_encoder <- joblib$load("label_encoder.pkl")
  cat("Model and encoder loaded successfully!\n")
}, error = function(e) {
  cat("Error loading model files:", e$message, "\n")
  stop("Cannot load model files.")
})

cat("Model loaded successfully!\n")
cat("Classes:", label_encoder$classes_, "\n")

# Function to make predictions
make_predictions <- function(X) {
  cat("Making predictions...\n")
  
  # Get predictions and probabilities
  predictions <- model$predict(X)
  probabilities <- model$predict_proba(X)
  
  # Convert to R and then to integer
  pred_r <- py_to_r(predictions)
  pred_int <- as.integer(pred_r)
  
  # Convert back to python for inverse transform
  pred_labels <- label_encoder$inverse_transform(r_to_py(pred_int))
  
  return(list(
    predictions = py_to_r(pred_labels),
    probabilities = py_to_r(probabilities),
    raw_predictions = pred_int
  ))
}

# Function to predict on new single observation
predict_single <- function(feature_vector) {
  # Convert to proper format
  X_single <- np$array(feature_vector)$reshape(1L, -1L)
  
  # Make prediction
  pred <- model$predict(X_single)
  prob <- model$predict_proba(X_single)
  
  # Return results
  return(list(
    prediction = py_to_r(label_encoder$inverse_transform(pred))[1],
    probability = py_to_r(prob)[1,],
    confidence = max(py_to_r(prob)[1,])
  ))
}

# Example usage
cat("\n=== Example Usage ===\n")

# Load test data (adjust path as needed)
data_path <- "/Users/richardlastrucci/Documents/university/msc/trg 880/capstone/trg-880-capstone-2025/clean_selected_collapsed_grouped_Ethan.csv"

if (file.exists(data_path)) {
  # Load data using R
  df_r <- read.csv(data_path)
  
  # Get feature columns (exclude STATUS)
  feature_cols <- names(df_r)[names(df_r) != "STATUS"]
  X_features <- df_r[feature_cols]
  
  # Convert categorical columns to numeric
  for(col in names(X_features)) {
    if(is.character(X_features[[col]]) || is.factor(X_features[[col]])) {
      X_features[[col]] <- as.numeric(as.factor(X_features[[col]]))
    }
  }
  
  # Convert to matrix and handle NAs
  X_r <- as.matrix(X_features)
  X_r[is.na(X_r)] <- 0
  
  # Convert to integer
  storage.mode(X_r) <- "integer"
  
  # Take sample and convert to numpy
  sample_size <- min(100, nrow(X_r))
  X_sample <- r_to_py(X_r[1:sample_size, ])
  
  # Make predictions
  results <- make_predictions(X_sample)
  
  # Display results
  cat("\nPrediction Results (first 10):\n")
  print(head(results$predictions, 10))
  
  cat("\nProbabilities (first 5):\n")
  print(head(results$probabilities, 5))
  
  # Summary statistics
  pred_table <- table(results$predictions)
  cat("\nPrediction Summary:\n")
  print(pred_table)
  
  cat("\nProbability Statistics:\n")
  cat("Class 0 - Mean:", mean(results$probabilities[,1]), "SD:", sd(results$probabilities[,1]), "\n")
  cat("Class 1 - Mean:", mean(results$probabilities[,2]), "SD:", sd(results$probabilities[,2]), "\n")
  
} else {
  cat("Data file not found. Please adjust the data_path variable.\n")
  cat("Current path:", data_path, "\n")
}

cat("\n=== Model Information ===\n")
cat("Model type: HistGradientBoostingClassifier\n")
cat("Classes:", label_encoder$classes_, "\n")
cat("Number of features:", model$n_features_in_, "\n")

cat("\nR script completed successfully!\n")
cat("Use predict_single(feature_vector) for single predictions\n")
cat("Use make_predictions(X_matrix) for batch predictions\n")
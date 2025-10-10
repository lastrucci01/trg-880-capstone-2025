library(dplyr)
library(lubridate)
library(readxl)

source("index.R")

# Finds the location of the Excel file based on this files location
get_data_script_path <- function() {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file.path(script_dir, "..", "data", "processed_data_with_sector")
}

# Opens and reads the Excel file
load_data <- function() {
    filepath <- get_data_script_path()
    df <- read_excel(filepath)
    return(df)
}

data_cleaning_pipeline <- function(df) {
    print("Standardizing text fields and renaming columns")
    df <- normalise_data(df)

    print("Removing duplicate records")
    df <- remove_duplicates(df)

    print("Mapping product codes to readable names")
    df <- map_product_code(df)

    print("Calculating payer age at policy commencement")
    df <- calculate_payer_age_at_commencement(df)

    print("Handling negative age values")
    df <- handle_negative_ages(df)

    print("Grouping ages into categorical bins")
    df <- bin_ages_by_categories(df)

    print("Cleaning and standardizing gender values")
    df <- handle_gender(df)

    print("Cleaning and correcting title values")
    df <- handle_title(df)

    print("Cleaning marital status values")
    df <- handle_marital_status(df)

    print("Cleaning payment mode values")
    df <- handle_payment_mode_unknown_pls(df)
    
    print("Cleaning term, payfreq, premium, income, ps_lapse1")
    df <- clean_missing_values(df)

    return(df)
}

df <- load_data()
df <- data_cleaning_pipeline(df)
# print(head(df$PAYER_AGE_GROUP, 10))

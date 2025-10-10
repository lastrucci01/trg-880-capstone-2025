library(dplyr)
library(lubridate)
library(readxl)

source("index.R")

# Finds the location of the Excel file based on this files location
get_data_script_path <- function() {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file.path(script_dir, "..", "data")
}

# Opens and reads the Excel file
load_data <- function() {
    filepath <- get_data_script_path()
    filepath <- file.path(filepath, "processed_data_with_sectors.xlsx")
    df <- read_excel(filepath)
    return(df)
}

data_cleaning_pipeline <- function(df) {

    print("Cleaning sector mapping")
    df <- clean_sector_mapping(df)

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
    df <- convert_payment_mode_to_factor(df)
    
    # print("Cleaning term, payfreq, premium, income, ps_lapse1") missing ... 
    # df <- clean_missing_values(df)

    print("Handling income outliers")
    df <- handle_income(df)

    print("Filtering payment frequency")
    df <- handle_payfreq(df)

    print("Cleaning term values")
    df <- handle_term(df)

    print("Handling premium outliers")
    df <- handle_premium(df)
    
    print("Handling policy lapse outliers")
    df <- handle_ps_lapse1(df)

    return(df)
}

df <- load_data()
df <- data_cleaning_pipeline(df)

# Save cleaned data to CSV in same directory as source data
FILE_NAME <- "cleaned_data.csv" # CHANGE THIS TO THE NAME OF THE FILE 
output_path <- file.path(dirname(get_data_script_path()), FILE_NAME)
write.csv(df, output_path, row.names = FALSE)
print(paste("Data saved to", output_path))

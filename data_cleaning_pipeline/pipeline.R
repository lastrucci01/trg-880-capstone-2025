library(dplyr)
library(lubridate)
library(readxl)

source("../richard/data_cleaning_functions/calculate_payer_age_at_commencement.R")
source("../richard/data_cleaning_functions/map_product_code.R")

get_data_script_path <- function() {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file.path(script_dir, "..", "data", "SUMMARY12025.xlsx")
}

load_data <- function() {
    filepath <- get_data_script_path()
    df <- read_excel(filepath)
    return(df)
}

data_cleaning_pipeline <- function(df) {
    df <- map_product_code(df)
    df <- calculate_payer_age_at_commencement(df)
    df <- handle_negative_ages(df)
    df <- bin_ages_by_categories(df)
    return(df)
}

df <- load_data()
df <- data_cleaning_pipeline(df)
head(df$PAYER_AGE_GROUP, 10)

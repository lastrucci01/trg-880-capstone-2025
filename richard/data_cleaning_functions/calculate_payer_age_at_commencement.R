library(dplyr)
library(lubridate)

# Calculate payer age at policy commencement date
calculate_payer_age_at_commencement <- function(df) {
  df %>%
    mutate(
      start_date = as.Date(PAYER_DATE_OF_BIRTH),
      end_date = as.Date(COMMENCEMENT_DATE),
      # Calculate age in years using interval division
      PAYER_AGE_AT_COMMENCEMENT = interval(start_date, end_date) %/% years(1)
    )
}

# Replace negative ages with product type mean
handle_negative_ages <- function(df) {
  df %>%
    group_by(PRODUCT_TYPE) %>%
    mutate(PAYER_AGE_AT_COMMENCEMENT = ifelse(
      PAYER_AGE_AT_COMMENCEMENT < 0,
      # Replace with mean age for same product type
      mean(PAYER_AGE_AT_COMMENCEMENT[PAYER_AGE_AT_COMMENCEMENT >= 0], na.rm = TRUE),
      PAYER_AGE_AT_COMMENCEMENT
    )) %>%
    ungroup()
}

# Create age group categories from continuous age values
bin_ages_by_categories <- function(df, age_brackets=c(-Inf, 17, 25, 35, 45, 55, 65, Inf), age_labels=c("<18", "18-25", "26-35", "36-45", "46-55", "56-65", "66+")) {
  # Cut ages into categorical groups
  df$PAYER_AGE_GROUP <- cut(
    df$PAYER_AGE_AT_COMMENCEMENT,
    breaks = age_brackets,
    labels = age_labels,
    right = TRUE
  )
  return(df)
}
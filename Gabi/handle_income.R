handle_income <- function(df, grouping = c("PAYER_GENDER", "PRODUCT_GROUP", "OCCUPATION_clean", "PAYER_AGE_GROUP")) {
  df$INCOME <- as.numeric(df$INCOME)
  
  # Compute overall median as fallback
  overall_median <- median(df$INCOME, na.rm = TRUE)
  
  # Create grouping key
  df$group_key <- do.call(paste, c(df[grouping], sep = "#"))
  
  df <- df %>%
    group_by(group_key) %>%
    mutate(
      Q1 = quantile(INCOME, 0.25, na.rm = TRUE),
      Q3 = quantile(INCOME, 0.75, na.rm = TRUE),
      IQR_val = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR_val,
      upper_bound = Q3 + 1.5 * IQR_val,
      is_outlier = (INCOME < lower_bound) | (INCOME > upper_bound),
      group_median = median(INCOME[!is_outlier], na.rm = TRUE),
      # Use overall median if group median is NA
      group_median = ifelse(is.na(group_median), overall_median, group_median),
      INCOME = ifelse(is.na(INCOME) | is_outlier, group_median, INCOME)
    ) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR_val, -lower_bound, -upper_bound, -is_outlier, -group_median, -group_key)
  
  return(df)
}

df <- handle_income(df)



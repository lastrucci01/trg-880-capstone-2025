handle_income <- function(df, grouping = c("PAYER_GENDER", "PRODUCT_GROUP", "SECTOR" , "PAYER_AGE_GROUP")) {
  df$INCOME <- as.numeric(df$INCOME)
  
  # Create grouping key
  df$group_key <- do.call(paste, c(df[grouping], sep = "#"))
  
  # Detect outliers and replace with group median
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
      # Replace NA or outlier with group median
      INCOME = ifelse(is.na(INCOME) | is_outlier, group_median, INCOME)
    ) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR_val, -lower_bound, -upper_bound, -is_outlier, -group_median, -group_key)
  
  return(df)
}

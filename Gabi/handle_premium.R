handle_premium <- function(df, group_cols = c("PAYER_AGE_GROUP", "PRODUCT_GROUP", "PAYER_GENDER", "OCCUPATION_clean")) {
  df <- df %>% mutate(PREMIUM = abs(PREMIUM))
  
  # Detect and replace outliers for PREMIUM using IQR within groups
  df <- df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      Q1 = quantile(PREMIUM, 0.25, na.rm = TRUE),
      Q3 = quantile(PREMIUM, 0.75, na.rm = TRUE),
      IQR_val = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR_val,
      upper_bound = Q3 + 1.5 * IQR_val,
      is_outlier = PREMIUM < lower_bound | PREMIUM > upper_bound,
      PREMIUM = ifelse(is_outlier, median(PREMIUM[!is_outlier], na.rm = TRUE), PREMIUM)
    ) %>%
    select(-Q1, -Q3, -IQR_val, -lower_bound, -upper_bound, -is_outlier) %>%
    ungroup()
  
  return(df)
}

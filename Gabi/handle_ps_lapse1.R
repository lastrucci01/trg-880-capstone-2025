handle_ps_lapse1<- function(df, group_cols = c("PAYER_AGE_GROUP", "PRODUCT_GROUP", "PAYER_GENDER", "OCCUPATION_clean")) {
  df <- df %>% mutate(ps_lapse1 = abs(ps_lapse1))
  
  # Detect and replace outliers for ps_lapse1 using IQR within groups
  df <- df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      Q1 = quantile(ps_lapse1, 0.25, na.rm = TRUE),
      Q3 = quantile(ps_lapse1, 0.75, na.rm = TRUE),
      IQR_val = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR_val,
      upper_bound = Q3 + 1.5 * IQR_val,
      is_outlier = ps_lapse1 < lower_bound | ps_lapse1 > upper_bound,
      ps_lapse1 = ifelse(is_outlier, median(ps_lapse1[!is_outlier], na.rm = TRUE), ps_lapse1)
    ) %>%
    select(-Q1, -Q3, -IQR_val, -lower_bound, -upper_bound, -is_outlier) %>%
    ungroup()
  
  return(df)
}


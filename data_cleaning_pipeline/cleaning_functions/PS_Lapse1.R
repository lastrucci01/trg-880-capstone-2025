handle_ps_lapse1 <- function(df, group_cols = c("PAYER_AGE_GROUP", "PRODUCT_GROUP", "PAYER_GENDER", "SECTOR")) {
  head(df$PS_LAPSE1)
  df <- df %>% mutate(PS_LAPSE1 = abs(PS_LAPSE1))
  
  # Detect and replace outliers for PS_LAPSE1 using IQR within groups
  df <- df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      Q1 = quantile(PS_LAPSE1, 0.25, na.rm = TRUE),
      Q3 = quantile(PS_LAPSE1, 0.75, na.rm = TRUE),
      IQR_val = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR_val,
      upper_bound = Q3 + 1.5 * IQR_val,
      is_outlier = PS_LAPSE1 < lower_bound | PS_LAPSE1 > upper_bound,
      PS_LAPSE1 = ifelse(is_outlier, median(PS_LAPSE1[!is_outlier], na.rm = TRUE), PS_LAPSE1)
    ) %>%
    select(-Q1, -Q3, -IQR_val, -lower_bound, -upper_bound, -is_outlier) %>%
    ungroup()
  
  return(df)
}

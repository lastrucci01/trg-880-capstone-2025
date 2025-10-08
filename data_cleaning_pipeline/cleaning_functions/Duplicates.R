library(dplyr)

remove_duplicates <- function(df)
{
  ignore_cols <- c("POLICY_NUMBER", "NBANK", "NLOANS", "EE")
  
  df %>%
    mutate(
      composite_key = apply(select(., -all_of(ignore_cols)), 1, paste, collapse = "_")
    ) %>%
    group_by(composite_key) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(-composite_key)
}

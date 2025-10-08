library(dplyr)

remove_duplicates <- function(df)
{
  ignore_cols <- c("POLICY_NUMBER", "NBANK", "NLOANS", "EE")
  
  new_df <- df %>%
    mutate(
      composite_key = apply(select(., -all_of(ignore_cols)), 1, paste, collapse = "_")
    ) %>%
    df_no_dups_composite <- df %>%
    group_by(composite_key) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(-composite_key)
  
  return(new_df)
}

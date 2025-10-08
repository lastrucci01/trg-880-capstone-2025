library(dplyr)

remove_duplicates <- function(df) 
{
  ignore_cols <- c("POLICY_NUMBER", "NBANK", "NLOANS", "EE")
  
    df %>%
    #Create composite key ignoring specified columns
      mutate(
        composite_key = apply(select(., -all_of(ignore_cols)), 1, paste, collapse = "_")
      ) %>%
      group_by(composite_key) %>%
      #Replace NBANK and NLOANS with max within the group
      mutate(
        NBANK = round(mean(NBANK, na.rm = TRUE)),
        NLOANS = max(NLOANS, na.rm = TRUE)
      ) %>%
      #Keep only the first row per group
      slice_head(n = 1) %>%
      ungroup() %>%
      select(-composite_key)
}

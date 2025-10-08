library(dplyr)

handle_marital_status <- function(df) 
{
  new_df <- df %>%
    mutate(
      across(
        contains("MARITAL_STATUS"),
        ~ replace(.x, .x == "" | is.na(.x)| .x == "N", "UNKNOWN") %>% 
          factor()
      )
    )
  
  return(new_df)
}

library(dplyr)

handle_payment_mode_unknown_pls <- function(df)
{
  new_df <- df %>% factor()
    
  return(new_df)
}
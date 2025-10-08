library(dplyr)

handle_payment_mode_unknown_pls <- function(df)
{
  df %>% mutate(PAYMENT_MODE = as.factor(PAYMENT_MODE))
}
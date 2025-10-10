library(dplyr)

convert_payment_mode_to_factor <- function(df)
{
  df %>% mutate(PAYMENT_MODE = as.factor(PAYMENT_MODE))
}
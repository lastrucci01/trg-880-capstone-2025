handle_payment_mode <- function(df) {
  df <- df %>%
    mutate(PAYMENT_MODE = str_to_upper(PAYMENT_MODE))
  
  return(df)
}
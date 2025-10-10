handle_payfreq <- function(df) {
  df <- df %>%
    filter(PAYFREQ != 147 & !is.na(PAYFREQ))
  
  return(df)
}

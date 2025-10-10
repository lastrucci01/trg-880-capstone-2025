handle_term <- function(df) {
  term_mode <- as.numeric(names(sort(table(df$TERM[df$TERM <= 100]), decreasing = TRUE)[1]))
  df <- df %>%
    mutate(TERM = ifelse(TERM > 100, term_mode, TERM))
  
  return(df)
}

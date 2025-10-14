# Income groups 
income_groups <- function(df, income_col = "INCOME") {
  df <- df %>%
    mutate(
      INCOME_group = cut(
        .data[[income_col]],
        breaks = c(0, 500, 1000, 5000, 10000, 50000, Inf),
        labels = c("0-500","500-1k","1k-5k","5k-10k","10k-50k","50k+"),
        include.lowest = TRUE
      )
    )
  return(df)
}

# Premium groups 
premium_groups <- function(df, premium_col = "PREMIUM") {
  df <- df %>%
    mutate(
      PREMIUM_group = cut(
        .data[[premium_col]],
        breaks = c(0, 100, 150, 200, 300, 500, 1000, 2000, 5000, Inf),
        labels = c("0-100","100-150","150-200","200-300","300-500",
                   "500-1k","1k-2k","2k-5k","5k+"),
        include.lowest = TRUE
      )
    )
  return(df)
}
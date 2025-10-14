# Income groups 
df <- df %>%
  mutate(INCOME_group = cut(
    INCOME,
    breaks = c(0, 500, 1000, 5000, 10000, 50000, Inf),
    labels = c("0-500","500-1k","1k-5k","5k-10k","10k-50k","50k+"),
    include.lowest = TRUE
  ))

library(dplyr)

map_product_code <- function(df) {
  df %>%
    mutate(
      PRODUCT_GROUP = factor(
        case_when(
          PRODUCT_CODE %in% c("A1", "A7", "A8", "A9", "A11") ~ "FUNERAL",
          PRODUCT_CODE %in% c("A2","A3","A4","A5","A6","A16","A18","A19","A20","A23") ~ "RISK",
          PRODUCT_CODE %in% c("A10","A13","A15","A26") ~ "INVEST",
          PRODUCT_CODE == "A12" ~ "HEALTH",
          TRUE ~ "UNKNOWN"
        )
      )
    )
}
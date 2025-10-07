library(dplyr)

handle_gender <- function(df)
{
  new_df <- df %>%
    mutate(
      PAYER_GENDER = if_else(
        PAYER_GENDER == "MFMFMF",
        case_when(
          PAYER_TITLE == "MR" ~ "MALE",
          PAYER_TITLE %in% c("MRS", "MISS", "MS") ~ "FEMALE",
          TRUE ~ NA_character_
        ),
        PAYER_GENDER
      )) %>%
    
    mutate(
      across(
        contains("GENDER"),
        ~ replace(.x, is.na(.x) | .x == "", "UNKNOWN")
      )) %>%
    
    mutate(
      across(
        contains("GENDER"),
        ~ factor(.x)
      ))
  
  return(new_df)
}

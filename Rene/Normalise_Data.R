library(dplyr)

normalise_data <- function(df)
{
  new_df <- df %>%
    mutate(
      across(
      where(is.character),
      ~ .x %>%               
        str_trim() %>%               
        str_to_upper() %>%           
        str_replace_all("\\.$", ""))) %>%
    
    rename_with(toupper) %>%
    rename(
      PAYER_ID_DOCUMENT_CODE = ID_DOCUMENT_CODE,
      PAYER_GENDER = GENDER,
      PAYER_MARITAL_STATUS = MARITAL_STATUS,
      PAYER_TITLE = TITLE,
      BROKER_DATE_OF_BIRTH = DATE_OF_BIRTH,
      BROKER_GENDER = GENDER1,
      BROKER_ID_DOCUMENT = ID_DOCUMENT
    )
  
  return(new_df)
}

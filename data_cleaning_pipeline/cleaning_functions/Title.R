library(dplyr)

#Make "" title "UNKNOWN
empty_TITLE_make_unknown <- function(df, TITLE_name)
{
  df %>%
    mutate(!!TITLE_name := ifelse(.data[[TITLE_name]] == "", "UNKNOWN", .data[[TITLE_name]]))
}

#If MALES is not REV, DR, HON, PROF, title should be MR
correct_MALE_TITLE <- function(df, TITLE_col, GENDER_col)
{
  male_titles_keep <- c("REV", "DR", "HON", "PROF", "MR")
  
  df %>%
    mutate(
      !!TITLE_col := ifelse(
        .data[[GENDER_col]] == "MALE" & !(.data[[TITLE_col]] %in% male_titles_keep),
        "MR",
        .data[[TITLE_col]]  
      )
    )
}

#If M/W make MRS, if S and title was MR, make MS
correct_FEMALE_TITLE <- function(df, TITLE_col, MARITAL_col, GENDER_col)
{
  df %>%
    mutate(
      !!TITLE_col := case_when(
        .data[[GENDER_col]] == "FEMALE" & .data[[MARITAL_col]] %in% c("M", "W") ~ "MRS",
        .data[[GENDER_col]] == "FEMALE" & .data[[MARITAL_col]] == "S" & 
          .data[[TITLE_col]] == "MR" ~ "MS",
        TRUE ~ .data[[TITLE_col]]
      )
    )
}   

#Final clean TITLE function
handle_title <- function(df, TITLE_cols = c("PAYER_TITLE", "INSURED_TITLE"), 
                         MARITAL_col_prefix = c("PAYER_MARITAL_STATUS", "INSURED_MARITAL_STATUS"), 
                         GENDER_col_prefix = c("PAYER_GENDER", "INSURED_GENDER")) 
{
  new_df <- df 
  
  #Loop over each TITLE column
  for (i in seq_along(TITLE_cols)) 
    {
      TITLE_col <- TITLE_cols[i]
      MARITAL_col <- MARITAL_col_prefix[i]
      GENDER_col <- GENDER_col_prefix[i]
      
      new_df <- new_df %>%
        empty_TITLE_make_unknown(TITLE_col) %>%               
        correct_MALE_TITLE(TITLE_col, GENDER_col) %>%  
        correct_FEMALE_TITLE(TITLE_col, MARITAL_col, GENDER_col)  
    }
  
  new_df %>%
    mutate(across(all_of(TITLE_cols), factor))
}


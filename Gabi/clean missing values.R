# Clean missing values

clean_missing_values <- function(df) {
  # ----- TERM -----
  term_mode <- as.numeric(names(sort(table(df$TERM[df$TERM <= 100]), decreasing = TRUE)[1]))
  df <- df %>%
    mutate(TERM = ifelse(TERM > 100, term_mode, TERM))
  
  # ----- PAYFREQ -----
  df <- df %>%
    filter(PAYFREQ != 147 & !is.na(PAYFREQ) & PAYFREQ != "") %>%
    mutate(PAYFREQ = as.factor(PAYFREQ))
  
  # ----- PREMIUM -----
  df <- df %>%
    mutate(PREMIUM = abs(PREMIUM))
  premium_median <- median(df$PREMIUM, na.rm = TRUE)
  df <- df %>%
    mutate(PREMIUM = ifelse(is.na(PREMIUM) | PREMIUM == 0, premium_median, PREMIUM))
  
  # ----- INCOME -----
  df$INCOME <- gsub("[,$ ]", "", df$INCOME)
  df$INCOME<- as.numeric(df$INCOME)
  income_outlier <- df$INCOME > (mean(df$INCOME, na.rm = TRUE) + 4*sd(df$INCOME, na.rm = TRUE))
  income_median <- median(df$INCOME[!income_outlier & !is.na(df$INCOME)], na.rm = TRUE)
  df <- df %>%
    mutate(INCOME = ifelse(is.na(INCOME) | income_outlier, income_median, INCOME))
  
  # ----- PAYMENT_MODE -----
  df <- df %>%
    mutate(PAYMENT_MODE = str_to_upper(PAYMENT_MODE))
  
  # ----- ps_lapse1 -----
  lapse_median <- median(df$ps_lapse1[df$ps_lapse1 <= 100], na.rm = TRUE)
  df <- df %>%
    mutate(ps_lapse1 = abs(ps_lapse1),
           ps_lapse1 = ifelse(ps_lapse1 > 100, lapse_median, ps_lapse1))
  
  return(df)
}

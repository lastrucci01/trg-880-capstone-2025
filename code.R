# CAPSTONE ASSIGNMENT

library(readxl)
library(haven)
library(writexl)
library(skimr)
library(dplyr)
library(stringr)
library(lubridate)

data <- read_sas("C:/Users/Gab/OneDrive - University of Pretoria/Documents/TRG880/Capstone assignment/summary12025.sas7bdat")

# Convert sas7bdat to excel 
write_xlsx(data, "C:/Users/Gab/OneDrive - University of Pretoria/Documents/TRG880/Capstone assignment/capstone_data.xlsx")

# ---------- Data description -----------
 
skim(data)
str(data) 

colSums(is.na(data))


# ----- Duplicated data -----





# ----- Remove unwanted STATUS values -----
data <- data %>%
  filter(!STATUS %in% c("NFS", "NUL", "PUP", "MAT", "TRM"))%>%
  mutate(
    STATUS = str_to_upper(STATUS),
    STATUS = case_when(
      STATUS == "LAP" ~ "LAP",
      STATUS == "ACT" ~ "ACT",
      TRUE ~ STATUS))
   
table(data$STATUS)


# ------ Future dates ------
date_cols <- c("STATUS_DATE", "COMMENCEMENT_DATE", "PAYER_DATE_OF_BIRTH",
               "INSURED_DATE_OF_BIRTH", "DATE_OF_BIRTH")
data <- data %>%
  mutate(across(all_of(date_cols), ~ as.Date(.)))
future_counts <- sapply(data[date_cols], function(x) sum(x > Sys.Date(), na.rm = TRUE))

future_counts
sum(future_counts)


# ----- Create Payer_age col -----
data <- data %>%
  mutate(
    PAYER_DATE_OF_BIRTH = as.Date(PAYER_DATE_OF_BIRTH),      
    PAYER_AGE = year(Sys.Date()) - year(PAYER_DATE_OF_BIRTH))
summary(data$PAYER_AGE)





# ----- Convert dates ------
data$COMMENCEMENT_DATE <- as.Date(data$COMMENCEMENT_DATE, format="%Y-%m-%d")
data$STATUS_DATE <- as.Date(data$STATUS_DATE, format="%Y-%m-%d")
data$DATE_OF_BIRTH <- as.Date(data$DATE_OF_BIRTH, format="%Y-%m-%d")


# ------ Convert to numeric/character -------
data$INCOME <- gsub("[,$ ]", "", data$INCOME)
data$INCOME <- as.numeric(data$INCOME)



# ----- Outliers and missing values ------

# Occupation code - has many NAS, convert strings like "NA", "N/A", "" to actual NA
data$OCCUPATION_CODE <- ifelse(data$OCCUPATION_CODE %in% c("NA", "N/A", ""), NA, data$OCCUPATION_CODE)
data$OCCUPATION_CODE <- as.factor(data$OCCUPATION_CODE)

data <- data %>%
  filter(!is.na(OCCUPATION_CODE) & OCCUPATION_CODE != "")
table(is.na(data$OCCUPATION_CODE))


# Occupation_desc1 - need to decide how to bin it
top_jobs <- data %>%
  group_by(OCCUPATION_DESC1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

occ_desc_counts <- data %>%
  group_by(OCCUPATION_DESC1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

unemployed_jobs <- data %>%
  filter(str_detect(OCCUPATION_DESC1, regex("unemployed|not working|student", ignore_case = TRUE))) %>%
  group_by(OCCUPATION_DESC1) %>%
  summarise(n_policies = n()) %>%
  arrange(desc(n_policies))


# Change to employed or unemployed
data <- data %>%
  mutate(OCC_GROUP = case_when(
    str_detect(OCCUPATION_DESC1, regex("Unemployed|Not working", ignore_case = TRUE)) ~ "Unemployed",
    TRUE ~ "Employed"
  ))
data$OCC_GROUP <- as.factor(data$OCC_GROUP)
table(data$OCC_GROUP)

table(is.na(data$OCCUPATION_CODE))



# Term - has very high values that are unrealistic, replace > 100 values with mode.
hist(data$TERM, breaks = 50, main = "Policy Term Distribution", xlab = "Years")
summary(data$TERM)
sum(data$TERM > 100, na.rm = TRUE) # how many terms are above 100

term_mode <- as.numeric(names(sort(table(data$TERM[data$TERM <= 100]), decreasing = TRUE)[1]))
data <- data %>%
  mutate(TERM = ifelse(TERM > 100, term_mode, TERM))
summary(data$TERM)



# Payfreq - has counts of 147 - maybe a code for Unknown, make NA
hist(data$PAYFREQ, breaks = 50, main = "Pay Freq Distribution", xlab = "Freq")
summary(data$PAYFREQ)

data <- data %>%
  mutate(PAYFREQ = ifelse(PAYFREQ == 147, NA, PAYFREQ))
summary(data$PAYFREQ)




# Premium - should have only positive values, remove - signs. Extreme values are unrealistic 
# relative to income, term and occupation. Replace all with median.
hist(data$PREMIUM, breaks = 50, main = "Premium Distribution", xlab = "Freq")
summary(data$PREMIUM)
sum(is.na(data$PREMIUM) | data$PREMIUM == "")

premium_median <- median(data$PREMIUM[data$PREMIUM <= (mean(data$PREMIUM, na.rm = TRUE) + 4*sd(data$PREMIUM, na.rm = TRUE))], na.rm = TRUE)
data <- data %>%
  mutate(
    PREMIUM = abs(PREMIUM),
    PREMIUM = ifelse(
      PREMIUM > (mean(PREMIUM, na.rm = TRUE) + 4*sd(PREMIUM, na.rm = TRUE)),
      premium_median,        
      PREMIUM))
summary(data$PREMIUM)



# Income - should be >=0, unrealistic incomes for certain jobs. Replace all with mean.
hist(data$INCOME, breaks = 50, main = "Income Distribution", xlab = "Freq")
summary(data$INCOME)

income_outlier <- data$INCOME > (mean(data$INCOME, na.rm = TRUE) + 4*sd(data$INCOME, na.rm = TRUE))
income_median <- median(data$INCOME[!income_outlier & !is.na(data$INCOME)], na.rm = TRUE)
data <- data %>%
  mutate(INCOME = ifelse(is.na(INCOME) | income_outlier, income_median, INCOME))
summary(data$INCOME)


# Payment mode - typos are outliers, change PLS to NA then replace with mode ? and change Add to ADD 
# (should only be ADD, ASO, DSO and RSO)
table(data$PAYMENT_MODE)
sum(is.na(data$PAYMENT_MODE) | data$PAYMENT_MODE == "")

data <- data %>%
  mutate(
    PAYMENT_MODE = str_to_upper(PAYMENT_MODE),
    PAYMENT_MODE = ifelse(PAYMENT_MODE == "PLS", "NA", PAYMENT_MODE)
  )
table(data$PAYMENT_MODE)




# Marital status - should only be D, M, N (unknown), S, W. Replace N and "" with Unknown and map Mrs to M for females.
table(data$MARITAL_STATUS)
data <- data %>%
  mutate(
    MARITAL_STATUS = as.character(MARITAL_STATUS),
    MARITAL_STATUS = ifelse(MARITAL_STATUS %in% c("", "N") & TITLE == "MRS" & GENDER == "F",
                            "M", MARITAL_STATUS),
    MARITAL_STATUS = ifelse(MARITAL_STATUS %in% c("", "N"),
                            "NA", MARITAL_STATUS),
    MARITAL_STATUS = factor(MARITAL_STATUS,
                            levels = c("M","S","D","W","NA")))
table(data$MARITAL_STATUS)
unique(data$MARITAL_STATUS)



# Insured marital status - should only be D, M, N, S, W
table(data$INSURED_MARITAL_STATUS)




# Payer_dateofbirth - only ages between 18 and 100,
summary(data$PAYER_DATE_OF_BIRTH)


# Gender - typos like "MF"
table(data$GENDER)


# ps_lapse1 - should be 0–100%, all positive. Take out - sign, replace values > 100 with median.
hist(data$ps_lapse1, breaks = 50, main = "Lapsed Distribution", xlab = "Freq")
summary(data$ps_lapse1)

lapse_median <- median(data$ps_lapse1[data$ps_lapse1 <= 100], na.rm = TRUE)
data <- data %>%
  mutate(ps_lapse1 = abs(ps_lapse1)) %>%
  mutate(ps_lapse1 = ifelse(ps_lapse1 > 100, lapse_median, ps_lapse1))

summary(data$ps_lapse1)




# nbank - must be positive, 0-5 is realistic
hist(data$nbank, breaks = 50, main = "No. Bank Accounts Distribution", xlab = "Freq")
table(data$nbank)




# nloans - must be positive, 0-10 is realistic
hist(data$nloans, breaks = 50, main = "No. Loans Distribution", xlab = "Freq")
table(data$nloans)




# ------ Chi-square test for significance -------



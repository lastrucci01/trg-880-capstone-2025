# CAPSTONE ASSIGNMENT

library(readxl)
library(haven)
library(writexl)
library(skimr)
library(dplyr)
library(stringr)

data <- read_sas("C:/Users/Gab/OneDrive - University of Pretoria/Documents/TRG880/Capstone assignment/summary12025.sas7bdat")

# Convert sas7bdat to excel 
write_xlsx(data, "C:/Users/Gab/OneDrive - University of Pretoria/Documents/TRG880/Capstone assignment/capstone_data.xlsx")

# ---------- Data description -----------
 
skim(data)
str(data) 

table(is.na(data))
colSums(is.na(data))


# Convert dates
data$COMMENCEMENT_DATE <- as.Date(data$COMMENCEMENT_DATE, format="%Y-%m-%d")
data$STATUS_DATE <- as.Date(data$STATUS_DATE, format="%Y-%m-%d")
data$DATE_OF_BIRTH <- as.Date(data$DATE_OF_BIRTH, format="%Y-%m-%d")

unique(data$PAYMENT_MODE)
table(unique(data$GENDER))
unique(data$MARITAL_STATUS)
unique(data$TITLE)

summary(data[, c("PREMIUM", "INCOME", "TERM", "nloans")])

# AGE check
data$AGE <- as.numeric(difftime(Sys.Date(), data$DATE_OF_BIRTH, units = "days")) %/% 365
summary(data$AGE)


# Convert to numeric/character
data$INCOME <- gsub("[,$ ]", "", data$INCOME)
data$INCOME <- as.numeric(data$INCOME)


# ----- Remove unwanted STATUS values -----
data <- data %>%
filter(!STATUS %in% c("NFS", "NUL", "PUP"))
table(data$STATUS)



# ----- Occupation code has 15194 NAS - convert strings like "NA", "N/A", "" to actual NA
data$OCCUPATION_CODE <- ifelse(data$OCCUPATION_CODE %in% c("NA", "N/A", ""), NA, data$OCCUPATION_CODE)
data$OCCUPATION_CODE <- as.factor(data$OCCUPATION_CODE)

data <- data %>%
  filter(!is.na(OCCUPATION_CODE) & OCCUPATION_CODE != "")
table(is.na(data$OCCUPATION_CODE))


# ------- Group occupation_desc1 ------
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



# ----- Outliers ------

# Term - has very high values that are unrealistic, replace > 100 values with mode.
hist(data$TERM, breaks = 50, main = "Policy Term Distribution", xlab = "Years")
summary(data$TERM)
sum(data$TERM > 100, na.rm = TRUE) # how many terms are above 100

term_mode <- as.numeric(names(sort(table(data$TERM[data$TERM <= 100]), decreasing = TRUE)[1]))
data <- data %>%
  mutate(TERM = ifelse(TERM > 100, term_mode, TERM))
summary(data$TERM)



# Payfreq has 537 counts of 147 - maybe a code for Unknown, remove 147
hist(data$PAYFREQ, breaks = 50, main = "Pay Freq Distribution", xlab = "Freq")
summary(data$PAYFREQ)

data <- data %>%
  filter(PAYFREQ != 147)
table(data$PAYFREQ)


# Premium - should have only positive values, remove - signs. Extreme values are unrealistic 
# relative to income, term and occupation. Replace all with mean.
hist(data$PREMIUM, breaks = 50, main = "Premium Distribution", xlab = "Freq")
summary(data$PREMIUM)

data <- data %>%
  mutate(
    PREMIUM = abs(PREMIUM),
    PREMIUM = ifelse(PREMIUM > (mean(PREMIUM, na.rm = TRUE) + 4*sd(PREMIUM, na.rm = TRUE)),
                     mean(PREMIUM[PREMIUM <= (mean(PREMIUM, na.rm = TRUE) + 4*sd(PREMIUM, na.rm = TRUE))], na.rm = TRUE),
                     PREMIUM)
  )
summary(data$PREMIUM)



# Income - should be >=0, unrealistic incomes for certain jobs. Replace all with mean.
hist(data$INCOME, breaks = 50, main = "Income Distribution", xlab = "Freq")
summary(data$INCOME)

income_outlier <- data$INCOME > (mean(data$INCOME, na.rm = TRUE) + 4*sd(data$INCOME, na.rm = TRUE))
data <- data %>%
  mutate(INCOME = ifelse(is.na(INCOME) | income_outlier, mean(data$INCOME, na.rm = TRUE), INCOME))
summary(data$INCOME)


# Payment mode - typos are outliers, remove PLS and change Add to ADD (should only be ADD, ASO, DSO and RSO)
hist(data$PAYMENT_MODE, breaks = 50, main = "Payment Mode Distribution", xlab = "Freq")
table(data$PAYMENT_MODE)

data <- data %>%
  mutate(PAYMENT_MODE = str_to_upper(PAYMENT_MODE),
         PAYMENT_MODE = ifelse(PAYMENT_MODE == "ADD", "ADD", PAYMENT_MODE)) %>%
  filter(PAYMENT_MODE != "PLS")
table(data$PAYMENT_MODE)


# Status date - outliers are dates before company existed, or in the far future.



# Commencement date -



# Marital status - should only be D, M, N (unknown), S, W. Replace N and "" with Unknown and map Mrs to M for females.

data <- data %>%
  mutate(MARITAL_STATUS = as.character(MARITAL_STATUS)) %>%
  mutate(MARITAL_STATUS = ifelse(
    MARITAL_STATUS %in% c("", "N") & TITLE == "MRS" & GENDER == "F",
    "M", MARITAL_STATUS)) %>%
  mutate(MARITAL_STATUS = ifelse(MARITAL_STATUS %in% c("", "N"), "Unknown", MARITAL_STATUS)) %>%
  mutate(MARITAL_STATUS = factor(MARITAL_STATUS, levels = c("M","S","D","W","Unknown")))
    
table(data$MARITAL_STATUS)
unique(data$MARITAL_STATUS)



# Insured marital status - should only be D, M, N, S, W
table(data$INSURED_MARITAL_STATUS)




# Payer_dateofbirth - only ages between 18 and 100,



# Gender - typos like "MF"


# ps_lapse1 - should be 0–100%, all positive. Replace - values and outliers with mode
hist(data$ps_lapse1, breaks = 50, main = "Lapsed Distribution", xlab = "Freq")
summary(data$ps_lapse1)



# nbank - must be positive, 0-5 is realistic
hist(data$nbank, breaks = 50, main = "No. Bank Accounts Distribution", xlab = "Freq")
table(data$nloans)




# nloans - must be positive, 0-10 is realistic
hist(data$nloans, breaks = 50, main = "No. Loans Distribution", xlab = "Freq")
table(data$nloans)




# ------ Chi-square test for significance -------



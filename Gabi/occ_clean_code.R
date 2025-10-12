# ----- Occupation description clean code -----
occ_code_clean <- read_excel() # ADD processed_data_with_sector code from data from Richard

# Add sector to df
df$OCCUPATION_clean <- occ_code_clean$sector

clean_sector_mapping <- function(df) {
  # Create mapping for cleaner occupation names
  occupation_mapping <- c(
    "[Technical / Engineering" = "Technical/Engineering",
    "Clerical" = "Clerical",
    "Security / Policing" = "Security/Policing", 
    "Craftsman" = "Craftsman",
    "Armed Forces" = "Armed Forces",
    "Healthcare / Social Services" = "Healthcare/Social Services",
    "Commercial and Industrial and Linesman" = "Commercial/Industrial",
    "Retail / Sales" = "Retail/Sales",
    "Animal Health" = "Animal Health",
    "Information Technology" = "Information Technology",
    "Transportation / Emergency Services" = "Transportation/Emergency",
    "Postal" = "Postal",
    "Animal Husbandry" = "Animal Husbandry",
    "Medical" = "Medical",
    "Food and Beverage" = "Food/Beverage",
    "Technical / Engineering" = "Technical/Engineering",
    "Sales" = "Sales",
    "Education" = "Education",
    "[engine water supply, horticulturist, plant, water supply" = "Utilities/Horticulture",
    "Planning" = "Planning"
  )
  
  # Apply mapping, keep original if not found
  df$SECTOR <- ifelse(
    df$sector %in% names(occupation_mapping),
    toupper(occupation_mapping[df$sector]),
    toupper(df$sector)
  )
  
  # Drop original sector column
  df$sector <- NULL
  
  return(df)
}

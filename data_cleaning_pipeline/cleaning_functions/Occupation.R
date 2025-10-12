handle_sector_missing <- function(df) {
  # Map empty strings to NA
  df$SECTOR[df$SECTOR == ""] <- NA
  
  # Convert to uppercase
  df$SECTOR <- toupper(df$SECTOR)
  
  # Drop rows with NA in SECTOR
  df <- df[!is.na(df$SECTOR), ]
  
  return(df)
}

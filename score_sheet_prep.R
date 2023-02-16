# score sheets to r compatible

# function to convert days and times to understandable output and filter only injected flies

excel_prep <- function(x){
  library(readxl)
  library(plyr)
  library(tidyverse)
  library(openxlsx)
  
  excel <- x %>% 
    filter(!is.na(Time_of_injection) & !Time_of_injection == "Time_of_injection") %>% 
    mutate(Day_of_injection = as.Date(as.numeric(Day_of_injection),origin = "1899-12-30")) %>% 
    mutate(Time_of_injection = format(as.POSIXct(as.numeric(Time_of_injection) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M")) %>% 
    mutate(Day_of_death = as.Date(as.numeric(Day_of_death),origin = "1899-12-30")) %>% 
    mutate(Time_of_death = format(as.POSIXct(as.numeric(Time_of_death) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))
  return(excel)
}

# Reads all sheets from files and extracts them all, then reformats them and combines into one dataframe

read_excel_allsheets <- function(filename) {
  
  
  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[!sheets %in% c("metadata")]
  x <- lapply(sheets, function(X) read.xlsx(filename, sheet = X, fillMergedCells = TRUE))
  x <- lapply(x, excel_prep)
  names(x) <- sheets
  x <- ldply(x, data.frame)
  x
}




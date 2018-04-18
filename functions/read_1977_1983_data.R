## read 1980 crime data
## thesis
##
## andrew wolff
## 6 april 2018

read_1977_1983_data <- function(series_number, YEAR){
  crime_data_frame <- 
    read.table(paste0("data/crime_data/1977-1983/ICPSR_08703/DS00", series_number, "/08703-00", series_number, "-Data.txt"),
               sep = "|", colClasses = "character") %>%
    mutate(year = as.numeric(YEAR),
           state = as.numeric(substr(V1, 12, 13)),
           county1 = as.numeric(substr(V1, 14, 16)),
           murder = as.numeric(substr(V1, 38, 41)),
           rape = as.numeric(substr(V1, 42, 46)),
           robbery = as.numeric(substr(V1, 47, 52)),
           assault = as.numeric(substr(V1, 53, 57)),
           burglary = as.numeric(substr(V1, 58, 63)),
           all = as.numeric(substr(V1, 31, 37))) %>%
    select(-V1)
  return(crime_data_frame)
}

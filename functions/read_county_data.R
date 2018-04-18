## read county data
## thesis
## 
## andrew wolff
## 6 april 2018

read_county_data <- function(filename, YEAR, crime_list){
  crime_totals <- 
    read.table(filename, sep = "|", colClasses = "character") %>% 
    mutate(year = YEAR,
           state = substr(V1, crime_list[[1]][1], crime_list[[1]][2]),
           county1 = substr(V1, crime_list[[3]][1], crime_list[[3]][2]),
           murder = substr(V1, crime_list[[4]][1], crime_list[[4]][2]),
           rape = substr(V1, crime_list[[5]][1], crime_list[[5]][2]),
           robbery = substr(V1, crime_list[[6]][1], crime_list[[6]][2]),
           assault = substr(V1, crime_list[[7]][1], crime_list[[7]][2]),
           burglary = substr(V1, crime_list[[8]][1], crime_list[[8]][2]),
           all = substr(V1, crime_list[[9]][1], crime_list[[9]][2]),
           murder = as.numeric(murder),
           rape = as.numeric(rape),
           robbery = as.numeric(robbery),
           assault = as.numeric(assault),
           burglary = as.numeric(burglary),
           all = as.numeric(all)) %>% 
    select(-V1)
}

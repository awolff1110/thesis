## code census
## thesis
##
## andrew wolff
## 6 april 2018

code_census <- function(census_year){
  coded_census <- 
    census_year %>% 
    mutate(house_weight = ifelse(PERNUM == 1, HHWT, 0),
           median_weight = ifelse(PERWT >= 1, 1, 0),
           black = ifelse(RACE == 2, 1, 0),
           poverty = ifelse(POVERTY <= 1 & PERNUM == 1, 1, 0),
           immigrant = ifelse(BPL > 120, 1, 0),
           young_male = ifelse(AGE >= 15 & AGE <= 25, 1, 0),
           unstable_family = ifelse(RELATE == 1 & MARST != 1 & SEX == 2, 1, 0),
           manufacture = ifelse(IND1990 %in% c(100:392), 1, 0),
           unemployed = ifelse(EMPSTAT == 2, 1, 0),
           low_skill_service = ifelse(IND1990 %in% low_skill_sector, 1, 0))
  return(coded_census)
}
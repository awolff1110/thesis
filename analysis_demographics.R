## demographic data for analysis
## thesis
##
## andrew wolff
## 5 april 2018


# initial settings --------------------------------------------------------

rm(list = ls())
setwd("~/Documents/thesis")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(zoo))
options(stringsAsFactors = FALSE)
source("functions/unemployment.R")

puma_county <- read.csv("data/puma_county_xwalk.csv", skip = 1) %>% 
  rename(county_weight = puma2k.to.county.alloc.factor) %>% 
  select(puma2k, county, county_weight) %>% 
  mutate(county = as.character(county),
         county = ifelse(nchar(county) == 4, paste0("0", county), county),
         FIPS_ST = as.numeric(substr(county, 1, 2)),
         county = as.numeric(substr(county, 3, 5)))

# load and wrangle acs data -----------------------------------------------

low_skill_sector <- c(10:32, 60, 400:432, 761:792, 580:691)
manufacturing_jobs <- c(100:392)
  
demographic_data <- read_csv("data/usa_00074.csv")

demographics <- 
  demographic_data %>% 
  left_join(puma_county,
            by = c("STATEFIP" = "FIPS_ST", "PUMA" = "puma2k")) %>% 
  filter(!is.na(county_weight)) %>% 
  mutate(person_weight = PERWT * county_weight,
         house_weight = ifelse(PERNUM == 1, HHWT * county_weight, 0),
         income_weight = ifelse(HHINCOME < 9999999 & person_weight >= 1, 1, 0),
         immigrant = ifelse(BPL > 120, 1, 0),
         young = ifelse(AGE >= 15 & AGE <= 25, 1, 0),
         manufacturing = ifelse(IND1990 %in% manufacturing_jobs, 1, 0),
         low_skill_sector = ifelse(IND1990 %in% low_skill_sector, 1, 0),
         black = ifelse(RACBLK == 2, 1, 0),
         poor = ifelse(POVERTY <= 1 & PERNUM == 1, 1, 0),
         unstable_family = ifelse(RELATE == 1 & MARST != 1 & SEX == 2, 1, 0),
         income = HHINCOME)

county_demographics <- 
  demographics %>% 
  group_by(county, STATEFIP, YEAR) %>% 
  summarize(population = sum(person_weight),
            households = sum(house_weight),
            foreign_born = sum(immigrant * person_weight) / population * 100,
            young_population = sum(young * person_weight) / population * 100,
            manufacturing = sum(manufacturing * person_weight) / population * 100,
            low_skill_sector = sum(low_skill_sector * person_weight) / population * 100,
            median_income = median(rep(income, times = income_weight)),
            poverty = sum(poor * house_weight) / households * 100,
            pct_black = sum(black * person_weight) / population * 100,
            family_instability = sum(unstable_family * house_weight) / households * 100) %>% 
  ungroup() %>% 
  mutate(ln_foreign_born = log(foreign_born + 1),
         ln_median_income = log(median_income + 1),
         deprivation = poverty + pct_black + family_instability)

county_demogr_employ <- 
  data.frame(
    left_join(unemployment_fips,
              county_demographics,
              by = c("county", "STATEFIP", "YEAR")) %>% 
      arrange(county, STATEFIP, YEAR) %>% 
      na.approx() 
  ) %>% 
  filter(!is.na(population) & population > 0) %>% 
  mutate(ln_deprivation = log(deprivation + 1))

write.csv(county_demogr_employ, "county_demographics.csv", row.names = FALSE)  





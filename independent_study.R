## updated independent study analysis
## thesis
##
## andrew wolff
## 31 march 2018

# initial settings --------------------------------------------------------

# install.packages("tidyverse", "readxl", "DataCombine", "maps")
rm(list = ls())
setwd("~/Documents/thesis/")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(DataCombine))
suppressPackageStartupMessages(library(maps))
options(stringsAsFactors = FALSE)
source("functions/unemployment.R")


# geographic crosswalks ---------------------------------------------------

puma_county_xwalk <- read.csv("data/puma_county_xwalk.csv", skip = 1) %>% 
  rename(county_weight = puma2k.to.county.alloc.factor) %>% 
  select(puma2k, county, county_weight) %>% 
  mutate(county = as.character(county),
         county = ifelse(nchar(county) == 4, paste0("0", county), county),
         FIPS_ST = as.numeric(substr(county, 1, 2)),
         county = as.numeric(substr(county, 3, 5)))

cdp_county_xwalk <- read.csv("data/cdp_county_xwalk.csv", skip = 1) %>% 
  rename(county_weight = placefp.to.county.alloc.factor) %>% 
  select(Place.Name, county, county_weight, State.Postal.Code) %>% 
  mutate(county = as.character(county),
         county = ifelse(nchar(county) == 4, paste0("0", county), county),
         county = as.numeric(substr(county, 3, 5)),
         Place.Name = sub(", .*$", "", Place.Name),
         Place.Name = gsub(" city| CDP| town| township| borough| village| municipality| and ",
                           "", Place.Name),
         Place.Name = gsub(" \\(.*\\)", "", Place.Name),
         Place.Name = gsub("^urban ", "", Place.Name),
         Place.Name = gsub("\\-.*$", "", Place.Name),
         Place.Name = gsub("/.*$", "", Place.Name),
         Place.Name = tolower(Place.Name)) %>% 
  filter(Place.Name != " ") %>% 
  filter(county_weight > 0)


state_fips <- read.table("https://www2.census.gov/geo/docs/reference/state.txt",
                         sep = "|", header = TRUE) %>% 
  select(-STATENS) %>% 
  rename(STATEFIP = STATE) %>% 
  mutate(STATE_NAME = tolower(STATE_NAME))

# refugee data ------------------------------------------------------------

free_case_countries <- c("Bhutan", "Burma", "Burundi", 
                         "Dem. Rep. Congo", "Eritrea",
                         "Iraq", "Laos", "Central African Republic")

refugees <- read_xlsx("data/MX - Arrivals by Destination and Nationality.xlsx", 
                      skip = 4) %>% 
  select(-c(Textbox94, Textbox95)) %>% 
  rename(origin = nat_definition5, 
         year = region_name_4,
         state_destination = Category4, 
         country_total = textbox38,
         city_total = Cases5, 
         city_destination = Assur_DestinationCity3,
         origin_total = textbox38,
         national_total = Cases6,
         state_total = textbox42) %>% 
  select(origin, year, state_destination, city_total, city_destination) %>% 
  mutate(year = gsub("CY ", "", year),
         year = as.numeric(year),
         city_total = gsub(",", "", city_total),
         city_total = as.numeric(city_total),
         city_destination = tolower(city_destination),
         city_destination = gsub("saint|^st ", "st.", city_destination),
         city_destination = gsub(" township", "", city_destination),
         city_destination = sub(",", "", city_destination),
         city_destination = tolower(city_destination),
         city_destination = ifelse(city_destination %in% 
                                     c("brooklyn", "queens", "staten island", "bronx"), 
                                   "new york", city_destination),
         city_destination = ifelse(city_destination == "tujunga", 
                                   "los angeles", city_destination),
         state_destination = tolower(state_destination)) %>% 
  left_join(state_fips, by = c("state_destination" = "STATE_NAME")) %>% 
  rename(State.Postal.Code = STUSAB,
         Place.Name = city_destination) %>% 
  left_join(cdp_county_xwalk, by = c("State.Postal.Code", "Place.Name")) %>% 
  filter(!is.na(county)) %>% 
  filter(!(origin %in% c("Iraq", "Burma") & year %in% c(2003, 2004, 2005, 2006))) %>%
  filter(!(origin == "Laos" & !(year %in% c(2004, 2005)))) %>%
  filter(!(origin == "Burundi" & !(year %in% c(2007, 2008))))

free_case_total <- read_xlsx("data/MX - Arrivals by Destination and Nationality.xlsx", 
                             skip = 4) %>% 
  select(-c(Textbox94, Textbox95)) %>% 
  rename(origin = nat_definition5, year = region_name_4,
         state_destination = Category4, country_total = textbox38,
         city_total = Cases5, city_destination = Assur_DestinationCity3,
         origin_total = textbox38,
         national_total = Cases6,
         state_total = textbox42) %>% 
  mutate(year = gsub("CY ", "", year),
         year = as.numeric(year)) %>% 
  filter(year < 2015) %>% 
  filter(origin %in% free_case_countries) %>% 
  group_by(origin, year, origin_total) %>% 
  summarise() %>% 
  ungroup() %>% 
  group_by(origin) %>% 
  summarize(total = sum(origin_total)) %>% 
  ungroup()

write.table(free_case_total, "free_case_total.txt", row.names = FALSE)

county_refugees <- refugees %>% 
  filter(origin %in% free_case_countries) %>% 
  group_by(year, STATEFIP, county) %>% 
  summarize(county_total = sum(city_total * county_weight)) %>% 
  ungroup()

no_iraq <- refugees %>% 
  filter(origin %in% free_case_countries) %>% 
  filter(origin != "Iraq") %>% 
  group_by(year, STATEFIP, county) %>% 
  summarize(county_total = sum(city_total * county_weight)) %>% 
  ungroup()

refugees_five_year <- refugees %>% 
  filter(origin %in% free_case_countries) %>% 
  filter(!(origin == "Bhutan" & year >= 2014) &
           !(origin == "Burma" & year >= 2011) &
           !(origin %in% c("Burundi", "Iraq") & year >= 2012) &
           !(origin %in% c("Dem. Rep. Congo", "Eritrea") & year >= 2008) &
           !(origin == "Laos" & year >= 2009)) %>% 
  group_by(year, STATEFIP, county) %>% 
  summarize(county_total = sum(city_total * county_weight)) %>% 
  ungroup()

# homicides data ----------------------------------------------------------

read_crime_data <- function(year, icpsr){
  read.table(paste0("data/ASR_County/ICPSR_", icpsr, "/DS0004/", icpsr, "-0004-Data.tsv"), 
             sep = "\t", header = TRUE) %>% mutate(YEAR = as.numeric(year)) %>% 
    rename(AGASLT = AGASSLT) 
}

read_old_crime_data <- function(year, icpsr){
  read_xlsx(paste0("data/ASR_County/ICPSR_", icpsr, "/DS0004/county_crime_", year, ".xlsx"),
            col_names = c("STUDYNO", "EDITION", "PART", "IDNO", "FIPS_ST",
                          "FIPS_CTY", "CPOPARST", "CPOPCRIM", "AG_ARRST",
                          "AG_OFF", "COVIND", "INDEX", "MODINDX", "MURDER",
                          "RAPE", "ROBBERY", "AGASLT", "BURGLRY", "LARCENY",
                          "MVTHEFT", "ARSON")) %>% mutate(YEAR = as.numeric(year)) %>% 
    mutate(VIOL = MURDER + RAPE + ROBBERY + AGASLT,
           PROPERTY = BURGLRY + LARCENY + MVTHEFT) %>% 
    select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)
}

county_crime_2014 <- read_crime_data("2014", "36399") %>% 
  select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)

county_crime_2013 <- read_crime_data("2013", "36117") %>% 
  select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)

county_crime_2012 <- read_crime_data("2012", "35019") %>% 
  rename(VIOL = INDEX, PROPERTY = MODINDX) %>% 
  select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)

county_crime_2011 <- read_crime_data("2011", "34582") %>% 
  select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)

county_crime_2010 <- read_crime_data("2010", "33523") %>% 
  select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)

county_crime_2009 <- read_crime_data("2009", "30763") %>% 
  select(FIPS_ST, FIPS_CTY, VIOL, PROPERTY, MURDER:YEAR, COVIND)

county_crime_2008 <- read_old_crime_data("2008", "27644")
county_crime_2007 <- read_old_crime_data("2007", "25114")
county_crime_2006 <- read_old_crime_data("2006", "23780")
county_crime_2005 <- read_old_crime_data("2005", "04717")
county_crime_2004 <- read_old_crime_data("2004", "04466")
county_crime_2003 <- read_old_crime_data("2003", "04360")

county_crime <- 
  rbind(county_crime_2003, county_crime_2004, county_crime_2005, county_crime_2006,
        county_crime_2007, county_crime_2008, county_crime_2009, county_crime_2010,
        county_crime_2011, county_crime_2012, county_crime_2013, 
        county_crime_2014) %>% 
  filter(COVIND >= 90)

rm(county_crime_2003, county_crime_2004, county_crime_2005, county_crime_2006,
   county_crime_2007, county_crime_2008, county_crime_2009, county_crime_2010,
   county_crime_2011, county_crime_2012, county_crime_2013, county_crime_2014)

# drug arrests ------------------------------------------------------------

read_arrest_data <- function(year, icpsr){
  read.table(paste0("data/ASR_County/ICPSR_", icpsr, "/DS0001/", icpsr, "-0001-Data.tsv"),
             sep = "\t", header = TRUE) %>% 
    mutate(YEAR = as.numeric(year)) %>% 
    select(FIPS_ST, FIPS_CTY, DRUGTOT, YEAR, COVIND)
}

substring_arrests <- function(year, icpsr){
  read.table(paste0("data/ASR_County/ICPSR_", icpsr, "/DS0001/", icpsr, "-0001-Data.txt"),
             colClasses = "character") %>% 
    mutate(FIPS_ST = as.numeric(substr(V1, 11, 12)), 
           FIPS_CTY = as.numeric(substr(V1, 13, 15)),
           DRUGTOT = as.numeric(substr(V1, 133, 137)),
           YEAR = as.numeric(year),
           COVIND = as.numeric(substr(V1, 28, 35))) %>% 
    select(-V1)
}

arrests_2014 <- read_arrest_data("2014", "36399")
arrests_2013 <- read_arrest_data("2013", "36117")
arrests_2012 <- read_arrest_data("2012", "35019")
arrests_2011 <- read_arrest_data("2011", "34582")
arrests_2010 <- read_arrest_data("2010", "33523")
arrests_2009 <- read_arrest_data("2009", "30763")
arrests_2008 <- substring_arrests("2008", "27644")
arrests_2007 <- substring_arrests("2007", "25114")
arrests_2006 <- substring_arrests("2006", "23780")
arrests_2005 <- substring_arrests("2005", "04717")
arrests_2004 <- substring_arrests("2004", "04466")
arrests_2003 <- substring_arrests("2003", "04360")

county_drug_arrests <-
  rbind(arrests_2003, arrests_2004, arrests_2005, arrests_2006, arrests_2007,
        arrests_2008, arrests_2009, arrests_2010, arrests_2011, arrests_2012,
        arrests_2013, arrests_2014) %>% filter(COVIND >= 90) %>% select(-COVIND)

rm(arrests_2003, arrests_2004, arrests_2005, arrests_2006, arrests_2007,
   arrests_2008, arrests_2009, arrests_2010, arrests_2011, arrests_2012, 
   arrests_2013, arrests_2014)

# census data -------------------------------------------------------------

census_2000 <- read_csv("data/Demographic/usa_00068.csv")

county_population <- 
  census_2000 %>% 
  left_join(puma_county_xwalk, 
            by = c("STATEFIP" = "FIPS_ST", "PUMA" = "puma2k")) %>% 
  filter(!is.na(county_weight)) %>% 
  mutate(PERWT = PERWT * county_weight) %>% 
  group_by(county, STATEFIP) %>% 
  summarize(population = sum(PERWT)) %>% 
  ungroup()

# combine 2000 data -------------------------------------------------------

county_combined <-
  left_join(county_population, county_crime, 
            by = c("STATEFIP" = "FIPS_ST", 
                   "county" = "FIPS_CTY")) %>% 
  left_join(county_drug_arrests, 
            by = c("STATEFIP" = "FIPS_ST", 
                   "county" = "FIPS_CTY", "YEAR")) %>% 
  left_join(county_refugees, 
            by = c("STATEFIP", "county", 
                   "YEAR" = "year")) %>% 
  filter(!is.na(MURDER)) %>% 
  mutate(county_total = ifelse(is.na(county_total), 
                               0, county_total),
         murder_rate = MURDER / population * 1e5,
         ln_murder_rate = log(murder_rate + 1),
         drug_arrest_rate = DRUGTOT / population * 1e5,
         ln_drug_arrest_rate = log(drug_arrest_rate + 1),
         violent_crime_rate = VIOL / population * 1e5,
         ln_violent_crime_rate = log(violent_crime_rate + 1),
         property_crime_rate = PROPERTY / population * 1e5,
         ln_property_crime_rate = log(property_crime_rate + 1),
         FIPS = STATEFIP * 1000 + county) %>% 
  filter(YEAR < 2015) %>% 
  distinct(YEAR, FIPS, .keep_all = TRUE) %>% 
  ungroup() %>% 
  filter(!is.na(DRUGTOT) & !is.na(MURDER)) %>% 
  slide(Var = "county_total", TimeVar = "YEAR", GroupVar = "FIPS",
        NewVar = "county_total_lag") %>% 
  left_join(unemployment_fips, 
            by = c("YEAR" = "Year", 
                   "STATEFIP", 
                   "county" = "COUNTYFIP")) %>% 
  mutate(YEAR = as.factor(YEAR),
         FIPS = as.factor(FIPS),
         ln_county_total_lag = log(county_total_lag + 1)) %>% 
  filter(!is.na(avg_rate))

write.csv(county_combined, 
          "data/census_data_set.csv", 
          na = ".", row.names = FALSE)

# combine acs data --------------------------------------------------------

county_demographics <- 
  read.csv("county_demographics.csv") %>% 
  select(-X)

acs_county_combined <- 
  left_join(county_crime,
            county_drug_arrests,
            by = c("FIPS_ST", "FIPS_CTY", "YEAR")) %>% 
  left_join(county_refugees,
            by = c("FIPS_ST" = "STATEFIP", 
                   "FIPS_CTY" = "county", 
                   "YEAR" = "year")) %>% 
  left_join(county_demographics,
            by = c("FIPS_ST" = "STATEFIP", 
                   "FIPS_CTY" = "county", 
                   "YEAR")) %>% 
  filter(!is.na(avg_rate) & !is.na(DRUGTOT)) %>% 
  mutate(county_total = ifelse(is.na(county_total), 0, county_total),
         ln_county_total = log(county_total + 1),
         murder_rate = MURDER / population * 1e5,
         ln_murder_rate = log(murder_rate + 1),
         drug_arrest_rate = DRUGTOT / population * 1e5,
         ln_drug_arrest_rate = log(drug_arrest_rate + 1),
         violent_crime_rate = VIOL / population * 1e5,
         ln_violent_crime_rate = log(violent_crime_rate + 1),
         property_crime_rate = PROPERTY / population * 1e5,
         ln_property_crime_rate = log(property_crime_rate + 1),
         FIPS = FIPS_ST * 1000 + FIPS_CTY) %>% 
  filter(YEAR <= 2014) %>% 
  slide(Var = "county_total", 
        TimeVar = "YEAR", 
        GroupVar = "FIPS",
        NewVar = "county_total_lag") %>% 
  slide(Var = "county_total", 
        TimeVar = "YEAR", 
        GroupVar = "FIPS",
        NewVar = "county_total_lag3",
        slideBy = -3) %>% 
  mutate(ln_county_total_lag = log(county_total_lag + 1),
         ln_county_total_lag3 = log(county_total_lag3 + 1))

write.csv(acs_county_combined, "data/acs_data_set.csv", na = ".", row.names = FALSE)


# no iraq acs -------------------------------------------------------------

no_iraq_acs <- 
  left_join(county_crime,
            county_drug_arrests,
            by = c("FIPS_ST", "FIPS_CTY", "YEAR")) %>% 
  left_join(no_iraq,
            by = c("FIPS_ST" = "STATEFIP", 
                   "FIPS_CTY" = "county", 
                   "YEAR" = "year")) %>% 
  left_join(county_demographics,
            by = c("FIPS_ST" = "STATEFIP", 
                   "FIPS_CTY" = "county", "YEAR")) %>% 
  filter(!is.na(avg_rate) & !is.na(DRUGTOT)) %>% 
  mutate(county_total = ifelse(is.na(county_total), 0, county_total),
         ln_county_total = log(county_total + 1),
         murder_rate = MURDER / population * 1e5,
         ln_murder_rate = log(murder_rate + 1),
         drug_arrest_rate = DRUGTOT / population * 1e5,
         ln_drug_arrest_rate = log(drug_arrest_rate + 1),
         violent_crime_rate = VIOL / population * 1e5,
         ln_violent_crime_rate = log(violent_crime_rate + 1),
         property_crime_rate = PROPERTY / population * 1e5,
         ln_property_crime_rate = log(property_crime_rate + 1),
         FIPS = FIPS_ST * 1000 + FIPS_CTY) %>% 
  filter(YEAR <= 2014) %>% 
  slide(Var = "county_total", 
        TimeVar = "YEAR", 
        GroupVar = "FIPS",
        NewVar = "county_total_lag") %>% 
  slide(Var = "county_total", 
        TimeVar = "YEAR", 
        GroupVar = "FIPS",
        NewVar = "county_total_lag3",
        slideBy = -3) %>% 
  mutate(ln_county_total_lag = log(county_total_lag + 1),
         ln_county_total_lag3 = log(county_total_lag3 + 1))

write.csv(no_iraq_acs, "data/no_iraq_acs.csv", na = ".", row.names = FALSE)


# five year acs -----------------------------------------------------------

five_year_acs <- 
  left_join(county_crime,
            county_drug_arrests,
            by = c("FIPS_ST", "FIPS_CTY", "YEAR")) %>% 
  left_join(refugees_five_year,
            by = c("FIPS_ST" = "STATEFIP", 
                   "FIPS_CTY" = "county", 
                   "YEAR" = "year")) %>% 
  left_join(county_demographics,
            by = c("FIPS_ST" = "STATEFIP", 
                   "FIPS_CTY" = "county", "YEAR")) %>% 
  filter(!is.na(avg_rate) & !is.na(DRUGTOT)) %>% 
  mutate(county_total = ifelse(is.na(county_total), 0, county_total),
         ln_county_total = log(county_total + 1),
         murder_rate = MURDER / population * 1e5,
         ln_murder_rate = log(murder_rate + 1),
         drug_arrest_rate = DRUGTOT / population * 1e5,
         ln_drug_arrest_rate = log(drug_arrest_rate + 1),
         violent_crime_rate = VIOL / population * 1e5,
         ln_violent_crime_rate = log(violent_crime_rate + 1),
         property_crime_rate = PROPERTY / population * 1e5,
         ln_property_crime_rate = log(property_crime_rate + 1),
         FIPS = FIPS_ST * 1000 + FIPS_CTY) %>% 
  filter(YEAR <= 2014) %>% 
  slide(Var = "county_total", 
        TimeVar = "YEAR", 
        GroupVar = "FIPS",
        NewVar = "county_total_lag") %>% 
  slide(Var = "county_total", 
        TimeVar = "YEAR", 
        GroupVar = "FIPS",
        NewVar = "county_total_lag3",
        slideBy = -3) %>% 
  mutate(ln_county_total_lag = log(county_total_lag + 1),
         ln_county_total_lag3 = log(county_total_lag3 + 1))

write.csv(five_year_acs, "data/five_year_acs.csv", na = ".", row.names = FALSE)


# graphs for writeup ------------------------------------------------------

# refugee ceiling, actual admissions, free-case refugees
# migration policy institute data

refugee_admits <- read_xlsx("data/MPI-Data-Hub_Refugee-Admissions_2017a.xlsx",
                            skip = 7) %>% 
  rename(Ceiling = `Annual Ceiling`,
         Admits = `Number of Admitted Refugees`) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(!is.na(Year)) %>% 
  filter(Year >= 2003 & Year <= 2014)

# WRAPS admissions data
refugee_admissions <- read_xlsx(
  "data/MX - Arrivals by Destination and Nationality.xlsx",
  skip = 4
) %>% 
  rename(origin = nat_definition5,
         year = region_name_4,
         origin_total = textbox38) %>% 
  mutate(year = sub("CY ", "", year),
         year = as.numeric(year)) %>% 
  filter(origin %in% free_case_countries) %>% 
  group_by(origin, year, origin_total) %>% 
  summarize() %>% 
  ungroup() %>% 
  filter(year <= 2014) %>% 
  group_by(year) %>% 
  summarize(free_cases = sum(origin_total)) %>% 
  ungroup()

refugee_graph_data <- left_join(refugee_admits, 
                                refugee_admissions, 
                                by = c("Year" = "year")) %>% 
  gather(type, total, Ceiling:free_cases) %>% 
  mutate(type = factor(type, levels = c("Ceiling", "Admits", "free_cases"), 
                       labels = c("Ceiling", "Admitted", "Estimated Free Cases")))

# create visualization

ggplot(refugee_graph_data, aes(x = Year, y = total)) +
  geom_line(aes(color = type)) +
  geom_point(aes(color = type)) +
  scale_x_continuous(breaks = c(2003:2014)) +
  scale_y_continuous(labels = c("0", "20,000", "40,000", 
                                "60,000", "80,000")) +
  theme_bw() +
  labs(x = "Year",
       y = "Refugee Admissions",
       color = "") +
  theme(legend.position = "bottom")
ggsave("figures/refugee_admissions.png", width = 8, height = 4)


# murder graphic ----------------------------------------------------------

county_murder_refugees <- 
  left_join(
    county_refugees,
    county_crime,
    by = c("year" = "YEAR", 
           "STATEFIP" = "FIPS_ST", 
           "county" = "FIPS_CTY")
  ) %>% 
  filter(year <= 2014 & !is.na(MURDER)) %>% 
  select(year, STATEFIP, county, MURDER) %>% 
  left_join(county_population, by = c("county", "STATEFIP")) %>% 
  mutate(murder_per_100k = MURDER / population * 1e5) %>% 
  group_by(year) %>% 
  summarize(refugee_county_murder_rate = 
              weighted.mean(murder_per_100k, population)) %>% 
  ungroup()

county_refugee_settled <- county_refugees %>% 
  filter(county_total >= 1)

county_murder_no_refugees <- 
  anti_join(
    county_crime,
    county_refugee_settled,
    by = c("YEAR" = "year", 
           "FIPS_ST" = "STATEFIP", 
           "FIPS_CTY" = "county")
  ) %>% 
  left_join(county_population, 
            by = c("FIPS_CTY" = "county", 
                   "FIPS_ST" = "STATEFIP")) %>% 
  mutate(murder_per_100k = MURDER / population * 1e5) %>% 
  filter(!is.na(murder_per_100k)) %>% 
  group_by(YEAR) %>% 
  summarize(no_refugee_county_murder_rate = 
              weighted.mean(murder_per_100k, population)) %>% 
  ungroup()

county_murder_rate <- 
  left_join(county_crime, 
            county_population,
            by = c("FIPS_CTY" = "county", 
                   "FIPS_ST" = "STATEFIP")) %>% 
  mutate(murder_per_100k = MURDER / population * 1e5) %>% 
  filter(!is.na(murder_per_100k)) %>% 
  group_by(YEAR) %>% 
  summarize(county_murder_rate = 
              weighted.mean(murder_per_100k, population)) %>% 
  ungroup()

murder_rates <- left_join(county_murder_refugees, 
                          county_murder_no_refugees,
                          by = c("year" = "YEAR")) %>% 
  left_join(county_murder_rate, by = c("year" = "YEAR")) %>% 
  gather(type, rate, refugee_county_murder_rate:county_murder_rate) %>% 
  mutate(type = factor(type, 
                       levels = c("county_murder_rate", 
                                  "refugee_county_murder_rate",
                                  "no_refugee_county_murder_rate"),
                       labels = c("All Counties",
                                  "Refugee Resettlement",
                                  "No Refugee Resettlement")),
         crime = "Murder")

# make graphic

ggplot(murder_rates, aes(x = year, y = rate, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2003:2014)) +
  scale_y_continuous(breaks = c(0:8), 
                     limits = c(0,8), 
                     labels = c("0.0", "1.0", "2.0", "3.0", 
                                "4.0", "5.0", "6.0", "7.0", "8.0")) +
  theme_bw() +
  labs(x = "Year",
       y = "Murder Rate (per 100,000)",
       color = "") +
  theme(legend.position = "bottom")
ggsave("figures/murder_rates.png", width = 8, height = 4)


# violent crime graphic ---------------------------------------------------

county_violent_refugees <- 
  left_join(
    county_refugees,
    county_crime,
    by = c("year" = "YEAR", 
           "STATEFIP" = "FIPS_ST", 
           "county" = "FIPS_CTY")
  ) %>% 
  filter(year <= 2014 & !is.na(VIOL)) %>% 
  select(year, STATEFIP, county, VIOL) %>% 
  left_join(county_population, 
            by = c("county", "STATEFIP")) %>% 
  mutate(violent_rate = VIOL / population * 1e5) %>% 
  group_by(year) %>% 
  summarize(refugee_violent_rate = 
              weighted.mean(violent_rate, population)) %>% 
  ungroup()

county_refugee_settled <- subset(county_refugees, county_total >= 1)

county_violent_none <- 
  anti_join(
    county_crime,
    county_refugee_settled,
    by = c("YEAR" = "year", 
           "FIPS_ST" = "STATEFIP", 
           "FIPS_CTY" = "county")
  ) %>% 
  left_join(county_population, 
            by = c("FIPS_CTY" = "county", 
                   "FIPS_ST" = "STATEFIP")) %>% 
  mutate(violent_rate = VIOL / population * 1e5) %>% 
  filter(!is.na(violent_rate)) %>% 
  group_by(YEAR) %>% 
  summarize(no_refugee_violent = 
              weighted.mean(violent_rate, population)) %>% 
  ungroup()

county_violent <- 
  county_crime %>%
  left_join(county_population, 
            by = c("FIPS_CTY" = "county", 
                   "FIPS_ST" = "STATEFIP")) %>% 
  mutate(violent_rate = VIOL / population * 1e5) %>% 
  filter(!is.na(violent_rate)) %>% 
  group_by(YEAR) %>% 
  summarize(county_violent = 
              weighted.mean(violent_rate, population)) %>% 
  ungroup()

violent_rates <- left_join(county_violent_refugees, 
                          county_violent_none,
                          by = c("year" = "YEAR")) %>% 
  left_join(county_violent, by = c("year" = "YEAR")) %>% 
  gather(type, rate, refugee_violent_rate:county_violent) %>% 
  mutate(type = factor(type, 
                       levels = c("county_violent", 
                                  "refugee_violent_rate",
                                  "no_refugee_violent"),
                       labels = c("All Counties",
                                  "Refugee Resettlement",
                                  "No Refugee Resettlement")),
         crime = "Violent")

# make graphic

ggplot(violent_rates, aes(x = year, y = rate, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2003:2014)) +
  scale_y_continuous(breaks = seq(0, 700, 100), 
                     limits = c(0,700), 
                     labels = c("0", "100", "200", "300", 
                                "400", "500", "600", "700")) +
  theme_bw() +
  labs(x = "Year",
       y = "Violent Crime Rate",
       color = "") +
  theme(legend.position = "bottom")
ggsave("figures/violent_rates.png", width = 8, height = 4)


# property crime graphic --------------------------------------------------

county_property_refugees <- 
  left_join(
    county_refugees,
    county_crime,
    by = c("year" = "YEAR",
           "STATEFIP" = "FIPS_ST", 
           "county" = "FIPS_CTY")
  ) %>% 
  filter(year <= 2014 & !is.na(PROPERTY)) %>% 
  select(year, STATEFIP, county, PROPERTY) %>% 
  left_join(county_population, by = c("county", "STATEFIP")) %>% 
  mutate(property_rate = PROPERTY / population * 1e5) %>% 
  group_by(year) %>% 
  summarize(refugee_property = weighted.mean(property_rate, population)) %>% 
  ungroup()

county_property_none <- 
  anti_join(
    county_crime,
    county_refugee_settled,
    by = c("YEAR" = "year", 
           "FIPS_ST" = "STATEFIP", 
           "FIPS_CTY" = "county")
  ) %>% 
  left_join(county_population, 
            by = c("FIPS_CTY" = "county", 
                   "FIPS_ST" = "STATEFIP")) %>% 
  mutate(property_rate = PROPERTY / population * 1e5) %>% 
  filter(!is.na(property_rate)) %>% 
  group_by(YEAR) %>% 
  summarize(no_refugee_property = 
              weighted.mean(property_rate, population)) %>% 
  ungroup()

county_property <- 
  county_crime %>%
  left_join(county_population, 
            by = c("FIPS_CTY" = "county", 
                   "FIPS_ST" = "STATEFIP")) %>% 
  mutate(property_rate = PROPERTY / population * 1e5) %>% 
  filter(!is.na(property_rate)) %>% 
  group_by(YEAR) %>% 
  summarize(county_property = 
              weighted.mean(property_rate, population)) %>% 
  ungroup()

property_rates <- left_join(county_property_refugees, 
                           county_property_none,
                           by = c("year" = "YEAR")) %>% 
  left_join(county_property, 
            by = c("year" = "YEAR")) %>% 
  gather(type, rate, refugee_property:county_property) %>% 
  mutate(type = factor(type, 
                       levels = c("county_property", 
                                  "refugee_property",
                                  "no_refugee_property"),
                       labels = c("All Counties",
                                  "Refugee Resettlement",
                                  "No Refugee Resettlement")),
         crime = "Property")

# make graphic

ggplot(property_rates, aes(x = year, y = rate, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2003:2014)) +
  scale_y_continuous(breaks = seq(0, 5000, 1000), 
                     limits = c(0,5000), 
                     labels = c("0", "1000", "2000", "3000", 
                                "4000", "5000")) +
  theme_bw() +
  labs(x = "Year",
       y = "Property Crime Rate",
       color = "") +
  theme(legend.position = "bottom")
ggsave("figures/property_rates.png", width = 8, height = 4)



# geographic distribution of refugees -------------------------------------

county_refugees_total <- county_refugees %>% 
  group_by(county, STATEFIP) %>% 
  summarize(county_total = sum(county_total)) %>% 
  ungroup()

# make map for refugee resettlement

state_fips <- read.table("https://www2.census.gov/geo/docs/reference/state.txt", 
                         sep = "|", header = TRUE) %>% 
  select(STUSAB, STATE_NAME)

county_codes <- "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"

county_fips <- read.csv(county_codes,
                        header = FALSE, 
                        col.names = c("STATE", "STATEFP", "COUNTYFP", 
                                      "COUNTYNAME", "CLASSFP")) %>% 
  mutate(COUNTYNAME = tolower(COUNTYNAME),
         COUNTYNAME = sub(" county$", "", COUNTYNAME),
         COUNTYNAME = sub("^st\\. ", "st ", COUNTYNAME),
         COUNTYNAME = sub("\\'", "", COUNTYNAME),
         COUNTYNAME = sub("^ste\\. ", "ste ", COUNTYNAME)) %>% 
  left_join(state_fips, by = c("STATE" = "STUSAB")) %>% 
  mutate(STATE_NAME = tolower(STATE_NAME))

county_map <- map_data("county") %>% 
  mutate(subregion = sub("^de ", "de", subregion),
         subregion = ifelse(region == "district of columbia", 
                            "district of columbia", subregion),
         subregion = ifelse(subregion == "norfolk" & region == "virginia", 
                            "norfolk city", subregion),
         subregion = ifelse(subregion == "jefferson" & region == "louisiana", 
                            "jefferson parish", subregion),
         subregion = ifelse(subregion == "desoto" & region == "louisiana", 
                            "de soto parish", subregion),
         subregion = ifelse(subregion == "winn" & region == "louisiana", 
                            "winn parish", subregion),
         subregion = ifelse(subregion == "west feliciana" & region == "louisiana", 
                            "west feliciana parish", subregion),
         subregion = ifelse(subregion == "suffolk" & region == "virginia", 
                            "suffolk city", subregion),
         subregion = ifelse(subregion == "virginia beach" & region == "virginia", 
                            "virginia beach city", subregion),
         subregion = ifelse(subregion == "newport news" & region == "virginia", 
                            "newport news city", subregion),
         subregion = sub("yellowstone national", "yellowstone", subregion)) %>% 
  left_join(county_fips, 
            by = c("region" = "STATE_NAME", 
                   "subregion" = "COUNTYNAME")) %>% 
  select(-CLASSFP)


unmatched_counties <- county_map %>% 
  filter(is.na(STATE)) %>% 
  rename(STATE_NAME = region,
         COUNTYNAME = subregion)

county_match <- fastLink(
  dfA = unmatched_counties, dfB = county_fips,
  varnames = c("STATE_NAME", "COUNTYNAME"),
  stringdist.match = c("STATE_NAME", "COUNTYNAME"),
  partial.match = c("COUNTYNAME")
)

for(index in county_match$matches$inds.a) {
  unmatched_counties$STATE[index] <- county_fips$STATE[index]
  unmatched_counties$STATEFP[index] <- county_fips$STATEFP[index]
  unmatched_counties$COUNTYFP[index] <- county_fips$COUNTYFP[index]
}

county_map_matched <- left_join(county_map, 
                        unmatched_counties, 
                        by = c("long", "lat", "group", "order", 
                               "region" = "STATE_NAME", 
                               "subregion" = "COUNTYNAME")) %>% 
  rename(STATE = STATE.x,
         STATEFP = STATEFP.x,
         COUNTYFP = COUNTYFP.x) %>% 
  mutate(STATE = ifelse(is.na(STATE) & !is.na(STATE.y), 
                        STATE.y, STATE),
         STATEFP = ifelse(is.na(STATEFP) & !is.na(STATEFP.y), 
                          STATEFP.y, STATEFP),
         COUNTYFP = ifelse(is.na(COUNTYFP) & !is.na(COUNTYFP.y), 
                           COUNTYFP.y, COUNTYFP)) %>% 
  select(-STATE.y, -STATEFP.y, -COUNTYFP.y)

county_map_refugees <- left_join(county_map_matched, 
                                 county_refugees_total,
                                 by = c("STATEFP" = "STATEFIP",
                                        "COUNTYFP" = "county")) %>% 
  filter(!is.na(county_total)) %>% 
  mutate(county_total_bins = factor(ifelse(county_total < 50, 1,
                                           ifelse(county_total < 100, 2, 
                                                  ifelse(county_total < 200, 3, 4))),
                                    labels = c("0 to 49", "50 to 99", "100 to 199", "200+")))

state_map = map_data("state")

# make map graphic

ggplot() +
  geom_polygon(data = county_map_refugees,
               aes(x = long, y = lat, group = group, fill = county_total_bins)) +
  geom_polygon(data = state_map,
               aes(x = long, y = lat, group = group),
               color = "black",
               fill = NA) +
  coord_map(projection = "polyconic") +
  theme_bw() +
  scale_fill_brewer() +
  labs(x = "",
       y = "",
       fill = "Refugees resettled,\n2003 to 2014")
ggsave("figures/refugees_resettled_map.png", width = 8, height = 5)

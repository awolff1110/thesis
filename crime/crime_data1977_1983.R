## 1980 crime data
## thesis
## 
## andrew wolff
## 6 april 2018

# initial settings --------------------------------------------------------

source("functions/read_1977_1983_data.R")
source("functions/ucr_county.R")
source("functions/county_populations.R")


# crime data --------------------------------------------------------------

crime1977_totals <- read_1977_1983_data("02", 1977)
crime1978_totals <- read_1977_1983_data("04", 1978)
crime1979_totals <- read_1977_1983_data("06", 1979)
crime1980_totals <- read_1977_1983_data("08", 1980)
crime1981_totals <- read_1977_1983_data("10", 1981)
crime1982_totals <- read_1977_1983_data("12", 1982)
crime1983_totals <- read_1977_1983_data("14", 1983)

crime1977_1983_totals <- rbind(
  crime1977_totals, crime1978_totals, crime1979_totals,
  crime1980_totals, crime1981_totals, crime1982_totals,
  crime1983_totals
)

# convert counties --------------------------------------------------------


crime1977_1983_county <-
  crime1977_1983_totals %>%
  filter(murder < 9999) %>%
  mutate(state = as.character(state),
         county1 = as.character(county1),
         state = ifelse(nchar(state) == 1, paste0("0", state), state),
         county1 = ifelse(nchar(county1) == 1, paste0("00", county1),
                          ifelse(nchar(county1) == 2, paste0("0", county1), county1)),
         FIPS = paste0(state, county1),
         FIPS = as.numeric(FIPS)) %>%
  select(FIPS, year, murder:all)


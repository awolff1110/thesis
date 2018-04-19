## 1990 crime data
## thesis
## 
## andrew wolff
## 6 april 2018


# initial settings --------------------------------------------------------

source("functions/read_county_data.R")
source("functions/ucr_county.R")
source("functions/county_populations.R")


# crime data --------------------------------------------------------------

crime1984_filename <- 
  "data/crime_data/1984/ICPSR_08714/DS0002/08714-0002-Data.txt"
crime1984_crime_list <- 
  list(
    c(12, 13),
    c(17, 23),
    c(14, 16),
    c(38, 41),
    c(42, 46),
    c(47, 52),
    c(53, 57),
    c(58, 63),
    c(24, 30)
  )

crime1984_totals <- 
  read_county_data(crime1984_filename, 1984, crime1984_crime_list)

crime1986_filename <- 
  "data/crime_data/1986/ICPSR_09119/DS0004/09119-0004-Data.txt"
crime1986_crime_list <- 
  list(
    c(11, 12),
    c(16, 22),
    c(13, 15),
    c(42, 45),
    c(46, 49),
    c(50, 54),
    c(55, 59),
    c(60, 65),
    c(30, 35)
  )

crime1986_totals <- 
  read_county_data(crime1986_filename, 1986, crime1986_crime_list)

crime1989_filename <- 
  "data/crime_data/1989/ICPSR_09573/DS0004/09573-0004-Data.txt"
crime1989_crime_list <- 
  list(
    c(11, 12),
    c(16, 22),
    c(13, 15),
    c(35, 38),
    c(39, 42),
    c(43, 47),
    c(48, 52),
    c(53, 58),
    c(23, 28)
  )

crime1989_totals <- 
  read_county_data(crime1989_filename, 1989, crime1989_crime_list)

crime1990_filename <- 
  "data/crime_data/1990/ICPSR_09785/DS0004/09785-0004-Data.txt"
crime1990_crime_list <-
  list(
    c(11, 12),
    c(16, 22),
    c(13, 15),
    c(43, 46),
    c(47, 50),
    c(51, 55),
    c(56, 60),
    c(61, 66),
    c(31, 36)
  )
crime1990_totals <- 
  read_county_data(crime1990_filename, 1990, crime1990_crime_list)

crime84_86_89_90_totals <- rbind(
  crime1984_totals, crime1986_totals, crime1989_totals, crime1990_totals
)


# county crime ------------------------------------------------------------

crime84_86_89_90_county <- 
  crime84_86_89_90_totals %>% 
  filter(murder < 9999) %>% 
  mutate(state = as.numeric(state),
         county1 = as.numeric(county1),
         FIPS = state * 1000 + county1) %>% 
  select(FIPS, year, murder:all) %>% 
  filter(all != 999999)
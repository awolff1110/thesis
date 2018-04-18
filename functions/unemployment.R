## unemployment statistics 2003 to 2014
## thesis
##
## andrew wolff
## 17 april 2018

# unemployment ------------------------------------------------------------

state_fips <- read.table("https://www2.census.gov/geo/docs/reference/state.txt",
                         sep = "|", header = TRUE) %>% 
  select(-STATENS) %>% 
  rename(STATEFIP = STATE) %>% 
  mutate(STATE_NAME = tolower(STATE_NAME))

unemployment <- read.csv("data/unemployment-by-county-us/output.csv") %>% 
  mutate(State = tolower(State),
         County = tolower(County),
         County = sub("saint ", "st. ", County),
         County = sub("sainte ", "ste. ", County),
         County = sub("\\'", "", County),
         County = sub("la salle", "lasalle", County),
         County = sub("debaca", "de baca", County),
         County = sub("mc kean", "mckean", County),
         County = sub(" county", "", County),
         County = sub(" city", "", County)) %>% 
  left_join(state_fips, by = c("State" = "STATE_NAME"))

us_counties <- read.csv("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
                        header = FALSE, 
                        col.names = c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "CLASSFP")) %>% 
  mutate(COUNTYNAME = tolower(COUNTYNAME),
         COUNTYNAME = sub("\\'", "", COUNTYNAME),
         COUNTYNAME = sub(" county", "", COUNTYNAME),
         COUNTYNAME = sub(" city", "", COUNTYNAME),
         COUNTYNAME = sub("la salle", "lasalle", COUNTYNAME))

unemployment_fips <- 
  left_join(unemployment,
            us_counties, 
            by = c("STUSAB" = "STATE", "County" = "COUNTYNAME")) %>% 
  rename(county = COUNTYFP,
         YEAR = Year) %>% 
  group_by(YEAR, county, STATEFIP) %>% 
  summarize(avg_rate = mean(Rate)) %>% 
  ungroup() %>% 
  filter(YEAR %in% c(2000:2016))

## county populations 1960 to 1990
## thesis
##
## andrew wolff
## 6 april 2018

# county populations ------------------------------------------------------

read_county <- function(state_name){
  state <- read_xlsx(paste0("data/county populations/", state_name, ".xlsx"),
                     skip = 1,
                     col_names = c("FIPS", "1990", "1980", "1970", "1960", "county"),
                     na = "---") %>% 
    mutate(county = tolower(county)) %>% 
    filter(!(county %in% c("united states", state_name)))
  return(state)
}

state_names <- c("alabama", 
                 "arizona", 
                 "arkansas", 
                 "california",
                 "colorado", 
                 "connecticut", 
                 "delaware",
                 "district of columbia", 
                 "florida", 
                 "georgia", 
                 "hawaii", 
                 "idaho", 
                 "illinois", 
                 "indiana",
                 "iowa", 
                 "kansas", 
                 "kentucky", 
                 "louisiana",
                 "maine", 
                 "maryland", 
                 "massachusetts",
                 "michigan", 
                 "minnesota", 
                 "mississippi",
                 "missouri", 
                 "montana", 
                 "nebraska",
                 "nevada", 
                 "new hampshire", 
                 "new jersey",
                 "new mexico", 
                 "new york", 
                 "north carolina",
                 "north dakota", 
                 "ohio", 
                 "oklahoma", 
                 "oregon",
                 "pennsylvania", 
                 "rhode island", 
                 "south carolina",
                 "south dakota", 
                 "tennessee", 
                 "texas", 
                 "utah",
                 "vermont", 
                 "virginia", 
                 "washington", 
                 "west virginia",
                 "wisconsin",
                 "wyoming")

county_populations <- data.frame(matrix(ncol = 6,
                                        nrow = 0))

colnames(county_populations) <- c("FIPS",
                                  "1990",
                                  "1980",
                                  "1970",
                                  "1960",
                                  "county")

for(state in state_names){
  state_counties <- read_county(state)
  county_populations <- rbind(county_populations, state_counties)
  rm(state_counties)
}


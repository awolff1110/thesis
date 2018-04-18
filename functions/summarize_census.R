## summarize census
## thesis
## 
## andrew wolff
## 6 april 2018

summarize_census <- function(coded_census, YEAR){
  census_demographics <- 
    coded_census %>%
    group_by(FIPS) %>% 
    summarize(population = sum(person_weight),
              households = sum(house_weight),
              pct_black = sum(black * person_weight) / population * 100,
              pct_poverty = sum(poverty * house_weight) / households * 100,
              foreign_born = sum(immigrant * person_weight) / population * 100,
              young_population = sum(young_male * person_weight) / population * 100,
              family_instability = sum(unstable_family * house_weight) / households * 100,
              manufacturing = sum(manufacture * person_weight) / population * 100,
              low_skill_sector = sum(low_skill_service * person_weight) / population * 100,
              unemployment = sum(unemployed * person_weight) / population * 100,
              ln_pct_black = log(pct_black + 1),
              ln_foreign_born = log(foreign_born + 1)) %>%
    ungroup() %>% 
    mutate(year = YEAR,
           census = paste0("census_", as.character(YEAR)))
}

## total 1975 offense data
## thesis
## 
## andrew wolff
## 6 april 2018

total_75_offense_data <- function(dta, YEAR){
  total_dta <- 
    dta %>% 
    mutate(total_murder = as.numeric(jan_murder) + as.numeric(feb_murder) + as.numeric(mar_murder) + 
             as.numeric(apr_murder) + as.numeric(may_murder) + as.numeric(jun_murder) + as.numeric(jul_murder) + 
             as.numeric(aug_murder) + as.numeric(sep_murder) + as.numeric(oct_murder) + as.numeric(nov_murder) + 
             as.numeric(dec_murder),
           total_rape = as.numeric(jan_rape) + as.numeric(feb_rape) + as.numeric(mar_rape) + 
             as.numeric(apr_rape) + as.numeric(may_rape) + as.numeric(jun_rape) + as.numeric(jul_rape) + 
             as.numeric(aug_rape) + as.numeric(sep_rape) + as.numeric(oct_rape) + as.numeric(nov_rape) + 
             as.numeric(dec_rape),
           total_robbery = as.numeric(jan_robbery) + as.numeric(feb_robbery) + as.numeric(mar_robbery) + 
             as.numeric(apr_robbery) + as.numeric(may_robbery) + as.numeric(jun_robbery) + as.numeric(jul_robbery) + 
             as.numeric(aug_robbery) + as.numeric(sep_robbery) + as.numeric(oct_robbery) + as.numeric(nov_robbery) + 
             as.numeric(dec_robbery),
           total_assault = as.numeric(jan_assault) + as.numeric(feb_assault) + as.numeric(mar_assault) + 
             as.numeric(apr_assault) + as.numeric(may_assault) + as.numeric(jun_assault) + as.numeric(jul_assault) + 
             as.numeric(aug_assault) + as.numeric(sep_assault) + as.numeric(oct_assault) + as.numeric(nov_assault) + 
             as.numeric(dec_assault),
           total_burglary = as.numeric(jan_burglary) + as.numeric(feb_burglary) + as.numeric(mar_burglary) + 
             as.numeric(apr_burglary) + as.numeric(may_burglary) + as.numeric(jun_burglary) + as.numeric(jul_burglary) + 
             as.numeric(aug_burglary) + as.numeric(sep_burglary) + as.numeric(oct_burglary) + as.numeric(nov_burglary) + 
             as.numeric(dec_burglary),
           total_all = as.numeric(jan_all) + as.numeric(feb_all) + as.numeric(mar_all) + 
             as.numeric(apr_all) + as.numeric(may_all) + as.numeric(jun_all) + as.numeric(jul_all) + 
             as.numeric(aug_all) + as.numeric(sep_all) + as.numeric(oct_all) + as.numeric(nov_all) + 
             as.numeric(dec_all),
           year = YEAR) %>% 
    select(state, county1, total_murder, total_rape, total_robbery, total_assault, total_burglary, total_all, year) %>% 
    filter(!is.na(total_murder)) %>% 
    group_by(state, county1, year) %>% 
    summarize(murder = sum(total_murder),
              rape = sum(total_rape),
              robbery = sum(total_robbery),
              assault = sum(total_assault),
              burglary = sum(total_burglary),
              all = sum(total_all)) %>% 
    ungroup()
  
  return(total_dta)
}

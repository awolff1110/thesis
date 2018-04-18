## total offense data function
## thesis
## 
## andrew wolff
## 6 april 2018

total_offense_data <- function(crime_data, YEAR) {
  as.data.frame(apply(crime_data, 2, function(x) as.numeric(x))) %>% 
    mutate(total_murder = jan_murder + feb_murder + mar_murder + apr_murder +
             may_murder + jun_murder + jul_murder + aug_murder + sep_murder +
             oct_murder + nov_murder + dec_murder,
           total_rape = jan_rape + feb_rape + mar_rape + apr_rape + 
             may_rape + jun_rape + jul_rape + aug_rape + sep_rape +
             oct_rape + nov_rape + dec_rape,
           total_robbery = jan_robbery + feb_robbery + mar_robbery + apr_robbery +
             may_robbery + jun_robbery + jul_robbery + aug_robbery + sep_robbery +
             oct_robbery + nov_robbery + dec_robbery,
           total_assault = jan_assault + feb_assault + mar_assault + apr_assault +
             may_assault + jun_assault + jul_assault + aug_assault + sep_assault +
             oct_assault + nov_assault + dec_assault,
           total_burglary = jan_burglary + feb_burglary + mar_burglary + apr_burglary +
             may_burglary + jun_burglary + jul_burglary + aug_burglary + sep_burglary +
             oct_burglary + nov_burglary + dec_burglary,
           total_all = jan_all + feb_all + mar_all + apr_all +
             may_all + jun_all + jul_all + aug_all + sep_all +
             oct_all + nov_all + dec_all,
           year = YEAR) %>% 
    select(state:county1, total_murder:total_all, year) %>% 
    group_by(state, county1, year) %>% 
    summarise(murder = sum(total_murder),
              rape = sum(total_rape),
              robbery = sum(total_robbery),
              assault = sum(total_assault),
              burglary = sum(total_burglary),
              all = sum(total_all)) %>% 
    ungroup()
}  

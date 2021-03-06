## read offense data function
## thesis
## 
## andrew wolff
## 6 april 2018

read_offense_data <- function(crime_data_list, year, series_number) {
  read.table(paste0("data/crime_data/", 
                    year, 
                    "/ICPSR_", 
                    series_number, 
                    "/DS0001/", 
                    series_number,
                    "-0001-Data.txt"),
             sep = "|", quote = "", fill = FALSE) %>% 
    mutate(state = substr(V1, crime_data_list[[1]][1],crime_data_list[[1]][2]),
           pop1 = substr(V1, crime_data_list[[2]][1],crime_data_list[[2]][2]),
           county1 = substr(V1, crime_data_list[[3]][1],crime_data_list[[3]][2]),
           jan_murder = substr(V1, crime_data_list[[4]][1],crime_data_list[[4]][2]),
           jan_rape = substr(V1, crime_data_list[[5]][1],crime_data_list[[5]][2]),
           jan_robbery = substr(V1, crime_data_list[[6]][1],crime_data_list[[6]][2]),
           jan_assault = substr(V1, crime_data_list[[7]][1],crime_data_list[[7]][2]),
           jan_burglary = substr(V1, crime_data_list[[8]][1],crime_data_list[[8]][2]),
           jan_all = substr(V1, crime_data_list[[9]][1],crime_data_list[[9]][2]),
           feb_murder = substr(V1, crime_data_list[[10]][1],crime_data_list[[10]][2]),
           feb_rape = substr(V1, crime_data_list[[11]][1],crime_data_list[[11]][2]),
           feb_robbery = substr(V1, crime_data_list[[12]][1],crime_data_list[[12]][2]),
           feb_assault = substr(V1, crime_data_list[[13]][1],crime_data_list[[13]][2]),
           feb_burglary = substr(V1, crime_data_list[[14]][1],crime_data_list[[14]][2]),
           feb_all = substr(V1, crime_data_list[[15]][1],crime_data_list[[15]][2]),
           mar_murder = substr(V1, crime_data_list[[16]][1],crime_data_list[[16]][2]),
           mar_rape = substr(V1, crime_data_list[[17]][1],crime_data_list[[17]][2]),
           mar_robbery = substr(V1, crime_data_list[[18]][1],crime_data_list[[18]][2]),
           mar_assault = substr(V1, crime_data_list[[19]][1],crime_data_list[[19]][2]),
           mar_burglary = substr(V1, crime_data_list[[20]][1],crime_data_list[[20]][2]),
           mar_all = substr(V1, crime_data_list[[21]][1],crime_data_list[[21]][2]),
           apr_murder = substr(V1, crime_data_list[[22]][1],crime_data_list[[22]][2]),
           apr_rape = substr(V1, crime_data_list[[23]][1],crime_data_list[[23]][2]),
           apr_robbery = substr(V1, crime_data_list[[24]][1],crime_data_list[[24]][2]),
           apr_assault = substr(V1, crime_data_list[[25]][1],crime_data_list[[25]][2]),
           apr_burglary = substr(V1, crime_data_list[[26]][1],crime_data_list[[26]][2]),
           apr_all = substr(V1, crime_data_list[[27]][1],crime_data_list[[27]][2]),
           may_murder = substr(V1, crime_data_list[[28]][1],crime_data_list[[28]][2]),
           may_rape = substr(V1, crime_data_list[[29]][1],crime_data_list[[29]][2]),
           may_robbery = substr(V1, crime_data_list[[30]][1],crime_data_list[[30]][2]),
           may_assault = substr(V1, crime_data_list[[31]][1],crime_data_list[[31]][2]),
           may_burglary = substr(V1, crime_data_list[[32]][1],crime_data_list[[32]][2]),
           may_all = substr(V1, crime_data_list[[33]][1],crime_data_list[[33]][2]),
           jun_murder = substr(V1, crime_data_list[[34]][1],crime_data_list[[34]][2]),
           jun_rape = substr(V1, crime_data_list[[35]][1],crime_data_list[[35]][2]),
           jun_robbery = substr(V1, crime_data_list[[36]][1],crime_data_list[[36]][2]),
           jun_assault = substr(V1, crime_data_list[[37]][1],crime_data_list[[37]][2]),
           jun_burglary = substr(V1, crime_data_list[[38]][1],crime_data_list[[38]][2]),
           jun_all = substr(V1, crime_data_list[[39]][1],crime_data_list[[39]][2]),
           jul_murder = substr(V1, crime_data_list[[40]][1],crime_data_list[[40]][2]),
           jul_rape = substr(V1, crime_data_list[[41]][1],crime_data_list[[41]][2]),
           jul_robbery = substr(V1, crime_data_list[[42]][1],crime_data_list[[42]][2]),
           jul_assault = substr(V1, crime_data_list[[43]][1],crime_data_list[[43]][2]),
           jul_burglary = substr(V1, crime_data_list[[44]][1],crime_data_list[[44]][2]),
           jul_all = substr(V1, crime_data_list[[45]][1],crime_data_list[[45]][2]),
           aug_murder = substr(V1, crime_data_list[[46]][1],crime_data_list[[46]][2]),
           aug_rape = substr(V1, crime_data_list[[47]][1],crime_data_list[[47]][2]),
           aug_robbery = substr(V1, crime_data_list[[48]][1],crime_data_list[[48]][2]),
           aug_assault = substr(V1, crime_data_list[[49]][1],crime_data_list[[49]][2]),
           aug_burglary = substr(V1, crime_data_list[[50]][1],crime_data_list[[50]][2]),
           aug_all = substr(V1, crime_data_list[[51]][1],crime_data_list[[51]][2]),
           sep_murder = substr(V1, crime_data_list[[52]][1],crime_data_list[[52]][2]),
           sep_rape = substr(V1, crime_data_list[[53]][1],crime_data_list[[53]][2]),
           sep_robbery = substr(V1, crime_data_list[[54]][1],crime_data_list[[54]][2]),
           sep_assault = substr(V1, crime_data_list[[55]][1],crime_data_list[[55]][2]),
           sep_burglary = substr(V1, crime_data_list[[56]][1],crime_data_list[[56]][2]),
           sep_all = substr(V1, crime_data_list[[57]][1],crime_data_list[[57]][2]),
           oct_murder = substr(V1, crime_data_list[[58]][1],crime_data_list[[58]][2]),
           oct_rape = substr(V1, crime_data_list[[59]][1],crime_data_list[[59]][2]),
           oct_robbery = substr(V1, crime_data_list[[60]][1],crime_data_list[[60]][2]),
           oct_assault = substr(V1, crime_data_list[[61]][1],crime_data_list[[61]][2]),
           oct_burglary = substr(V1, crime_data_list[[62]][1],crime_data_list[[62]][2]),
           oct_all = substr(V1, crime_data_list[[63]][1],crime_data_list[[63]][2]),
           nov_murder = substr(V1, crime_data_list[[64]][1],crime_data_list[[64]][2]),
           nov_rape = substr(V1, crime_data_list[[65]][1],crime_data_list[[65]][2]),
           nov_robbery = substr(V1, crime_data_list[[66]][1],crime_data_list[[66]][2]),
           nov_assault = substr(V1, crime_data_list[[67]][1],crime_data_list[[67]][2]),
           nov_burglary = substr(V1, crime_data_list[[68]][1],crime_data_list[[68]][2]),
           nov_all = substr(V1, crime_data_list[[69]][1],crime_data_list[[69]][2]),
           dec_murder = substr(V1, crime_data_list[[70]][1],crime_data_list[[70]][2]),
           dec_rape = substr(V1, crime_data_list[[71]][1],crime_data_list[[71]][2]),
           dec_robbery = substr(V1, crime_data_list[[72]][1],crime_data_list[[72]][2]),
           dec_assault = substr(V1, crime_data_list[[73]][1],crime_data_list[[73]][2]),
           dec_burglary = substr(V1, crime_data_list[[74]][1],crime_data_list[[74]][2]),
           dec_all = substr(V1, crime_data_list[[75]][1],crime_data_list[[75]][2])) %>% 
    select(-V1)
}

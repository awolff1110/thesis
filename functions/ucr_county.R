## ucr county crosswalk
## thesis
##
## andrew wolff
## 6 april 2018


# initial settings --------------------------------------------------------



suppressPackageStartupMessages(library(tidyverse))
options(stringsAsFactors = FALSE)
source("functions/county_populations.R")

# ucr county population ---------------------------------------------------


ucr_code_xwalk <- read.table("data/ucr_crosswalk/ICPSR_02876/DS0001/02876-0001-Data.txt",
                             sep = "|", quote = "", fill = FALSE) %>% 
  mutate(UORI = substr(V1, 4, 11),
         UAGENCY = substr(V1, 12, 59),
         UCORI = substr(V1, 60, 60),
         UMULTICO = substr(V1, 61, 61),
         USTATENO = substr(V1, 62, 63),
         UCOUNTY = substr(V1, 64, 66),
         UCTYNAME = substr(V1, 67, 97),
         UPOPCOV = substr(V1, 103, 111),
         UZIP = substr(V1, 232, 236),
         FSTATE = substr(V1, 331, 332),
         FCOUNTY = substr(V1, 333, 335)) %>% 
  select(-V1)

ucr_county <- ucr_code_xwalk %>% 
  distinct(USTATENO, UCOUNTY, FSTATE, FCOUNTY) %>% 
  mutate(FCOUNTY = ifelse(USTATENO == "45" & UCOUNTY == "000", "059", FCOUNTY)) %>% 
  mutate(FIPS = paste0(FSTATE, FCOUNTY),
         FIPS = as.numeric(FIPS)) %>% 
  left_join(county_populations,
            by = "FIPS") %>% 
  filter(!is.na(county))
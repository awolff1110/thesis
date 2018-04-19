## 1975 crime data
## thesis
## 
## andrew wolff
## 6 april 2018

# initial settings --------------------------------------------------------

source("functions/total_75_offense_data.R")
source("functions/ucr_county.R")
source("functions/county_populations.R")


# crime data --------------------------------------------------------------

crime1975 <- 
  read.table("data/crime_data/1975/ICPSR_09028/DS0051/09028-0051-Data.txt", sep = "|") %>% 
  mutate(state = substr(V1, 2, 3),
         county1 = substr(V1, 53, 55),
         jan_murder = substr(V1, 280, 282),
         jan_rape = substr(V1, 285, 287),
         jan_robbery = substr(V1, 293, 296),
         jan_assault = substr(V1, 312, 315),
         jan_burglary = substr(V1, 334, 338),
         jan_all = substr(V1, 371, 375),
         feb_murder = substr(V1, 557, 559),
         feb_rape = substr(V1, 562, 564),
         feb_robbery = substr(V1, 570, 573),
         feb_assault = substr(V1, 589, 592),
         feb_burglary = substr(V1, 611, 615),
         feb_all = substr(V1, 647, 651),
         mar_murder = substr(V1, 840, 842),
         mar_rape = substr(V1, 845, 847),
         mar_robbery = substr(V1, 853, 856),
         mar_assault = substr(V1, 873, 876),
         mar_burglary = substr(V1, 895, 899),
         mar_all = substr(V1, 932, 936),
         apr_murder = substr(V1, 1125, 1127),
         apr_rape = substr(V1, 1130, 1132),
         apr_robbery = substr(V1, 1138, 1141),
         apr_assault = substr(V1, 1157, 1160),
         apr_burglary = substr(V1, 1179, 1183),
         apr_all = substr(V1, 1216, 1220),
         may_murder = substr(V1, 1410, 1412),
         may_rape = substr(V1, 1415, 1417),
         may_robbery = substr(V1, 1423, 1426),
         may_assault = substr(V1, 1443, 1446),
         may_burglary = substr(V1, 1465, 1469),
         may_all = substr(V1, 1502, 1506),
         jun_murder = substr(V1, 1694, 1696),
         jun_rape = substr(V1, 1699, 1701),
         jun_robbery = substr(V1, 1707, 1710),
         jun_assault = substr(V1, 1727, 1730),
         jun_burglary = substr(V1, 1749, 1753),
         jun_all = substr(V1, 1786, 1790),
         jul_murder = substr(V1, 1976, 1978),
         jul_rape = substr(V1, 1981, 1983),
         jul_robbery = substr(V1, 1989, 1992),
         jul_assault = substr(V1, 2009, 2012),
         jul_burglary = substr(V1, 2031, 2035),
         jul_all = substr(V1, 2068, 2072),
         aug_murder = substr(V1, 2255, 2257),
         aug_rape = substr(V1, 2260, 2262),
         aug_robbery = substr(V1, 2268, 2271),
         aug_assault = substr(V1, 2288, 2291),
         aug_burglary = substr(V1, 2310, 2314),
         aug_all = substr(V1, 2347, 2351),
         sep_murder = substr(V1, 2535, 2537),
         sep_rape = substr(V1, 2540, 2542),
         sep_robbery = substr(V1, 2548, 2551),
         sep_assault = substr(V1, 2568, 2571),
         sep_burglary = substr(V1, 2590, 2594),
         sep_all = substr(V1, 2627, 2631),
         oct_murder = substr(V1, 2821, 2823),
         oct_rape = substr(V1, 2826, 2828),
         oct_robbery = substr(V1, 2834, 2837),
         oct_assault = substr(V1, 2854, 2857),
         oct_burglary = substr(V1, 2876, 2880),
         oct_all = substr(V1, 2913, 2917),
         nov_murder = substr(V1, 3104, 3106),
         nov_rape = substr(V1, 3109, 3111),
         nov_robbery = substr(V1, 3117, 3120),
         nov_assault = substr(V1, 3137, 3140),
         nov_burglary = substr(V1, 3159, 3163),
         nov_all = substr(V1, 3196, 3200),
         dec_murder = substr(V1, 3381, 3383),
         dec_rape = substr(V1, 3386, 3388),
         dec_robbery = substr(V1, 3394, 3397),
         dec_assault = substr(V1, 3414, 3417),
         dec_burglary = substr(V1, 3434, 3438),
         dec_all = substr(V1, 3471, 3475)) %>% 
  select(-V1)

crime1975_totals <- total_75_offense_data(crime1975, 1975) %>% filter()

crime1976 <- 
  read.table("data/crime_data/1976/ICPSR_09028/DS0049/09028-0049-Data.txt", sep = "|") %>% 
  mutate(state = substr(V1, 13, 14),
         county1 = substr(V1, 65, 67),
         jan_murder = substr(V1, 2359, 2361),
         feb_murder = substr(V1, 2362, 2364),
         mar_murder = substr(V1, 2365, 2367),
         apr_murder = substr(V1, 2368, 2370),
         may_murder = substr(V1, 2371, 2373),
         jun_murder = substr(V1, 2374, 2376),
         jul_murder = substr(V1, 2377, 2379),
         aug_murder = substr(V1, 2380, 2382),
         sep_murder = substr(V1, 2383, 2385),
         oct_murder = substr(V1, 2386, 2388),
         nov_murder = substr(V1, 2389, 2391),
         dec_murder = substr(V1, 2392, 2394),
         jan_rape = substr(V1, 2419, 2421),
         feb_rape = substr(V1, 2422, 2424),
         mar_rape = substr(V1, 2425, 2427),
         apr_rape = substr(V1, 2428, 2430),
         may_rape = substr(V1, 2431, 2433),
         jun_rape = substr(V1, 2434, 2436),
         jul_rape = substr(V1, 2437, 2439),
         aug_rape = substr(V1, 2440, 2442),
         sep_rape = substr(V1, 2443, 2445),
         oct_rape = substr(V1, 2446, 2448),
         nov_rape = substr(V1, 2449, 2451),
         dec_rape = substr(V1, 2452, 2454),
         jan_robbery = substr(V1, 2516, 2519),
         feb_robbery = substr(V1, 2520, 2523),
         mar_robbery = substr(V1, 2524, 2527),
         apr_robbery = substr(V1, 2528, 2531),
         may_robbery = substr(V1, 2532, 2535),
         jun_robbery = substr(V1, 2536, 2539),
         jul_robbery = substr(V1, 2540, 2543),
         aug_robbery = substr(V1, 2544, 2547),
         sep_robbery = substr(V1, 2548, 2551),
         oct_robbery = substr(V1, 2552, 2555),
         nov_robbery = substr(V1, 2556, 2559),
         dec_robbery = substr(V1, 2560, 2563),
         jan_assault = substr(V1, 2749, 2752),
         feb_assault = substr(V1, 2753, 2756),
         mar_assault = substr(V1, 2757, 2760),
         apr_assault = substr(V1, 2761, 2764),
         may_assault = substr(V1, 2765, 2768),
         jun_assault = substr(V1, 2769, 2773),
         jul_assault = substr(V1, 2774, 2778),
         aug_assault = substr(V1, 2779, 2782),
         sep_assault = substr(V1, 2783, 2786),
         oct_assault = substr(V1, 2787, 2790),
         nov_assault = substr(V1, 2791, 2794),
         dec_assault = substr(V1, 2795, 2798),
         jan_burglary = substr(V1, 3019, 3023),
         feb_burglary = substr(V1, 3024, 3028),
         mar_burglary = substr(V1, 3029, 3033),
         apr_burglary = substr(V1, 3034, 3038),
         may_burglary = substr(V1, 3039, 3043),
         jun_burglary = substr(V1, 3044, 3048),
         jul_burglary = substr(V1, 3049, 3053),
         aug_burglary = substr(V1, 3054, 3058),
         sep_burglary = substr(V1, 3059, 3063),
         oct_burglary = substr(V1, 3064, 3068),
         nov_burglary = substr(V1, 3069, 3073),
         dec_burglary = substr(V1, 3074, 3078),
         jan_all = substr(V1, 3468, 3472),
         feb_all = substr(V1, 3473, 3477),
         mar_all = substr(V1, 3478, 3482),
         apr_all = substr(V1, 3483, 3487),
         may_all = substr(V1, 3488, 3492),
         jun_all = substr(V1, 3493, 3497),
         jul_all = substr(V1, 3498, 3502),
         aug_all = substr(V1, 3503, 3507),
         sep_all = substr(V1, 3508, 3512),
         oct_all = substr(V1, 3513, 3517),
         nov_all = substr(V1, 3518, 3522),
         dec_all = substr(V1, 3523, 3527)) %>% 
  select(-V1)

crime1976_totals <- total_75_offense_data(crime1976, 1976)

crime1975_1976_totals <- rbind(crime1975_totals, crime1976_totals)

# convert counties --------------------------------------------------------

ucr_county_pop <- ucr_county %>% 
  group_by(USTATENO, UCOUNTY) %>% 
  summarize(population = sum(`1970`)) %>% 
  ungroup()

ucr_fips_county_1975 <- ucr_county %>% 
  left_join(ucr_county_pop,
            by = c("USTATENO", "UCOUNTY")) %>% 
  mutate(weight = `1970` / population,
         USTATENO = as.numeric(USTATENO),
         UCOUNTY = as.numeric(UCOUNTY))

crime1975_1976_county <- 
  crime1975_1976_totals %>% 
  mutate(county1 = as.numeric(county1),
         state = as.numeric(state),
         county1 = ifelse(is.na(county1), 0, county1),
         county1 = ifelse(state == 31 & county1 == 0, 24, county1)) %>% 
  left_join(ucr_fips_county_1975,
            by = c("state" = "USTATENO", "county1" = "UCOUNTY")) %>% 
  filter(!is.na(weight)) %>% 
  mutate(murder = murder * weight,
         rape = rape * weight,
         robbery = robbery * weight,
         assault = assault * weight,
         burglary = burglary * weight,
         all = all * weight) %>% 
  group_by(FIPS, year) %>% 
  summarize(murder = round(sum(murder)),
            rape = round(sum(rape)),
            robbery = round(sum(robbery)),
            assault = round(sum(assault)),
            burglary = round(sum(burglary)),
            all = round(sum(all))) %>% 
  ungroup() %>% 
  filter(!is.na(robbery))


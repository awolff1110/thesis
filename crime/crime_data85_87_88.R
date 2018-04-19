## 1985 crime data
## thesis
## 
## andrew wolff
## 6 april 2018

# initial settings --------------------------------------------------------

source("functions/total_75_offense_data.R")
source("functions/ucr_county.R")
source("functions/county_populations.R")




# crime data --------------------------------------------------------------

crime1985 <- 
  read.table("data/crime_data/1985/ICPSR_09028/DS0035/09028-0035-Data.txt", 
             sep = "|", quote = "", fill = FALSE) %>% 
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
         jan_robbery = substr(V1, 2515, 2518),
         feb_robbery = substr(V1, 2519, 2522),
         mar_robbery = substr(V1, 2523, 2526),
         apr_robbery = substr(V1, 2527, 2530),
         may_robbery = substr(V1, 2431, 2434),
         jun_robbery = substr(V1, 2535, 2538),
         jul_robbery = substr(V1, 2539, 2542),
         aug_robbery = substr(V1, 2543, 2546),
         sep_robbery = substr(V1, 2547, 2550),
         oct_robbery = substr(V1, 2551, 2554),
         nov_robbery = substr(V1, 2555, 2558),
         dec_robbery = substr(V1, 2559, 2562),
         jan_assault = substr(V1, 2755, 2758),
         feb_assault = substr(V1, 2759, 2762),
         mar_assault = substr(V1, 2763, 2766),
         apr_assault = substr(V1, 2767, 2770),
         may_assault = substr(V1, 2771, 2774),
         jun_assault = substr(V1, 2775, 2778),
         jul_assault = substr(V1, 2779, 2782),
         aug_assault = substr(V1, 2783, 2786),
         sep_assault = substr(V1, 2787, 2790),
         oct_assault = substr(V1, 2791, 2794),
         nov_assault = substr(V1, 2795, 2798),
         dec_assault = substr(V1, 2799, 2802),
         jan_burglary = substr(V1, 3043, 3047),
         feb_burglary = substr(V1, 3048, 3052),
         mar_burglary = substr(V1, 3053, 3057),
         apr_burglary = substr(V1, 3058, 3062),
         may_burglary = substr(V1, 3063, 3067),
         jun_burglary = substr(V1, 3068, 3072),
         jul_burglary = substr(V1, 3073, 3077),
         aug_burglary = substr(V1, 3078, 3082),
         sep_burglary = substr(V1, 3083, 3087),
         oct_burglary = substr(V1, 3088, 3092),
         nov_burglary = substr(V1, 3093, 3097),
         dec_burglary = substr(V1, 3098, 3102),
         jan_all = substr(V1, 3499, 3503),
         feb_all = substr(V1, 3504, 3508),
         mar_all = substr(V1, 3509, 3513),
         apr_all = substr(V1, 3514, 3518),
         may_all = substr(V1, 3519, 3523),
         jun_all = substr(V1, 3524, 3528),
         jul_all = substr(V1, 3529, 3533),
         aug_all = substr(V1, 3534, 3538),
         sep_all = substr(V1, 3539, 3543),
         oct_all = substr(V1, 3544, 3548),
         nov_all = substr(V1, 3549, 3553),
         dec_all = substr(V1, 3554, 3558)) %>% 
  select(-V1)

crime1985_totals <- total_75_offense_data(crime1985, 1985) %>% 
  filter(!is.na(robbery))


crime1987 <- 
  read.table("data/crime_data/1987/ICPSR_09028/DS0055/09028-0055-Data.txt", 
             sep = "|", quote = "", fill = FALSE) %>% 
  mutate(state = substr(V1, 13, 14),
         county1 = substr(V1, 64, 66),
         jan_murder = substr(V1, 1245, 1247),
         feb_murder = substr(V1, 1248, 1250),
         mar_murder = substr(V1, 1251, 1253),
         apr_murder = substr(V1, 1254, 1256),
         may_murder = substr(V1, 1257, 1259),
         jun_murder = substr(V1, 1260, 1262),
         jul_murder = substr(V1, 1263, 1265),
         aug_murder = substr(V1, 1266, 1268),
         sep_murder = substr(V1, 1269, 1271),
         oct_murder = substr(V1, 1272, 1274),
         nov_murder = substr(V1, 1275, 1277),
         dec_murder = substr(V1, 1278, 1280),
         jan_rape = substr(V1, 1305, 1307),
         feb_rape = substr(V1, 1308, 1310),
         mar_rape = substr(V1, 1311, 1313),
         apr_rape = substr(V1, 1314, 1316),
         may_rape = substr(V1, 1317, 1319),
         jun_rape = substr(V1, 1320, 1322),
         jul_rape = substr(V1, 1323, 1325),
         aug_rape = substr(V1, 1326, 1328),
         sep_rape = substr(V1, 1329, 1331),
         oct_rape = substr(V1, 1332, 1334),
         nov_rape = substr(V1, 1335, 1337),
         dec_rape = substr(V1, 1338, 1340),
         jan_robbery = substr(V1, 1401, 1404),
         feb_robbery = substr(V1, 1405, 1408),
         mar_robbery = substr(V1, 1409, 1412),
         apr_robbery = substr(V1, 1413, 1416),
         may_robbery = substr(V1, 1417, 1420),
         jun_robbery = substr(V1, 1421, 1424),
         jul_robbery = substr(V1, 1425, 1428),
         aug_robbery = substr(V1, 1429, 1432),
         sep_robbery = substr(V1, 1433, 1436),
         oct_robbery = substr(V1, 1437, 1440),
         nov_robbery = substr(V1, 1441, 1444),
         dec_robbery = substr(V1, 1445, 1448),
         jan_assault = substr(V1, 1641, 1644),
         feb_assault = substr(V1, 1645, 1648),
         mar_assault = substr(V1, 1649, 1652),
         apr_assault = substr(V1, 1653, 1656),
         may_assault = substr(V1, 1657, 1660),
         jun_assault = substr(V1, 1661, 1664),
         jul_assault = substr(V1, 1665, 1668),
         aug_assault = substr(V1, 1669, 1672),
         sep_assault = substr(V1, 1673, 1676),
         oct_assault = substr(V1, 1677, 1680),
         nov_assault = substr(V1, 1681, 1684),
         dec_assault = substr(V1, 1685, 1688),
         jan_burglary = substr(V1, 1929, 1933),
         feb_burglary = substr(V1, 1934, 1938),
         mar_burglary = substr(V1, 1939, 1943),
         apr_burglary = substr(V1, 1944, 1948),
         may_burglary = substr(V1, 1949, 1953),
         jun_burglary = substr(V1, 1954, 1958),
         jul_burglary = substr(V1, 1959, 1963),
         aug_burglary = substr(V1, 1964, 1968),
         sep_burglary = substr(V1, 1969, 1973),
         oct_burglary = substr(V1, 1974, 1978),
         nov_burglary = substr(V1, 1979, 1983),
         dec_burglary = substr(V1, 1984, 1988),
         jan_all = substr(V1, 2397, 2401),
         feb_all = substr(V1, 2402, 2406),
         mar_all = substr(V1, 2407, 2411),
         apr_all = substr(V1, 2412, 2416),
         may_all = substr(V1, 2417, 2421),
         jun_all = substr(V1, 2422, 2426),
         jul_all = substr(V1, 2427, 2431),
         aug_all = substr(V1, 2432, 2436),
         sep_all = substr(V1, 2437, 2441),
         oct_all = substr(V1, 2442, 2446),
         nov_all = substr(V1, 2447, 2451),
         dec_all = substr(V1, 2452, 2456)) %>% 
  select(-V1)

crime1987_totals <- total_75_offense_data(crime1987, 1987) %>% 
  filter(!is.na(robbery))

crime1988 <- 
  read.table("data/crime_data/1988/ICPSR_09028/DS0059/09028-0059-Data.txt", 
             sep = "|", quote = "", fill = FALSE) %>% 
  mutate(state = substr(V1, 13, 14),
         county1 = substr(V1, 64, 66),
         jan_murder = substr(V1, 1245, 1247),
         feb_murder = substr(V1, 1248, 1250),
         mar_murder = substr(V1, 1251, 1253),
         apr_murder = substr(V1, 1254, 1256),
         may_murder = substr(V1, 1257, 1259),
         jun_murder = substr(V1, 1260, 1262),
         jul_murder = substr(V1, 1263, 1265),
         aug_murder = substr(V1, 1266, 1268),
         sep_murder = substr(V1, 1269, 1271),
         oct_murder = substr(V1, 1272, 1274),
         nov_murder = substr(V1, 1275, 1277),
         dec_murder = substr(V1, 1278, 1280),
         jan_rape = substr(V1, 1305, 1307),
         feb_rape = substr(V1, 1308, 1310),
         mar_rape = substr(V1, 1311, 1313),
         apr_rape = substr(V1, 1314, 1316),
         may_rape = substr(V1, 1317, 1319),
         jun_rape = substr(V1, 1320, 1322),
         jul_rape = substr(V1, 1323, 1325),
         aug_rape = substr(V1, 1326, 1328),
         sep_rape = substr(V1, 1329, 1331),
         oct_rape = substr(V1, 1332, 1334),
         nov_rape = substr(V1, 1335, 1337),
         dec_rape = substr(V1, 1338, 1340),
         jan_robbery = substr(V1, 1401, 1404),
         feb_robbery = substr(V1, 1405, 1408),
         mar_robbery = substr(V1, 1409, 1412),
         apr_robbery = substr(V1, 1413, 1416),
         may_robbery = substr(V1, 1417, 1420),
         jun_robbery = substr(V1, 1421, 1424),
         jul_robbery = substr(V1, 1425, 1428),
         aug_robbery = substr(V1, 1429, 1432),
         sep_robbery = substr(V1, 1433, 1436),
         oct_robbery = substr(V1, 1437, 1440),
         nov_robbery = substr(V1, 1441, 1444),
         dec_robbery = substr(V1, 1445, 1448),
         jan_assault = substr(V1, 1641, 1644),
         feb_assault = substr(V1, 1645, 1648),
         mar_assault = substr(V1, 1649, 1652),
         apr_assault = substr(V1, 1653, 1656),
         may_assault = substr(V1, 1657, 1660),
         jun_assault = substr(V1, 1661, 1664),
         jul_assault = substr(V1, 1665, 1668),
         aug_assault = substr(V1, 1669, 1672),
         sep_assault = substr(V1, 1673, 1676),
         oct_assault = substr(V1, 1677, 1680),
         nov_assault = substr(V1, 1681, 1684),
         dec_assault = substr(V1, 1685, 1688),
         jan_burglary = substr(V1, 1929, 1933),
         feb_burglary = substr(V1, 1934, 1938),
         mar_burglary = substr(V1, 1939, 1943),
         apr_burglary = substr(V1, 1944, 1948),
         may_burglary = substr(V1, 1949, 1953),
         jun_burglary = substr(V1, 1954, 1958),
         jul_burglary = substr(V1, 1959, 1963),
         aug_burglary = substr(V1, 1964, 1968),
         sep_burglary = substr(V1, 1969, 1973),
         oct_burglary = substr(V1, 1974, 1978),
         nov_burglary = substr(V1, 1979, 1983),
         dec_burglary = substr(V1, 1984, 1988),
         jan_all = substr(V1, 2397, 2401),
         feb_all = substr(V1, 2402, 2406),
         mar_all = substr(V1, 2407, 2411),
         apr_all = substr(V1, 2412, 2416),
         may_all = substr(V1, 2417, 2421),
         jun_all = substr(V1, 2422, 2426),
         jul_all = substr(V1, 2427, 2431),
         aug_all = substr(V1, 2432, 2436),
         sep_all = substr(V1, 2437, 2441),
         oct_all = substr(V1, 2442, 2446),
         nov_all = substr(V1, 2447, 2451),
         dec_all = substr(V1, 2452, 2456)) %>% 
  select(-V1)

crime1988_totals <- total_75_offense_data(crime1988, 1988) %>% 
  filter(!is.na(robbery))

crime85_87_88_totals <- rbind(
  crime1985_totals, crime1987_totals, crime1988_totals
)

# convert counties --------------------------------------------------------

ucr_county_pop <- ucr_county %>% 
  group_by(USTATENO, UCOUNTY) %>% 
  summarize(population = sum(`1980`)) %>% 
  ungroup()

ucr_fips_county_1985 <- ucr_county %>% 
  left_join(ucr_county_pop,
            by = c("USTATENO", "UCOUNTY")) %>% 
  mutate(weight = `1980` / population,
         USTATENO = as.numeric(USTATENO),
         UCOUNTY = as.numeric(UCOUNTY))

crime85_87_88_county <- 
  crime85_87_88_totals %>% 
  mutate(county1 = as.numeric(county1),
         state = as.numeric(state),
         county1 = ifelse(is.na(county1), 0, county1),
         county1 = ifelse(state == 31 & county1 == 0, 24, county1)) %>% 
  left_join(ucr_fips_county_1985,
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


## 1970 crime data
## thesis
## 
## andrew wolff
## 6 april 2018

# initial settings --------------------------------------------------------

setwd("~/Documents/thesis")
source("functions/read_offense_data.R")
source("functions/total_offense_data.R")
source("functions/ucr_county.R")
source("functions/county_populations.R")


# crime data --------------------------------------------------------------

crime1970_variables <- list(c(2, 3), c(46, 52), c(53, 55), # state, county pop, county
                            c(280, 281), c(284, 286), c(292, 295), 
                            c(306, 309), c(326, 330), c(358, 362), # january
                            c(532, 533), c(536, 538), c(544, 547), 
                            c(558, 561), c(578, 582), c(610, 614), # february
                            c(784, 785), c(788, 790), c(796, 799), 
                            c(810, 813), c(831, 835), c(863, 867), # march
                            c(1044, 1045), c(1048, 1050), c(1056, 1059), 
                            c(1070, 1073), c(1091, 1095), c(1123, 1127), # april
                            c(1302, 1303), c(1306, 1308), c(1314, 1317), 
                            c(1328, 1331), c(1349, 1353), c(1381, 1385), # may
                            c(1562, 1563), c(1566, 1568), c(1574, 1577), 
                            c(1588, 1591), c(1609, 1613), c(1641, 1645), # june
                            c(1821, 1823), c(1826, 1828), c(1834, 1837), 
                            c(1848, 1851), c(1869, 1873), c(1901, 1905), # july
                            c(2081, 2083), c(2086, 2088), c(2094, 2097), 
                            c(2108, 2111), c(2129, 2133), c(2161, 2165), # august
                            c(2343, 2344), c(2347, 2349), c(2355, 2358), 
                            c(2369, 2372), c(2390, 2394), c(2422, 2426), # september
                            c(2606, 2607), c(2610, 2612), c(2618, 2621), 
                            c(2632, 2635), c(2653, 2657), c(2685, 2689), # october
                            c(2866, 2868), c(2871, 2873), c(2879, 2882), 
                            c(2893, 2896), c(2914, 2918), c(2946, 2950), # november
                            c(3128, 3129), c(3132, 3134), c(3140, 3143), 
                            c(3154, 3157), c(3174, 3178), c(3206, 3210)) # december

crime1971_variables <- list(c(2, 3), c(46, 52), c(53, 55), # state, county pop, county
                            c(280, 282), c(285, 287), c(293, 296), 
                            c(307, 310), c(327, 331), c(359, 363), # january
                            c(535, 536), c(539, 541), c(547, 550), 
                            c(561, 564), c(581, 585), c(613, 617), # february
                            c(789, 791), c(794, 796), c(802, 805), 
                            c(816, 819), c(837, 841), c(869, 873), # march
                            c(1050, 1052), c(1055, 1057), c(1063, 1066), 
                            c(1077, 1080), c(1098, 1102), c(1130, 1134), # april
                            c(1310, 1311), c(1314, 1316), c(1322, 1325), 
                            c(1336, 1339), c(1357, 1361), c(1389, 1393), # may
                            c(1573, 1575), c(1578, 1580), c(1586, 1589), 
                            c(1600, 1603), c(1622, 1626), c(1654, 1658), # june
                            c(1834, 1836), c(1839, 1841), c(1847, 1850), 
                            c(1861, 1864), c(1883, 1887), c(1915, 1919), # july
                            c(2097, 2099), c(2102, 2104), c(2110, 2113), 
                            c(2124, 2127), c(2145, 2149), c(2177, 2181), # august
                            c(2354, 2356), c(2359, 2361), c(2367, 2370), 
                            c(2381, 2384), c(2402, 2406), c(2434, 2438), # september
                            c(2612, 2614), c(2617, 2619), c(2625, 2628), 
                            c(2639, 2642), c(2661, 2665), c(2693, 2697), # october
                            c(2873, 2875), c(2878, 2880), c(2886, 2889), 
                            c(2900, 2903), c(2920, 2924), c(2952, 2956), # november
                            c(3131, 3133), c(3136, 3138), c(3144, 3147), 
                            c(3158, 3161), c(3180, 3184), c(3212, 3216)) # december

crime1972_variables <- list(c(2, 3), c(46, 52), c(53, 55), # state, county pop, county
                            c(277, 279), c(282, 284), c(290, 293), 
                            c(304, 307), c(324, 328), c(356, 360), # january
                            c(530, 532), c(535, 537), c(543, 546), 
                            c(557, 560), c(577, 581), c(607, 611), # february
                            c(783, 785), c(788, 790), c(796, 799), 
                            c(810, 813), c(831, 835), c(862, 866), # march
                            c(1039, 1041), c(1044, 1046), c(1052, 1055), 
                            c(1066, 1069), c(1087, 1091), c(1119, 1123), # april
                            c(1295, 1297), c(1300, 1302), c(1308, 1311), 
                            c(1322, 1325), c(1344, 1348), c(1376, 1380), # may
                            c(1554, 1556), c(1559, 1561), c(1567, 1570), 
                            c(1581, 1584), c(1603, 1607), c(1635, 1639), # june
                            c(1815, 1817), c(1820, 1822), c(1828, 1831), 
                            c(1842, 1845), c(1864, 1868), c(1896, 1900), # july
                            c(2073, 2075), c(2078, 2080), c(2086, 2089), 
                            c(2100, 2103), c(2122, 2126), c(2154, 2158), # august
                            c(2337, 2339), c(2342, 2344), c(2350, 2353), 
                            c(2364, 2367), c(2386, 2390), c(2417, 2421), # september
                            c(2596, 2598), c(2601, 2603), c(2609, 2612), 
                            c(2623, 2626), c(2644, 2648), c(2675, 2679), # october
                            c(2856, 2858), c(2861, 2863), c(2869, 2872), 
                            c(2883, 2886), c(2904, 2908), c(2934, 2938), # november
                            c(3109, 3111), c(3114, 3116), c(3122, 3125), 
                            c(3136, 3139), c(3158, 3162), c(3189, 3193)) # december

crime1973_variables <- list(c(2, 3), c(46, 52), c(53, 55), 
                            c(282, 284), c(287, 289), c(295, 298), 
                            c(313, 315), c(330, 334), c(360, 364), 
                            c(536, 538), c(541, 543), c(549, 552), 
                            c(563, 566), c(583, 587), c(613, 617),
                            c(794, 796), c(799, 801), c(807, 810), 
                            c(821, 824), c(843, 847), c(873, 877), 
                            c(1056, 1058), c(1061, 1063), c(1069, 1072), 
                            c(1083, 1086), c(1105, 1109), c(1135, 1139), 
                            c(1315, 1317), c(1320, 1322), c(1328, 1331), 
                            c(1342, 1345), c(1364, 1368), c(1396, 1400),
                            c(1574, 1576), c(1579, 1581), c(1587, 1590), 
                            c(1601, 1604), c(1623, 1627), c(1654, 1658), 
                            c(1834, 1836), c(1839, 1841), c(1847, 1850), 
                            c(1861, 1864), c(1883, 1887), c(1916, 1920), 
                            c(2100, 2102), c(2105, 2107), c(2113, 2116), 
                            c(2127, 2130), c(2149, 2153), c(2181, 2185), 
                            c(2364, 2366), c(2369, 2371), c(2377, 2380), 
                            c(2391, 2394), c(2413, 2417), c(2444, 2448),
                            c(2629, 2631), c(2634, 2636), c(2642, 2645), 
                            c(2656, 2659), c(2677, 2681), c(2708, 2712), 
                            c(2889, 2891), c(2894, 2896), c(2902, 2905), 
                            c(2916, 2919), c(2937, 2941), c(2968, 2972),
                            c(3148, 3150), c(3153, 3155), c(3161, 3164), 
                            c(3175, 3178), c(3195, 3199), c(3226, 3230))

crime1974_variables <- list(c(2, 3), c(46, 52), c(53, 55), 
                            c(277, 279), c(282, 284), c(290, 293), 
                            c(309, 312), c(330, 334), c(365, 369), 
                            c(559, 561), c(564, 566), c(572, 575), 
                            c(591, 594), c(611, 615), c(645, 649),
                            c(834, 836), c(839, 841), c(847, 850), 
                            c(866, 869), c(887, 891), c(922, 926), 
                            c(1117, 1119), c(1122, 1124), c(1130, 1133), 
                            c(1149, 1152), c(1170, 1174), c(1205, 1209), 
                            c(1400, 1402), c(1405, 1407), c(1413, 1416), 
                            c(1432, 1435), c(1454, 1458), c(1490, 1494),
                            c(1684, 1686), c(1689, 1691), c(1697, 1700), 
                            c(1717, 1720), c(1739, 1743), c(1774, 1778), 
                            c(1962, 1964), c(1967, 1969), c(1975, 1978), 
                            c(1995, 1998), c(2017, 2021), c(2054, 2058),
                            c(2246, 2248), c(2251, 2253), c(2259, 2262), 
                            c(2279, 2282), c(2301, 2305), c(2338, 2342), 
                            c(2525, 2527), c(2530, 2532), c(2538, 2541), 
                            c(2557, 2560), c(2579, 2583), c(2615, 2619),
                            c(2799, 2801), c(2804, 2806), c(2812, 2815), 
                            c(2832, 2835), c(2854, 2858), c(2891, 2895),
                            c(3088, 3090), c(3093, 3095), c(3101, 3104), 
                            c(3120, 3123), c(3141, 3145), c(3177, 3181),
                            c(3371, 3373), c(3376, 3378), c(3384, 3387), 
                            c(3404, 3407), c(3426, 3430), c(3463, 3467))

crime1970 <- read_offense_data(crime1970_variables, "1970", "04198")
crime1970_totals <- total_offense_data(crime1970, 1970)
crime1971 <- read_offense_data(crime1971_variables, "1971", "04199")
crime1971_totals <- total_offense_data(crime1971, 1971)
crime1972 <- read_offense_data(crime1972_variables, "1972", "04200")
crime1972_totals <- total_offense_data(crime1972, 1972)
crime1973 <- read_offense_data(crime1973_variables, "1973", "04201")
crime1973_totals <- total_offense_data(crime1973, 1973)
crime1974 <- read_offense_data(crime1974_variables, "1974", "04202")
crime1974_totals <- total_offense_data(crime1974, 1974)

crime_1970_1974 <- rbind(
  crime1970_totals, crime1971_totals, 
  crime1972_totals, crime1973_totals, 
  crime1974_totals
)

# convert counties --------------------------------------------------------

ucr_county_pop <- ucr_county %>% 
  group_by(USTATENO, UCOUNTY) %>% 
  summarize(population = sum(`1970`)) %>% 
  ungroup()

ucr_fips_county_1970 <- ucr_county %>% 
  left_join(ucr_county_pop,
            by = c("USTATENO", "UCOUNTY")) %>% 
  mutate(weight = `1970` / population,
         USTATENO = as.numeric(USTATENO),
         UCOUNTY = as.numeric(UCOUNTY))

crime1970_1974_county <- 
  crime_1970_1974 %>% 
  mutate(county1 = ifelse(is.na(county1), 0, county1),
         county1 = ifelse(state == 31 & county1 == 0, 24, county1)) %>% 
  left_join(ucr_fips_county_1970,
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
  filter(!is.na(murder))

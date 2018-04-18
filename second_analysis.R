## second_analysis
## thesis
##
## andrew wolff
## 12 april 2018


# initial settings --------------------------------------------------------

rm(list = ls())
setwd("~/Documents/thesis")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(estimatr))
suppressPackageStartupMessages(library(clubSandwich))
options(stringsAsFactors = FALSE)
source("functions/county_populations.R")
source("crime/crime_data1970_1974.R")
source("crime/crime_data1975_1976.R")
source("crime/crime_data1977_1983.R")
source("crime/crime_data84_86_89_90.R")
source("crime/crime_data85_87_88.R")


# crime data 1970 to 1990 -------------------------------------------------

crime_county <- rbind(
  crime1970_1974_county,
  crime1975_1976_county,
  crime1977_1983_county,
  crime84_86_89_90_county,
  crime85_87_88_county
) %>% 
  mutate(murder = ifelse(murder < 0, 0, murder),
         robbery = ifelse(robbery < 0, 0, robbery),
         rape = ifelse(rape < 0, 0, rape),
         burglary = ifelse(burglary < 0, 0, burglary),
         assault = ifelse(assault < 0, 0, assault),
         all = ifelse(all < 0, 0, all)) %>% 
  filter(murder > 0 | robbery > 0 | rape > 0 | burglary > 0 | assault > 0 | all > 0)

# indochinese migration ---------------------------------------------------

indochina <- c(51300, 51100, 51800, 51910)

countygrps_1980 <- read_xls("data/cg98stat.xls", na = ".") %>% 
  rename(STATEFIP = `State FIPS Code`,
         COUNTYFIP = `County FIPS code`,
         countygrp = `1980 County Group`,
         county = `County Name`,
         countygrp_pop = `County Group Population`,
         county_pop = `County Population`) %>% 
  filter(!is.na(COUNTYFIP)) %>% 
  mutate(weight = county_pop / countygrp_pop,
         FIPS = STATEFIP * 1000 + COUNTYFIP,
         county = tolower(county)) %>% 
  select(FIPS,
         STATEFIP,
         COUNTYFIP,
         countygrp,
         county,
         weight)

county_treatment <- 
  read_csv("data/usa_00072.csv") %>% 
  mutate(indochinese_refugee = ifelse(BPLD %in% indochina & YRIMMIG == 1980, 1, 0),
         immigrant = ifelse(BPL > 120, 1, 0)) %>% 
  rename(countygrp = CNTYGP98) %>% 
  left_join(countygrps_1980,
            by = c("countygrp",
                   "STATEFIP")) %>% 
  mutate(person_weight = PERWT * weight) %>% 
  filter(!is.na(person_weight)) %>% 
  group_by(STATEFIP, COUNTYFIP) %>% 
  summarize(indochinese_pop = sum(indochinese_refugee * person_weight),
            ct_foreign_born = sum(immigrant * person_weight)) %>% 
  ungroup() %>% 
  mutate(pct_foreign_born = indochinese_pop / ct_foreign_born,
         treated = ifelse(pct_foreign_born >= .10 & ct_foreign_born >= 100, 1, 0),
         treated_nl = ifelse(pct_foreign_born >= .10, 1, 0),
         treated_k = ifelse(pct_foreign_born >= .10 & ct_foreign_born >= 1000, 1, 0),
         treated_20pct = ifelse(pct_foreign_born >= .20 & ct_foreign_born >= 100, 1, 0),
         treated_5pct = ifelse(pct_foreign_born >= .05 & ct_foreign_born >= 100, 1, 0),
         treated_1pct = ifelse(pct_foreign_born >= 0.01 & ct_foreign_born >= 100, 1, 0),
         FIPS = STATEFIP * 1000 + COUNTYFIP)


# merge into one data frame -----------------------------------------------

county_populations_gather <- county_populations %>% 
  gather(year, population, `1990`:`1960`) %>% 
  mutate(year = as.numeric(year))

county_data <- 
  left_join(crime_county, county_treatment, by = c("FIPS")) %>%
  mutate(after_1980 = ifelse(year %in% c(1981: 1990), 1, 0),
         pop_year = ifelse(year %in% c(1970:1979), 1970, 
                           ifelse(year %in% c(1980:1989), 1980,
                                  1990))) %>% 
  left_join(county_populations_gather, 
            by = c("FIPS", "pop_year" = "year")) %>% 
  filter(!is.na(murder) & !is.na(treated) & FIPS != 51570) %>% 
  mutate(murder_rate = murder / population * 1e5,
         ln_murder_rate = log(murder_rate + 1),
         robbery_rate = robbery / population * 1e5,
         ln_robbery_rate = log(robbery + 1),
         crime_rate = all / population * 1e5,
         ln_crime_rate = log(crime_rate + 1),
         ln_population = log(population + 1)) %>% 
  filter(!is.na(murder_rate) & !is.na(robbery_rate) & !is.na(crime_rate))

write.csv(county_data, "data/second_analysis.csv", row.names = FALSE)

# murder graph ------------------------------------------------------------

murder_rates <- county_data %>% 
  group_by(year, treated) %>% 
  summarize(murder = sum(murder),
            population = sum(population)) %>% 
  ungroup() %>% 
  mutate(murder_rate = murder / population * 1e5,
         treated = factor(treated, labels = c("Untreated", "Treated")))

ggplot(murder_rates, aes(x = year, y = murder_rate, color = treated)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1975, color = "grey") +
  geom_vline(xintercept = 1980, color = "grey") +
  scale_y_continuous(limits = c(0,12)) +
  labs(x = "Year",
       y = "Murder Rate",
       color = "") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("figures/murder_rates.png", width = 8, height = 4)


# robbery graph -----------------------------------------------------------

robbery_rates <- county_data %>% 
  group_by(year, treated) %>% 
  summarize(robberies = sum(robbery),
            population = sum(population)) %>% 
  ungroup() %>% 
  mutate(treated = factor(treated, labels = c("Untreated", "Treated")),
         robbery_rate = robberies / population * 1e5)

ggplot(robbery_rates, aes(x = year, y = robbery_rate, color = treated)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1975, color = "grey") +
  geom_vline(xintercept = 1980, color = "grey") +
  scale_y_continuous(limits = c(0,300)) +
  labs(x = "Year",
       y = "Robbery Rate",
       color = "") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("figures/robbery_rates.png", width = 8, height = 4)


# overall crime graph -----------------------------------------------------

crime_rates <- county_data %>% 
  group_by(year, treated) %>% 
  summarize(crime = sum(all),
            population = sum(population)) %>% 
  ungroup() %>% 
  mutate(treated = factor(treated, labels = c("Untreated", "Treated")),
         crime_rate = crime / population *1e5)

ggplot(crime_rates, aes(x = year, y = crime_rate, color = treated)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1975, color = "grey") +
  geom_vline(xintercept = 1980, color = "grey") +
  scale_y_continuous(limits = c(0, 7000)) +
  labs(x = "Year",
       y = "Overall Crime Rate",
       color = "") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("figures/crime_rates.png", width = 8, height = 4)

# make treatment effect graphics ------------------------------------------

murder_graph <- data.frame(
  "time" = c(rep("Before 1980", 2), rep("After 1980", 2)),
  "group" = c("Untreated", "Treated", "Untreated", "Treated"),
  "treatment" = c(1.91, 1.70, 1.86, 1.67),
  "lower" = c(1.79, 1.40, 1.75, 1.37),
  "higher" = c(2.03, 2.00, 1.99, 1.97)
) %>% 
  mutate(group = factor(group, levels = c("Untreated", "Treated")),
         time = factor(time, levels = c("Before 1980", "After 1980")))

murder_counter <- data.frame(
  "time" = c("Before 1980", "After 1980"),
  "estimate" = c(1.70, 1.66)
) %>% 
  mutate(time = factor(time, levels = c("Before 1980", "After 1980")))

ggplot() +
  geom_pointrange(data = murder_graph, 
                  aes(y = treatment, 
                      x = time, 
                      color = group, 
                      ymin = lower, 
                      ymax = higher)) +
  geom_line(data = murder_graph, aes(y = treatment,
                                     x = time, 
                                     color = group,
                                     group = group)) +
  geom_line(data = murder_counter, 
            aes(x = time, y = estimate, group = 1),
            linetype = "dotted") +
  scale_y_continuous(limits = c(1,2.1), breaks = c(1, 1.2, 1.4, 1.6, 1.8, 2.0)) +
  labs(x = NULL,
       y = "Average log(Murder Rate)",
       color = "") +
  theme_bw()
ggsave("figures/murder_dif.png", width = 8, height = 4)

robbery_graph <- data.frame(
  "time" = c(rep("Before 1980", 2), rep("After 1980", 2)),
  "group" = c("Untreated", "Treated", "Untreated", "Treated"),
  "treatment" = c(5.49, 4.18, 5.64, 4.41),
  "lower" = c(4.99, 3.31, 5.16, 3.60),
  "higher" = c(6.00, 5.06, 6.13, 5.24)
) %>% 
  mutate(group = factor(group, levels = c("Untreated", "Treated")),
         time = factor(time, levels = c("Before 1980", "After 1980")))

robbery_counter <- data.frame(
  "time" = c("Before 1980", "After 1980"),
  "estimate" = c(4.18, 4.34)
) %>% 
  mutate(time = factor(time, levels = c("Before 1980", "After 1980")))

ggplot() +
  geom_pointrange(data = robbery_graph, 
                  aes(y = treatment, 
                      x = time, 
                      color = group, 
                      ymin = lower, 
                      ymax = higher)) +
  geom_line(data = robbery_graph, aes(y = treatment,
                                     x = time, 
                                     color = group,
                                     group = group)) +
  geom_line(data = robbery_counter, 
            aes(x = time, y = estimate, group = 1),
            linetype = "dotted") +
  scale_y_continuous(limits = c(3, 6.2), breaks = c(3, 3.5, 4, 4.5, 5, 5.5, 6)) +
  labs(x = NULL,
       y = "Average log(Robbery Rate)",
       color = "") +
  theme_bw()
ggsave("figures/robbery_dif.png", width = 8, height = 4)

crime_graph <- data.frame(
  "time" = c(rep("Before 1980", 2), rep("After 1980", 2)),
  "group" = c("Untreated", "Treated", "Untreated", "Treated"),
  "treatment" = c(8.26, 8.08, 8.45, 8.34),
  "lower" = c(8.18, 7.82, 8.38, 8.11),
  "higher" = c(8.33, 8.34, 8.52, 8.58)
) %>% 
  mutate(group = factor(group, levels = c("Untreated", "Treated")),
         time = factor(time, levels = c("Before 1980", "After 1980")))

crime_counter <- data.frame(
  "time" = c("Before 1980", "After 1980"),
  "estimate" = c(8.08, 8.27)
) %>% 
  mutate(time = factor(time, levels = c("Before 1980", "After 1980")))

ggplot() +
  geom_pointrange(data = crime_graph, 
                  aes(y = treatment, 
                      x = time, 
                      color = group, 
                      ymin = lower, 
                      ymax = higher)) +
  geom_line(data = crime_graph, aes(y = treatment,
                                      x = time, 
                                      color = group,
                                      group = group)) +
  geom_line(data = crime_counter, 
            aes(x = time, y = estimate, group = 1),
            linetype = "dotted") +
  scale_y_continuous(limits = c(7, 9), breaks = c(7, 7.5, 8, 8.5, 9)) +
  labs(x = NULL,
       y = "Average log(All Crime Rate)",
       color = "") +
  theme_bw()
ggsave("figures/crime_dif.png", width = 8, height = 4)


# southeast asian refugees 1970 -------------------------------------------

census_1970 <- read_csv("data/usa_00079.csv")

southeast_asia <- c(51100, 51300, 51800, 51910)

southeast_asian_pop <- 
  census_1970 %>% 
  filter(BPLD %in% southeast_asia) %>% 
  group_by(YEAR, YRIMMIG) %>% 
  summarize(population = sum(PERWT)) %>% 
  ungroup()


# difference-in-differences -----------------------------------------------

lm_murder <- lm(ln_murder_rate ~ factor(treated) * factor(after_1980),
                data = county_data,
                weights = population)

lm_murder_crve <- 
coef_test(lm_murder, 
          vcov = "CR2",
          cluster = county_data$FIPS,
          test = "Satterthwaite")

lm_robbery <- lm(ln_robbery_rate ~ factor(treated) * factor(after_1980),
                 data = county_data,
                 weights = population)

lm_robbery_crve <- 
coef_test(lm_robbery, 
          vcov = "CR2",
          cluster = county_data$FIPS,
          test = "Satterthwaite")

lm_crime <- lm(ln_crime_rate ~ factor(treated) * factor(after_1980),
                 data = county_data,
                 weights = population)

lm_crime_crve <- 
coef_test(lm_crime, 
          vcov = "CR2",
          cluster = county_data$FIPS,
          test = "Satterthwaite")


# using estimatr ----------------------------------------------------------






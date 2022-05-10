
# data exploration & table 1
# epi 514
# bridget waters
# created: may 1 2022
# last updated: may 9 2022

# description: data exploration, crosstabs, histograms

# components
  # explore cleaned and reweighted brfss data
  # create composite ace score variable
  # create household income adjustment based on number in household 
    # see https://www.census.gov/topics/income-poverty/income-inequality/about/metrics/equivalence.html
      # https://www.pewresearch.org/social-trends/2011/10/03/appendix-b-adjusting-household-income-for-household-size/
  # create table 1

# packages in addition to those used in part2data
library(survey)
library(magrittr)

# read in cleaned and reweighted data by running part2data
  # running this version preserves the variables as factors rather than losing this if importing from csv

# data exploration
summary(brfss)

# crosstabs of new and old variables
table(brfss$old_decide, brfss$decide, useNA = "always")
table(brfss$old_ace_depres, brfss$ace_depres, useNA = "always")
table(brfss$old_ace_drink, brfss$ace_drink, useNA = "always")
table(brfss$old_ace_drugs, brfss$ace_drugs, useNA = "always")
table(brfss$old_ace_prison, brfss$ace_prison, useNA = "always")
table(brfss$old_ace_divor, brfss$ace_divor, useNA = "always")
table(brfss$old_ace_punch, brfss$ace_punch, useNA = "always")
table(brfss$old_ace_hurt, brfss$ace_hurt, useNA = "always")
table(brfss$old_ace_swear, brfss$ace_swear, useNA = "always")
table(brfss$old_ace_touch_receive, brfss$ace_touch_receive, useNA = "always")
table(brfss$old_ace_touch_give, brfss$ace_touch_give, useNA = "always")
table(brfss$old_ace_sex, brfss$ace_sex, useNA = "always")
table(brfss$old_sex, brfss$sex, useNA = "always")
table(brfss$old_income, brfss$income, useNA = "always")
table(brfss$old_age_5yr_group, brfss$age_5yr_group, useNA = "always")
table(brfss$old_age_65_plus, brfss$age_65_plus, useNA = "always")
table(brfss$old_race_eth, brfss$race_eth, useNA = "always")
table(brfss$old_education, brfss$education, useNA = "always")
table(brfss$old_employ, brfss$employ, useNA = "always")
table(brfss$old_ment_health, brfss$ment_health, useNA = "always")
table(brfss$old_health_plan, brfss$health_plan, useNA = "always")
table(brfss$age_10yr_group, brfss$age_5yr_group, useNA = "always")
table(brfss$old_children, brfss$children, useNA = "always")
table(brfss$old_adults_cell, brfss$adults_cell, useNA = "always")
table(brfss$adults_LL, brfss$adults_LL, useNA = "always")
table(brfss$men_LL, brfss$men_LL, useNA = "always")
table(brfss$women_LL, brfss$women_LL, useNA = "always")
table(brfss$old_marital, brfss$marital, useNA = "always")

# cleaning up what are likely brfss typos
# 81 was likely entered incorrectly (unlikely that this household has 81 children, so changed to NA)
brfss$children <- recode(brfss$children, `81` = NA_integer_)
table(brfss$old_children, brfss$children, useNA = "always")
# when the number of adults in the household is missing but there are zero values (non-missing) for number of adult men and adult women
brfss <- brfss %>%
  mutate(
    men_LL = case_when(
      is.na(adults_LL) & is.na(adults_cell) & !is.na(men_LL) | !is.na(women_LL) ~ NA_integer_,
      TRUE ~ men_LL
    ),
    women_LL = case_when(
      is.na(adults_LL) & is.na(adults_cell) & !is.na(men_LL) | !is.na(women_LL) ~ NA_integer_,
      TRUE ~ women_LL
    )
  )

# create composite ACE score
  # NAs for participants who are missing responses to all ace questions
brfss <- brfss %>% 
  mutate(
    ace_score_com = select(., starts_with("ace")) %>% rowSums(na.rm = FALSE)
  ) %>%
# create ace household challenges score
  mutate(
    ace_score_house = select(., c("ace_depres", "ace_drink", "ace_drugs", "ace_prison", "ace_divor", "ace_punch")) %>% 
    rowSums(na.rm = FALSE)
  ) %>%
# create ace physical abuse score and verbal abuse score
  mutate(
    ace_score_phys = ace_hurt, 
    ace_score_verb = ace_swear
  ) %>%
# create ace sexual abuse score
  mutate(
    ace_score_sex = case_when(
      (ace_touch_receive == 0 & ace_touch_give == 0 & ace_sex == 0) ~ 0,
      (ace_touch_receive == 1 | ace_touch_give == 1 | ace_sex == 1) ~ 1,
      (is.na(ace_touch_receive) | is.na(ace_touch_give) | is.na(ace_sex)) ~ as.numeric(NA)
    )
  )

# explore new ace score variables, histograms
summary(brfss)
hist(brfss$ace_score_com)
hist(brfss$ace_score_house)
hist(brfss$ace_score_phys)
hist(brfss$ace_score_verb)
hist(brfss$ace_score_sex)

# create categories for composite ace score
brfss <- brfss %>%
  mutate(
    ace_score_com_cat = case_when(
      is.na(ace_score_com) ~ as.character(NA),
      ace_score_com >= 4 ~ "4+",
      TRUE ~ as.character(ace_score_com)
    ),
  # create categories for household ace score
    ace_score_house_cat = case_when(
      is.na(ace_score_house) ~ as.character(NA), 
      ace_score_house >= 3 ~ "3+", 
      TRUE ~ as.character(ace_score_house)
    )
  )

# convert all ace variables to factors
brfss$ace_depres <- factor(brfss$ace_depres, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_drink <- factor(brfss$ace_drink, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_drugs <- factor(brfss$ace_drugs, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_prison <- factor(brfss$ace_prison, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_divor <- factor(brfss$ace_divor, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_punch <- factor(brfss$ace_punch, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_hurt <- factor(brfss$ace_hurt, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_swear <- factor(brfss$ace_swear, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_touch_receive <- factor(brfss$ace_touch_receive, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_touch_give <- factor(brfss$ace_touch_give, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_sex <- factor(brfss$ace_sex, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_score_com_cat <- as.factor(brfss$ace_score_com_cat)
brfss$ace_score_house_cat <- as.factor(brfss$ace_score_house_cat)
brfss$ace_score_phys_cat <- factor(brfss$ace_score_phys, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_score_verb_cat <- factor(brfss$ace_score_verb, levels = 0:1, labels = c("No", "Yes"))
brfss$ace_score_sex_cat <- factor(brfss$ace_score_sex, levels = 0:1, labels = c("No", "Yes"))

summary(brfss)

# calculate adjusted household income (based on number of people in house)
brfss$old_income2 <- brfss$income
# recode income variable to approximation for each income group (median of range except for bottom and top levels)
brfss$old_income2 <- as.numeric(brfss$old_income2)
brfss$old_income2 <- recode(brfss$old_income2, `1` = 10000L, `2` = 12500L, `3` = 17500L, `4` = 22500L, `5` = 30000L, `6` = 42500L, `7` = 62500L, `8` = 75000L)
# calculations
brfss <- brfss %>% mutate(
  # number of adults in household
  adults = case_when(
    is.na(adults_cell) & !is.na(adults_LL) & adults_LL != 0 ~ as.numeric(adults_LL),
    !is.na(adults_cell) & is.na(adults_LL) & adults_cell != 0 ~ as.numeric(adults_cell),
    is.na(adults_cell) & is.na(adults_LL) ~ as.numeric(men_LL + women_LL),
    is.na(adults_cell) & is.na(adults_LL) & is.na(men_LL) & is.na(women_LL) ~ as.numeric(marital), 
    TRUE ~ NA_real_
  ),
  # income adjustment
  income_adj = case_when(
    is.na(children) | children == 0 & adults >= 1 ~ adults^0.5,
    children == 1 & adults == 1 ~ 1.8^0.7,
    children > 1 & adults == 1 ~ (adults + 0.8 + (0.5 * (children - 1)))^0.7,
    !is.na(children) & children != 0 & adults > 1 ~ (adults + (0.5 * children))^0.7, 
    TRUE ~ NA_real_
  ),
  # new income rescaled to if household had 2 adults and 2 children
  income2 =  (old_income2 / income_adj) * (2 + (0.5 * 2))^0.7, 
  income = case_when(
    income2 < 20000 ~ 1, 
    income2 >= 20000 & income2 < 35000 ~ 2,
    income2 >= 35000 & income2 < 50000 ~ 3,
    income2 >= 50000 & income2 < 75000 ~ 4, 
    income2 >= 75000 & income2 < 100000 ~ 5,
    income2 >= 100000 ~ 6, 
    TRUE ~ NA_real_
  )
)

# convert new income variable to factor
brfss$income <- factor(brfss$income, levels = 1:6, labels = c("<$20K", "$20-35K", "$35-50K", "$50-75K", "$75-100K", "$100K+"))

# create table 1
# calculate weighted prevalences

# set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

# assign weights
design <- svydesign(data = brfss, 
                    id = ~1, 
                    strata = ~strat, 
                    weights = ~svy_weight)

# calculate weighted prevalence values by ace score

sex <- data.frame(svytable(~ace_score_com_cat + sex, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = sex)
age <- data.frame(svytable(~ace_score_com_cat + age_10yr_group, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = age_10yr_group)
race_eth <- data.frame(svytable(~ace_score_com_cat + race_eth, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = race_eth)
income <- data.frame(svytable(~ace_score_com_cat + income, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = income)
education <- data.frame(svytable(~ace_score_com_cat + education, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = education)
employ <- data.frame(svytable(~ace_score_com_cat + employ, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = employ)
ment_health <- data.frame(svytable(~ace_score_com_cat + ment_health, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = ment_health)
health_plan <- data.frame(svytable(~ace_score_com_cat + health_plan, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = health_plan)

# make table 1a
table1a <- rbind(sex, age, race_eth, income, education, employ, ment_health, health_plan)
# round prevalence and make percent
table1a <- table1a %>% mutate(pct = round(Freq * 100, 1)) %>% 
  select(-Freq) %>%
# make the table wide
  pivot_wider(names_from = ace_score_com_cat, values_from = pct)

# calculate total weighted percents for each covariate
# couldn't figure out how to write a function for this when using svytable

sex_tot <- data.frame(svytable(~sex, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = sex) %>%
  select(covariate, Total)

age_tot <- data.frame(svytable(~age_10yr_group, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = age_10yr_group) %>%
  select(covariate, Total)

race_eth_tot <- data.frame(svytable(~race_eth, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = race_eth) %>%
  select(covariate, Total)

income_tot <- data.frame(svytable(~income, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = income) %>%
  select(covariate, Total)

education_tot <- data.frame(svytable(~education, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = education) %>%
  select(covariate, Total)

employ_tot <- data.frame(svytable(~employ, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = employ) %>%
  select(covariate, Total)

ment_health_tot <- data.frame(svytable(~ment_health, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = ment_health) %>%
  select(covariate, Total)

health_plan_tot <- data.frame(svytable(~health_plan, design)) %>% 
  mutate(
    Total = round((Freq / sum(Freq)) * 100, 1)
  ) %>%
  rename(covariate = health_plan) %>%
  select(covariate, Total)

# create the total column by binding all of the weighted percents together
total <- rbind(sex_tot, age_tot, race_eth_tot, income_tot, education_tot, employ_tot, ment_health_tot, health_plan_tot) %>%
  rename(covariate1 = covariate)

# bind with table1a
table1b <- cbind(table1a, total) %>% select(-covariate1)

# create data frame that excludes participants with missing outcome and primary exposure (composite ace score)
brfss_nonNA <- brfss %>% filter(!is.na(decide) & !is.na(ace_score_com_cat))

# write a function to compute prevalence for missing values (not weighted)
missing <- function(x){
  brfss_nonNA %>% 
  select({{x}}, ace_score_com_cat) %>% 
  group_by({{x}}, ace_score_com_cat) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(ace_score_com_cat) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = round((n / sum) * 100, 1)) %>%
  select(-n, -sum) %>%
  pivot_wider(names_from = ace_score_com_cat, values_from = prop) %>%
  tail(1) %>%
  rename(covariate = {{x}})
}

# run the function on all the covariates
NA_age <- missing(age_10yr_group)
NA_race_eth <- missing(race_eth)
NA_income <- missing(income)
NA_education <- missing(education)
NA_employ <- missing(employ)
NA_ment_health <- missing(ment_health)
NA_health_plan <- missing(health_plan)

# make table 1c
table1c <- rbind(NA_age, NA_race_eth, NA_income, NA_education, NA_employ, NA_ment_health, NA_health_plan)
table1c$covariate <- as.character(table1c$covariate)
table1c[1, 1] <- "Missing age group"
table1c[2, 1] <- "Missing race/ethnicity"
table1c[3, 1] <- "Missing income"
table1c[4, 1] <- "Missing education"
table1c[5, 1] <- "Missing employment"
table1c[6, 1] <- "Missing mental health"
table1c[7, 1] <- "Missing health insurance"

# create a total column for missing values
missing_tot <- function(x){
  brfss_nonNA %>% 
    select({{x}}) %>% 
    group_by({{x}}) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(sum = sum(n)) %>%
    mutate(prop = round((n / sum) * 100, 1)) %>%
    select(-n, -sum) %>%
    tail(1) %>%
    rename(covariate = {{x}})
}

# run on all covariates except sex which has no missings
NA_age_tot <- missing_tot(age_10yr_group)
NA_race_eth_tot <- missing_tot(race_eth)
NA_income_tot <- missing_tot(income)
NA_education_tot <- missing_tot(education)
NA_employ_tot <- missing_tot(employ)
NA_ment_health_tot <- missing_tot(ment_health)
NA_health_plan_tot <- missing_tot(health_plan)

# bind total missings together to create total column
table1d <- rbind(NA_age_tot, NA_race_eth_tot, NA_income_tot, NA_education_tot, NA_employ_tot, NA_ment_health_tot, NA_health_plan_tot) %>%
  select(-covariate)

# bind table1c and table1d to add total column
table1e <- cbind(table1c, table1d) %>% rename(Total = prop)

# bind table1a and table1b
table1 <- rbind(table1b, table1e)
# order the rows 
table1 <- table1 %>% 
  slice(1:8, 34, 9:16, 35, 17:22, 36, 23:26, 37, 27:28, 38, 29:31, 39, 32:33, 40)
# write to csv
write.csv(table1, "table1.csv", row.names = FALSE)

# get Ns for each composite ace group using data with only non-missing exposure and outcome
table(brfss_nonNA$ace_score_com_cat, brfss_nonNA$ace_score_com_cat)

# get total N
count(brfss_nonNA)

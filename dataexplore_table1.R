
# data exploration & table 1
# epi 514
# bridget waters
# created: may 1 2022
# last updated: may 1 2022

# description: data exploration, crosstabs, histograms
  # make table 1 using table1 package

# components
  # explore cleaned and reweighted brfss data
  # create composite ace score variable
  # table 1

# packages
library(survey)
library(haven)
library(tidyverse)

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

# create composite ACE score
  # NAs for participants who are missing responses to all ace questions
brfss <- brfss %>% 
  mutate(
    ace_score_com = select(., starts_with("ace")) %>% rowSums(na.rm = TRUE)
  ) %>%
   mutate(
    ace_score_com = case_when(
      (is.na(ace_depres) & 
      is.na(ace_drink) &
      is.na(ace_drugs) &
      is.na(ace_prison) &
      is.na(ace_divor) & 
      is.na(ace_punch) & 
      is.na(ace_hurt) & 
      is.na(ace_swear) &
      is.na(ace_touch_receive) & 
      is.na(ace_touch_give) & 
      is.na(ace_sex)) ~ NA_real_,
      TRUE ~ as.numeric(ace_score_com)
      )
   )
  
# create ace household challenges score
brfss <- brfss %>% 
  mutate(
    ace_score_house = select(., c("ace_depres", "ace_drink", "ace_drugs", "ace_prison", "ace_divor", "ace_punch")) %>% 
    rowSums(na.rm = TRUE)
  ) %>%
  mutate(
    ace_score_house = case_when(
      (is.na(ace_depres) & 
        is.na(ace_drink) &
        is.na(ace_drugs) &
        is.na(ace_prison) &
        is.na(ace_divor) & 
        is.na(ace_punch)) ~ NA_real_,
        TRUE ~ as.numeric(ace_score_house)
    )
  )

# create ace physical abuse score and verbal abuse score
brfss <- brfss %>% 
  mutate(
    ace_score_phys = ace_hurt, 
    ace_score_verb = ace_swear
  )

# create ace sexual abuse score
brfss <- brfss %>% 
  mutate(
    ace_score_sex = case_when(
      (ace_touch_receive == 0 & ace_touch_give == 0 & ace_sex == 0) ~ 0,
      (ace_touch_receive == 1 | ace_touch_give == 1 | ace_sex == 1) ~ 1,
      (is.na(ace_touch_receive) & is.na(ace_touch_give) & is.na(ace_sex)) ~ NA_real_
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

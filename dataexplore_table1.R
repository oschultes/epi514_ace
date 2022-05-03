
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
library(tidyverse)
library(survey)

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
      ace_score_com>= 4 ~ "4+",
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

# create table 1
# calculate weighted prevalences

# set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

# assign weights
design <- svydesign(data = brfss, 
                    id = ~1, 
                    strata = ~strat, 
                    weights = ~svy_weight)

# calculate weighted prevalence values
sex <- data.frame(svytable(~ace_score_com_cat + sex, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = sex)
age <- data.frame(svytable(~ace_score_com_cat + age_65_plus, design) %>% 
  prop.table(margin = 1)) %>% 
  rename(covariate = age_65_plus)
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

# compute prevalence for missing values (not weighted)
NA_age <- data.frame(table(brfss$age_65_plus, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[3, -1]/.[4, -1])*100, 1)) %>% 
  tail(1)
NA_race_eth <- data.frame(table(brfss$race_eth, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[9, -1]/.[10, -1])*100, 1)) %>% 
  tail(1)
NA_income <- data.frame(table(brfss$income, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[3, -1]/.[4, -1])*100, 1)) %>% 
  tail(1)
NA_education <- data.frame(table(brfss$education, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[3, -1]/.[4, -1])*100, 1)) %>% 
  tail(1)
NA_employ <- data.frame(table(brfss$employ, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[3, -1]/.[4, -1])*100, 1)) %>% 
  tail(1)
NA_ment_health <- data.frame(table(brfss$ment_health, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[3, -1]/.[4, -1])*100, 1)) %>% 
  tail(1)
NA_health_plan <- data.frame(table(brfss$health_plan, brfss$ace_score_com_cat, useNA = "always")) %>% 
  drop_na(Var2) %>%
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  add_row(round((.[3, -1]/.[4, -1])*100, 1)) %>% 
  tail(1)

# make table 1b
table1b <- rbind(NA_age, NA_race_eth, NA_income, NA_education, NA_employ, NA_ment_health, NA_health_plan)
table1b$Var1 <- as.character(table1b$Var1)
table1b[1, 1] <- "Missing age group"
table1b[2, 1] <- "Missing race/ethnicity"
table1b[3, 1] <- "Missing income"
table1b[4, 1] <- "Missing education"
table1b[5, 1] <- "Missing employment"
table1b[6, 1] <- "Missing mental health"
table1b[7, 1] <- "Missing health insurance"
table1b <- table1b %>% rename(covariate = Var1)

# bind table1a and table1b
table1 <- rbind(table1a, table1b)
# order the rows 
table1 <- table1 %>% 
  slice(1:4, 23, 5:12, 24, 13:14, 25, 15:16, 26, 17:18, 27, 19:20, 28, 21:22, 29)
#write to csv
write.csv(table1, "table1.csv")

#get Ns for each composite ace group
table(brfss$ace_score_com_cat, brfss$ace_score_com_cat)


# results & table 2
# epi 514
# bridget waters
# created: may 9 2022
# last updated: may 9 2022

# description: Mantel-Haenszel stratified analyses and tables for results

# be sure to run part2data.R and dataexplore_table1.R prior to running this R script

# components
  # consolidate and recode variables in preparation for M-H analyses
  # calculate weighted prevalence of outcome for all exposure groups and for all covariates
  # Mantel-Haenszel stratified analyses to calculate prevalence ratios
  
# packages in addition to those loaded in part2data and dataexplore_table1
library(gdata)
library(epiR)
# clear out environment except for data
keep(brfss, brfss_nonNA, sure = TRUE)
detach("package:gdata", unload = TRUE)

# create a new race variable that combines race/ethnicity groups with small sample sizes
  # combines American Indian or Alaskan Native only,non-Hispanic with Other race only, non-Hispanic and Multiracial, non-Hispanic
  # combines Asian only, non-Hispanic with Native Hawaiian or Other Pacific Islander only, non-Hispanic
brfss$race_eth_mh <- as.numeric(brfss$race_eth)
brfss$race_eth_mh <- recode(brfss$race_eth_mh, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 4L, `6` = 3L, `7` = 3L, `8` = 5L)
brfss$race_eth_mh <- factor(brfss$race_eth_mh, levels = 1:5, labels = c("White only, non-Hispanic",
                                                                  "Black only, non-Hispanic",
                                                                  "Other, non-Hispanic", 
                                                                  "Asian or Pacific Islander, non-Hispanic", 
                                                                  "Hispanic"))
summary(brfss$race_eth_mh)
table(brfss$race_eth, brfss$race_eth_mh)

# create a 1/2 version of decide variable (outcome)
brfss <- brfss %>% mutate(
  decide_12 = case_when(
    as.numeric(decide) == 1 ~ 2, 
    as.numeric(decide) == 2 ~ 1, 
    TRUE ~ NA_real_
  )
)
brfss$decide_12 <- factor(brfss$decide_12, levels = 1:2, labels = c("Yes", "No"))
table(brfss$decide, brfss$decide_12)

# create indicator exposure variables for bivariate comparisons
# composite ace score
  # reference group for composite ace score is 0
brfss$ace_score_01 <- factor(brfss$ace_score_com_cat, levels = levels(brfss$ace_score_com_cat)[2:1])
brfss$ace_score_02 <- factor(brfss$ace_score_com_cat, levels = levels(brfss$ace_score_com_cat)[c(3,1)])
brfss$ace_score_03 <- factor(brfss$ace_score_com_cat, levels = levels(brfss$ace_score_com_cat)[c(4,1)])
brfss$ace_score_04 <- factor(brfss$ace_score_com_cat, levels = levels(brfss$ace_score_com_cat)[c(5,1)])

# check with crosstabs
table(brfss$ace_score_com_cat, brfss$ace_score_01)
table(brfss$ace_score_com_cat, brfss$ace_score_02)
table(brfss$ace_score_com_cat, brfss$ace_score_03)
table(brfss$ace_score_com_cat, brfss$ace_score_04)

# household ace score
brfss$ace_house_01 <- factor(brfss$ace_score_house_cat, levels = levels(brfss$ace_score_house_cat)[2:1])
brfss$ace_house_02 <- factor(brfss$ace_score_house_cat, levels = levels(brfss$ace_score_house_cat)[c(3,1)])
brfss$ace_house_03 <- factor(brfss$ace_score_house_cat, levels = levels(brfss$ace_score_house_cat)[c(4,1)])

# check with crosstabs
  # reference group is 0
table(brfss$ace_score_house_cat, brfss$ace_house_01)
table(brfss$ace_score_house_cat, brfss$ace_house_02)
table(brfss$ace_score_house_cat, brfss$ace_house_03)

# create 1/2 versions of physical, verbal, and sexual ace scores
brfss <- brfss %>% mutate(
  ace_phys_12 = case_when(
    as.numeric(ace_score_phys_cat) == 1 ~ 2, 
    as.numeric(ace_score_phys_cat) == 2 ~ 1, 
    TRUE ~ NA_real_
    ),
  ace_verb_12 = case_when(
    as.numeric(ace_score_verb_cat) == 1 ~ 2, 
    as.numeric(ace_score_verb_cat) == 2 ~ 1, 
    TRUE ~ NA_real_
    ), 
  ace_sex_12 = case_when(
    as.numeric(ace_score_sex_cat) == 1 ~ 2, 
    as.numeric(ace_score_sex_cat) == 2 ~ 1, 
    TRUE ~ NA_real_
  )
)

brfss$ace_phys_12 <- factor(brfss$ace_phys_12, levels = 1:2, labels = c("Yes", "No"))
brfss$ace_verb_12 <- factor(brfss$ace_verb_12, levels = 1:2, labels = c("Yes", "No"))
brfss$ace_sex_12 <- factor(brfss$ace_sex_12, levels = 1:2, labels = c("Yes", "No"))

# check with crosstabs
table(brfss$ace_score_phys_cat, brfss$ace_phys_12)
table(brfss$ace_score_verb_cat, brfss$ace_verb_12)
table(brfss$ace_score_sex_cat, brfss$ace_sex_12)

# calculate weighted prevalence of outcome and assemble into table that will be exported as csv

# set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

# assign weights
design <- svydesign(data = brfss, 
                    id = ~1, 
                    strata = ~strat, 
                    weights = ~svy_weight)

# weighted prevalence of outcome by composite ace score for all covariates

# overall prevalence of outcome by composite ace score
total <- data.frame(svytable(~decide_12 + ace_score_com_cat, design) %>% prop.table(margin = 2)) %>%
  filter(decide_12 == "Yes") %>% mutate(prop = round(Freq * 100, 1)) %>% select(-Freq) %>%
  pivot_wider(names_from = ace_score_com_cat, values_from = prop) %>% rename(covariate = decide_12)

# for each level of each covariate

sex <- data.frame(svytable(~decide_12 + ace_score_com_cat + sex, design)) %>% rename(covariate = 3)
age <- data.frame(svytable(~decide_12 + ace_score_com_cat + age_10yr_group, design))%>% rename(covariate = 3)
race_eth <- data.frame(svytable(~decide_12 + ace_score_com_cat + race_eth_mh, design))%>% rename(covariate = 3)
income <- data.frame(svytable(~decide_12 + ace_score_com_cat + income, design))%>% rename(covariate = 3)
education <- data.frame(svytable(~decide_12 + ace_score_com_cat + education, design))%>% rename(covariate = 3)
employ <- data.frame(svytable(~decide_12 + ace_score_com_cat + employ, design))%>% rename(covariate = 3)
ment_health <- data.frame(svytable(~decide_12 + ace_score_com_cat + ment_health, design))%>% rename(covariate = 3)
health_plan <- data.frame(svytable(~decide_12 + ace_score_com_cat + health_plan, design))%>% rename(covariate = 3)

# write a function to calculate prevalences from weighted frequencies 
prev_outcome_com <- function(x) {
  x %>% group_by(ace_score_com_cat, covariate) %>%
    mutate(sum = sum(Freq)) %>% ungroup() %>%
    mutate(prop = round((Freq / sum) * 100, 1)) %>% 
    filter(decide_12 == "Yes") %>%
    select(-Freq, -sum, -decide_12) %>%
    pivot_wider(names_from = ace_score_com_cat, values_from = prop)
}

# use a for loop to run this function for all covariates
covariates <- c("sex", "age", "race_eth", "income", "education", "employ", "ment_health", "health_plan")
for (i in covariates){
  assign(i, prev_outcome_com(get(i)))
}

# bind all covariates together
table2a <- rbind(total, sex, age, race_eth, income, education, employ, ment_health, health_plan)

# create a total column
sex <- data.frame(svytable(~decide_12 + sex, design) %>% prop.table(margin = 2)) 
age <- data.frame(svytable(~decide_12 + age_10yr_group, design) %>% prop.table(margin = 2)) 
race_eth <- data.frame(svytable(~decide_12 + race_eth_mh, design) %>% prop.table(margin = 2)) 
income <- data.frame(svytable(~decide_12 + income, design) %>% prop.table(margin = 2))
education <- data.frame(svytable(~decide_12 + education, design) %>% prop.table(margin = 2))
employ <- data.frame(svytable(~decide_12 + employ, design) %>% prop.table(margin = 2))
ment_health <- data.frame(svytable(~decide_12 + ment_health, design) %>% prop.table(margin = 2))
health_plan <- data.frame(svytable(~decide_12 + health_plan, design) %>% prop.table(margin = 2))

# function for total column
total_function <- function(x) {
  x %>%
  filter(decide_12 == "Yes") %>% 
  mutate(total = round(Freq * 100, 1)) %>% 
  select(-Freq, -decide_12) %>%
  rename(covariate = 1)
}

# use a for loop to run this function for all covariates
for (i in covariates){
  assign(i, total_function(get(i)))
}

# bind all covariates together to make total column
total_col <- rbind(sex, age, race_eth, income, education, employ, ment_health, health_plan)

# add the top row for formatting 
total_col <- rbind(data.frame(covariate = "Yes", total = NA_real_), total_col) %>% select(-covariate)

# add the total column to table 2a 
table2a1 <- cbind(table2a, total_col)

# weighted prevalence of outcome by household ace score for all covariates

#overall prevalence of outcome by household ace score
total <- data.frame(svytable(~decide_12 + ace_score_house_cat, design) %>% prop.table(margin = 2)) %>%
  filter(decide_12 == "Yes") %>% mutate(prop = round(Freq * 100, 1)) %>% select(-Freq) %>%
  pivot_wider(names_from = ace_score_house_cat, values_from = prop) %>% rename(covariate = decide_12)

# for each level of each covariate

sex <- data.frame(svytable(~decide_12 + ace_score_house_cat + sex, design)) %>% rename(covariate = 3)
age <- data.frame(svytable(~decide_12 + ace_score_house_cat + age_10yr_group, design))%>% rename(covariate = 3)
race_eth <- data.frame(svytable(~decide_12 + ace_score_house_cat + race_eth_mh, design))%>% rename(covariate = 3)
income <- data.frame(svytable(~decide_12 + ace_score_house_cat + income, design))%>% rename(covariate = 3)
education <- data.frame(svytable(~decide_12 + ace_score_house_cat + education, design))%>% rename(covariate = 3)
employ <- data.frame(svytable(~decide_12 + ace_score_house_cat + employ, design))%>% rename(covariate = 3)
ment_health <- data.frame(svytable(~decide_12 + ace_score_house_cat + ment_health, design))%>% rename(covariate = 3)
health_plan <- data.frame(svytable(~decide_12 + ace_score_house_cat + health_plan, design))%>% rename(covariate = 3)

# write a function to calculate prevalences from weighted frequencies 
prev_outcome_house <- function(x) {
  x %>% group_by(ace_score_house_cat, covariate) %>%
    mutate(sum = sum(Freq)) %>% ungroup() %>%
    mutate(prop = round((Freq / sum) * 100, 1)) %>% 
    filter(decide_12 == "Yes") %>%
    select(-Freq, -sum, -decide_12) %>%
    pivot_wider(names_from = ace_score_house_cat, values_from = prop)
}

# use a for loop to run this function for all covariates
for (i in covariates){
  assign(i, prev_outcome_house(get(i)))
}

# bind all covariates together to make table2b
table2b1 <- rbind(total, sex, age, race_eth, income, education, employ, ment_health, health_plan)

# weighted prevalence of outcome by physical, verbal, and sexual ace scores for all covariates

#overall prevalence of outcome
total_phys <- data.frame(svytable(~decide_12 + ace_score_phys_cat, design) %>% prop.table(margin = 2)) %>%
  filter(decide_12 == "Yes") %>% mutate(prop = round(Freq * 100, 1)) %>% select(-Freq) %>%
  pivot_wider(names_from = ace_score_phys_cat, values_from = prop) %>% rename(covariate = decide_12)

total_verb <- data.frame(svytable(~decide_12 + ace_score_verb_cat, design) %>% prop.table(margin = 2)) %>%
  filter(decide_12 == "Yes") %>% mutate(prop = round(Freq * 100, 1)) %>% select(-Freq) %>%
  pivot_wider(names_from = ace_score_verb_cat, values_from = prop) %>% rename(covariate = decide_12)

total_sex <- data.frame(svytable(~decide_12 + ace_score_sex_cat, design) %>% prop.table(margin = 2)) %>%
  filter(decide_12 == "Yes") %>% mutate(prop = round(Freq * 100, 1)) %>% select(-Freq) %>%
  pivot_wider(names_from = ace_score_sex_cat, values_from = prop) %>% rename(covariate = decide_12)

# for each level of each covariate for physical

sex <- data.frame(svytable(~decide_12 + ace_score_phys_cat + sex, design)) %>% rename(covariate = 3)
age <- data.frame(svytable(~decide_12 + ace_score_phys_cat + age_10yr_group, design))%>% rename(covariate = 3)
race_eth <- data.frame(svytable(~decide_12 + ace_score_phys_cat + race_eth_mh, design))%>% rename(covariate = 3)
income <- data.frame(svytable(~decide_12 + ace_score_phys_cat + income, design))%>% rename(covariate = 3)
education <- data.frame(svytable(~decide_12 + ace_score_phys_cat + education, design))%>% rename(covariate = 3)
employ <- data.frame(svytable(~decide_12 + ace_score_phys_cat + employ, design))%>% rename(covariate = 3)
ment_health <- data.frame(svytable(~decide_12 + ace_score_phys_cat + ment_health, design))%>% rename(covariate = 3)
health_plan <- data.frame(svytable(~decide_12 + ace_score_phys_cat + health_plan, design))%>% rename(covariate = 3)

# write a function to calculate prevalences from weighted frequencies 
prev_outcome_phys <- function(x) {
  x %>% group_by(ace_score_phys_cat, covariate) %>%
    mutate(sum = sum(Freq)) %>% ungroup() %>%
    mutate(prop = round((Freq / sum) * 100, 1)) %>% 
    filter(decide_12 == "Yes") %>%
    select(-Freq, -sum, -decide_12) %>%
    pivot_wider(names_from = ace_score_phys_cat, values_from = prop)
}

# use a for loop to run this function for all covariates
for (i in covariates){
  assign(i, prev_outcome_phys(get(i)))
}

# bind all covariates together to make table2c1
table2c1_phys <- rbind(total_phys, sex, age, race_eth, income, education, employ, ment_health, health_plan)

# for each level of each covariate for verbal

sex <- data.frame(svytable(~decide_12 + ace_score_verb_cat + sex, design)) %>% rename(covariate = 3)
age <- data.frame(svytable(~decide_12 + ace_score_verb_cat + age_10yr_group, design))%>% rename(covariate = 3)
race_eth <- data.frame(svytable(~decide_12 + ace_score_verb_cat + race_eth_mh, design))%>% rename(covariate = 3)
income <- data.frame(svytable(~decide_12 + ace_score_verb_cat + income, design))%>% rename(covariate = 3)
education <- data.frame(svytable(~decide_12 + ace_score_verb_cat + education, design))%>% rename(covariate = 3)
employ <- data.frame(svytable(~decide_12 + ace_score_verb_cat + employ, design))%>% rename(covariate = 3)
ment_health <- data.frame(svytable(~decide_12 + ace_score_verb_cat + ment_health, design))%>% rename(covariate = 3)
health_plan <- data.frame(svytable(~decide_12 + ace_score_verb_cat + health_plan, design))%>% rename(covariate = 3)

# write a function to calculate prevalences from weighted frequencies 
prev_outcome_verb <- function(x) {
  x %>% group_by(ace_score_verb_cat, covariate) %>%
    mutate(sum = sum(Freq)) %>% ungroup() %>%
    mutate(prop = round((Freq / sum) * 100, 1)) %>% 
    filter(decide_12 == "Yes") %>%
    select(-Freq, -sum, -decide_12) %>%
    pivot_wider(names_from = ace_score_verb_cat, values_from = prop)
}

# use a for loop to run this function for all covariates
for (i in covariates){
  assign(i, prev_outcome_verb(get(i)))
}

# bind all covariates together to make table2c1 for verbal
table2c1_verb <- rbind(total_verb, sex, age, race_eth, income, education, employ, ment_health, health_plan) %>% select(-covariate)

# for each level of each covariate for sexual

sex <- data.frame(svytable(~decide_12 + ace_score_sex_cat + sex, design)) %>% rename(covariate = 3)
age <- data.frame(svytable(~decide_12 + ace_score_sex_cat + age_10yr_group, design))%>% rename(covariate = 3)
race_eth <- data.frame(svytable(~decide_12 + ace_score_sex_cat + race_eth_mh, design))%>% rename(covariate = 3)
income <- data.frame(svytable(~decide_12 + ace_score_sex_cat + income, design))%>% rename(covariate = 3)
education <- data.frame(svytable(~decide_12 + ace_score_sex_cat + education, design))%>% rename(covariate = 3)
employ <- data.frame(svytable(~decide_12 + ace_score_sex_cat + employ, design))%>% rename(covariate = 3)
ment_health <- data.frame(svytable(~decide_12 + ace_score_sex_cat + ment_health, design))%>% rename(covariate = 3)
health_plan <- data.frame(svytable(~decide_12 + ace_score_sex_cat + health_plan, design))%>% rename(covariate = 3)

# write a function to calculate prevalences from weighted frequencies 
prev_outcome_sex <- function(x) {
  x %>% group_by(ace_score_sex_cat, covariate) %>%
    mutate(sum = sum(Freq)) %>% ungroup() %>%
    mutate(prop = round((Freq / sum) * 100, 1)) %>% 
    filter(decide_12 == "Yes") %>%
    select(-Freq, -sum, -decide_12) %>%
    pivot_wider(names_from = ace_score_sex_cat, values_from = prop)
}

# use a for loop to run this function for all covariates
for (i in covariates){
  assign(i, prev_outcome_sex(get(i)))
}

# bind all covariates together to make table2c1 for sexual
table2c1_sex <- rbind(total_sex, sex, age, race_eth, income, education, employ, ment_health, health_plan) %>% select(-covariate)

# bind physical, verbal, and sexual together to make table2c1
table2c1 <- cbind(table2c1_phys, table2c1_verb, table2c1_sex)

# M-H stratified analyses for prevalence ratios and 95% CI

# create all strata for unadjusted PRs
# 0 to 1 ace composite score
strat_1 <- epi.2by2(with(brfss, table(ace_score_01, decide_12)))
strat_1 <- data.frame(strat_1$massoc.detail$RR.strata.wald)

strat_1_sex <- epi.2by2(with(brfss, table(ace_score_01, decide_12, sex)))
strat_1_sex_p <- data.frame(strat_1_sex$massoc.detail$wRR.homog)
strat_1_sex <- data.frame(strat_1_sex$massoc.detail$RR.strata.wald)

strat_1_age <- epi.2by2(with(brfss, table(ace_score_01, decide_12, age_10yr_group)))
strat_1_age_p <- data.frame(strat_1_age$massoc.detail$wRR.homog)
strat_1_age <- data.frame(strat_1_age$massoc.detail$RR.strata.wald)

strat_1_race_eth <- epi.2by2(with(brfss, table(ace_score_01, decide_12, race_eth_mh)))
strat_1_race_eth_p <- data.frame(strat_1_race_eth$massoc.detail$wRR.homog)
strat_1_race_eth <- data.frame(strat_1_race_eth$massoc.detail$RR.strata.wald)

strat_1_income <- epi.2by2(with(brfss, table(ace_score_01, decide_12, income)))
strat_1_income_p <- data.frame(strat_1_income$massoc.detail$wRR.homog)
strat_1_income <- data.frame(strat_1_income$massoc.detail$RR.strata.wald)

strat_1_education <- epi.2by2(with(brfss, table(ace_score_01, decide_12, education)))
strat_1_education_p <- data.frame(strat_1_education$massoc.detail$wRR.homog)
strat_1_education <- data.frame(strat_1_education$massoc.detail$RR.strata.wald)

strat_1_employ <- epi.2by2(with(brfss, table(ace_score_01, decide_12, employ)))
strat_1_employ_p <- data.frame(strat_1_employ$massoc.detail$wRR.homog)
strat_1_employ <- data.frame(strat_1_employ$massoc.detail$RR.strata.wald)

strat_1_ment_health <- epi.2by2(with(brfss, table(ace_score_01, decide_12, ment_health)))
strat_1_ment_health_p <- data.frame(strat_1_ment_health$massoc.detail$wRR.homog)
strat_1_ment_health <- data.frame(strat_1_ment_health$massoc.detail$RR.strata.wald)

strat_1_health_plan <- epi.2by2(with(brfss, table(ace_score_01, decide_12, health_plan)))
strat_1_health_plan_p <- data.frame(strat_1_health_plan$massoc.detail$wRR.homog)
strat_1_health_plan <- data.frame(strat_1_health_plan$massoc.detail$RR.strata.wald)

# 0 to 2 ace composite score
strat_2 <- epi.2by2(with(brfss, table(ace_score_02, decide_12)))
strat_2 <- data.frame(strat_2$massoc.detail$RR.strata.wald)

strat_2_sex <- epi.2by2(with(brfss, table(ace_score_02, decide_12, sex)))
strat_2_sex_p <- data.frame(strat_2_sex$massoc.detail$wRR.homog)
strat_2_sex <- data.frame(strat_2_sex$massoc.detail$RR.strata.wald)

strat_2_age <- epi.2by2(with(brfss, table(ace_score_02, decide_12, age_10yr_group)))
strat_2_age_p <- data.frame(strat_2_age$massoc.detail$wRR.homog)
strat_2_age <- data.frame(strat_2_age$massoc.detail$RR.strata.wald)

strat_2_race_eth <- epi.2by2(with(brfss, table(ace_score_02, decide_12, race_eth_mh)))
strat_2_race_eth_p <- data.frame(strat_2_race_eth$massoc.detail$wRR.homog)
strat_2_race_eth <- data.frame(strat_2_race_eth$massoc.detail$RR.strata.wald)

strat_2_income <- epi.2by2(with(brfss, table(ace_score_02, decide_12, income)))
strat_2_income_p <- data.frame(strat_2_income$massoc.detail$wRR.homog)
strat_2_income <- data.frame(strat_2_income$massoc.detail$RR.strata.wald)

strat_2_education <- epi.2by2(with(brfss, table(ace_score_02, decide_12, education)))
strat_2_education_p <- data.frame(strat_2_education$massoc.detail$wRR.homog)
strat_2_education <- data.frame(strat_2_education$massoc.detail$RR.strata.wald)

strat_2_employ <- epi.2by2(with(brfss, table(ace_score_02, decide_12, employ)))
strat_2_employ_p <- data.frame(strat_2_employ$massoc.detail$wRR.homog)
strat_2_employ <- data.frame(strat_2_employ$massoc.detail$RR.strata.wald)

strat_2_ment_health <- epi.2by2(with(brfss, table(ace_score_02, decide_12, ment_health)))
strat_2_ment_health_p <- data.frame(strat_2_ment_health$massoc.detail$wRR.homog)
strat_2_ment_health <- data.frame(strat_2_ment_health$massoc.detail$RR.strata.wald)

strat_2_health_plan <- epi.2by2(with(brfss, table(ace_score_02, decide_12, health_plan)))
strat_2_health_plan_p <- data.frame(strat_2_health_plan$massoc.detail$wRR.homog)
strat_2_health_plan <- data.frame(strat_2_health_plan$massoc.detail$RR.strata.wald)

# 0 to 3 ace composite score
strat_3 <- epi.2by2(with(brfss, table(ace_score_03, decide_12)))
strat_3 <- data.frame(strat_3$massoc.detail$RR.strata.wald)

strat_3_sex <- epi.2by2(with(brfss, table(ace_score_03, decide_12, sex)))
strat_3_sex_p <- data.frame(strat_3_sex$massoc.detail$wRR.homog)
strat_3_sex <- data.frame(strat_3_sex$massoc.detail$RR.strata.wald)

strat_3_age <- epi.2by2(with(brfss, table(ace_score_03, decide_12, age_10yr_group)))
strat_3_age_p <- data.frame(strat_3_age$massoc.detail$wRR.homog)
strat_3_age <- data.frame(strat_3_age$massoc.detail$RR.strata.wald)

strat_3_race_eth <- epi.2by2(with(brfss, table(ace_score_03, decide_12, race_eth_mh)))
strat_3_race_eth_p <- data.frame(strat_3_race_eth$massoc.detail$wRR.homog)
strat_3_race_eth <- data.frame(strat_3_race_eth$massoc.detail$RR.strata.wald)

strat_3_income <- epi.2by2(with(brfss, table(ace_score_03, decide_12, income)))
strat_3_income_p <- data.frame(strat_3_income$massoc.detail$wRR.homog)
strat_3_income <- data.frame(strat_3_income$massoc.detail$RR.strata.wald)

strat_3_education <- epi.2by2(with(brfss, table(ace_score_03, decide_12, education)))
strat_3_education_p <- data.frame(strat_3_education$massoc.detail$wRR.homog)
strat_3_education <- data.frame(strat_3_education$massoc.detail$RR.strata.wald)

strat_3_employ <- epi.2by2(with(brfss, table(ace_score_03, decide_12, employ)))
strat_3_employ_p <- data.frame(strat_3_employ$massoc.detail$wRR.homog)
strat_3_employ <- data.frame(strat_3_employ$massoc.detail$RR.strata.wald)

strat_3_ment_health <- epi.2by2(with(brfss, table(ace_score_03, decide_12, ment_health)))
strat_3_ment_health_p <- data.frame(strat_3_ment_health$massoc.detail$wRR.homog)
strat_3_ment_health <- data.frame(strat_3_ment_health$massoc.detail$RR.strata.wald)

strat_3_health_plan <- epi.2by2(with(brfss, table(ace_score_03, decide_12, health_plan)))
strat_3_health_plan_p <- data.frame(strat_3_health_plan$massoc.detail$wRR.homog)
strat_3_health_plan <- data.frame(strat_3_health_plan$massoc.detail$RR.strata.wald)

# 0 to 4 ace composite score
strat_4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12)))
strat_4 <- data.frame(strat_4$massoc.detail$RR.strata.wald)

strat_4_sex <- epi.2by2(with(brfss, table(ace_score_04, decide_12, sex)))
strat_4_sex_p <- data.frame(strat_4_sex$massoc.detail$wRR.homog)
strat_4_sex <- data.frame(strat_4_sex$massoc.detail$RR.strata.wald)

strat_4_age <- epi.2by2(with(brfss, table(ace_score_04, decide_12, age_10yr_group)))
strat_4_age_p <- data.frame(strat_4_age$massoc.detail$wRR.homog)
strat_4_age <- data.frame(strat_4_age$massoc.detail$RR.strata.wald)

strat_4_race_eth <- epi.2by2(with(brfss, table(ace_score_04, decide_12, race_eth_mh)))
strat_4_race_eth_p <- data.frame(strat_4_race_eth$massoc.detail$wRR.homog)
strat_4_race_eth <- data.frame(strat_4_race_eth$massoc.detail$RR.strata.wald)

strat_4_income <- epi.2by2(with(brfss, table(ace_score_04, decide_12, income)))
strat_4_income_p <- data.frame(strat_4_income$massoc.detail$wRR.homog)
strat_4_income <- data.frame(strat_4_income$massoc.detail$RR.strata.wald)

strat_4_education <- epi.2by2(with(brfss, table(ace_score_04, decide_12, education)))
strat_4_education_p <- data.frame(strat_4_education$massoc.detail$wRR.homog)
strat_4_education <- data.frame(strat_4_education$massoc.detail$RR.strata.wald)

strat_4_employ <- epi.2by2(with(brfss, table(ace_score_04, decide_12, employ)))
strat_4_employ_p <- data.frame(strat_4_employ$massoc.detail$wRR.homog)
strat_4_employ <- data.frame(strat_4_employ$massoc.detail$RR.strata.wald)

strat_4_ment_health <- epi.2by2(with(brfss, table(ace_score_04, decide_12, ment_health)))
strat_4_ment_health_p <- data.frame(strat_4_ment_health$massoc.detail$wRR.homog)
strat_4_ment_health <- data.frame(strat_4_ment_health$massoc.detail$RR.strata.wald)

strat_4_health_plan <- epi.2by2(with(brfss, table(ace_score_04, decide_12, health_plan)))
strat_4_health_plan_p <- data.frame(strat_4_health_plan$massoc.detail$wRR.homog)
strat_4_health_plan <- data.frame(strat_4_health_plan$massoc.detail$RR.strata.wald)

# bind the rows together for each strat, round
strat_1_tab <- round(rbind(strat_1, strat_1_sex, strat_1_age, strat_1_race_eth, strat_1_income, strat_1_education, strat_1_employ, strat_1_ment_health, strat_1_health_plan), 2)
strat_2_tab <- round(rbind(strat_2, strat_2_sex, strat_2_age, strat_2_race_eth, strat_2_income, strat_2_education, strat_2_employ, strat_2_ment_health, strat_2_health_plan), 2)
strat_3_tab <- round(rbind(strat_3, strat_3_sex, strat_3_age, strat_3_race_eth, strat_3_income, strat_3_education, strat_3_employ, strat_3_ment_health, strat_3_health_plan), 2)
strat_4_tab <- round(rbind(strat_4, strat_4_sex, strat_4_age, strat_4_race_eth, strat_4_income, strat_4_education, strat_4_employ, strat_4_ment_health, strat_4_health_plan), 2)

# write a function to create a new column that combines the PR and 95 CI into one cell with formatting
est_95_ci <- function(x) {
  x %>% mutate(PR = paste0(est," (", lower, ", ", upper, ")")) %>%
    select(PR)
}
strat_1_tab <- est_95_ci(strat_1_tab)
strat_2_tab <- est_95_ci(strat_2_tab)
strat_3_tab <- est_95_ci(strat_3_tab)
strat_4_tab <- est_95_ci(strat_4_tab)

# bind all strats together
pr_output <- cbind(strat_1_tab, strat_2_tab, strat_3_tab, strat_4_tab)

# bind strats to existing table2a1
table2a <- cbind(table2a1, pr_output)

# write to csv
write.csv(table2a, "table2a.csv", row.names = FALSE)

# bind all the p-value rows and columns together
strat_1_p <- rbind(strat_1_sex_p, strat_1_age_p, strat_1_race_eth_p, strat_1_income_p, strat_1_education_p, strat_1_employ_p, strat_1_ment_health_p, strat_1_health_plan_p) %>% select(p.value) %>% rename(one = 1)
strat_2_p <- rbind(strat_2_sex_p, strat_2_age_p, strat_2_race_eth_p, strat_2_income_p, strat_2_education_p, strat_2_employ_p, strat_2_ment_health_p, strat_2_health_plan_p) %>% select(p.value) %>% rename(two = 1)
strat_3_p <- rbind(strat_3_sex_p, strat_3_age_p, strat_3_race_eth_p, strat_3_income_p, strat_3_education_p, strat_3_employ_p, strat_3_ment_health_p, strat_3_health_plan_p) %>% select(p.value) %>% rename(three = 1)
strat_4_p <- rbind(strat_4_sex_p, strat_4_age_p, strat_4_race_eth_p, strat_4_income_p, strat_4_education_p, strat_4_employ_p, strat_4_ment_health_p, strat_4_health_plan_p) %>% select(p.value) %>% rename(four = 1)

# bind strat p-values together to make table
p_table_ace_com <- round(cbind(strat_1_p, strat_2_p, strat_3_p, strat_4_p), 4) 
covariate <-  c("sex", "age", "race", "income", "education", "employ", "ment_health", "health_plan")
p_table_ace_com$covariate <- covariate
p_table_ace_com <- p_table_ace_com %>% select(covariate, one, two, three, four)
write.csv(p_table_ace_com, "p_table_ace_com.csv", row.names = FALSE)

# based on results of table2a and p-values table check for both confounding and effect modification for some covariates
# sex
sex1 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, sex)))
(sex1$massoc.detail$RR.crude.wald - sex1$massoc.detail$RR.mh.wald)/sex1$massoc.detail$RR.crude.wald
# is age both an effect modifier and confounder
age1 <- epi.2by2(with(brfss, table(ace_score_01, decide_12, age_10yr_group)))
(age1$massoc.detail$RR.crude.wald - age1$massoc.detail$RR.mh.wald)/age1$massoc.detail$RR.crude.wald
age2 <- epi.2by2(with(brfss, table(ace_score_02, decide_12, age_10yr_group)))
(age2$massoc.detail$RR.crude.wald - age2$massoc.detail$RR.mh.wald)/age2$massoc.detail$RR.crude.wald
age3 <- epi.2by2(with(brfss, table(ace_score_03, decide_12, age_10yr_group)))
(age3$massoc.detail$RR.crude.wald - age3$massoc.detail$RR.mh.wald)/age3$massoc.detail$RR.crude.wald
age4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, age_10yr_group)))
(age4$massoc.detail$RR.crude.wald - age4$massoc.detail$RR.mh.wald)/age4$massoc.detail$RR.crude.wald
# race_eth
race_eth1 <- epi.2by2(with(brfss, table(ace_score_01, decide_12, race_eth_mh)))
(race_eth1$massoc.detail$RR.crude.wald - race_eth1$massoc.detail$RR.mh.wald)/race_eth1$massoc.detail$RR.crude.wald
race_eth2 <- epi.2by2(with(brfss, table(ace_score_02, decide_12, race_eth_mh)))
(race_eth2$massoc.detail$RR.crude.wald - race_eth2$massoc.detail$RR.mh.wald)/race_eth2$massoc.detail$RR.crude.wald
race_eth3 <- epi.2by2(with(brfss, table(ace_score_03, decide_12, race_eth_mh)))
(race_eth3$massoc.detail$RR.crude.wald - race_eth3$massoc.detail$RR.mh.wald)/race_eth3$massoc.detail$RR.crude.wald
race_eth4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, race_eth_mh)))
(race_eth4$massoc.detail$RR.crude.wald - race_eth4$massoc.detail$RR.mh.wald)/race_eth4$massoc.detail$RR.crude.wald
# income
income1 <- epi.2by2(with(brfss, table(ace_score_01, decide_12, income)))
(income1$massoc.detail$RR.crude.wald - income1$massoc.detail$RR.mh.wald)/income1$massoc.detail$RR.crude.wald
income2 <- epi.2by2(with(brfss, table(ace_score_02, decide_12, income)))
(income2$massoc.detail$RR.crude.wald - income2$massoc.detail$RR.mh.wald)/income2$massoc.detail$RR.crude.wald
income3 <- epi.2by2(with(brfss, table(ace_score_03, decide_12, income)))
(income3$massoc.detail$RR.crude.wald - income3$massoc.detail$RR.mh.wald)/income3$massoc.detail$RR.crude.wald
income4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, income)))
(income4$massoc.detail$RR.crude.wald - income4$massoc.detail$RR.mh.wald)/income4$massoc.detail$RR.crude.wald
# education
education1 <- epi.2by2(with(brfss, table(ace_score_01, decide_12, education)))
(education1$massoc.detail$RR.crude.wald - education1$massoc.detail$RR.mh.wald)/education1$massoc.detail$RR.crude.wald
education2 <- epi.2by2(with(brfss, table(ace_score_02, decide_12, education)))
(education2$massoc.detail$RR.crude.wald - education2$massoc.detail$RR.mh.wald)/education2$massoc.detail$RR.crude.wald
education3 <- epi.2by2(with(brfss, table(ace_score_03, decide_12, education)))
(education3$massoc.detail$RR.crude.wald - education3$massoc.detail$RR.mh.wald)/education3$massoc.detail$RR.crude.wald
education4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, education)))
(education4$massoc.detail$RR.crude.wald - education4$massoc.detail$RR.mh.wald)/education4$massoc.detail$RR.crude.wald
# employment 
employ4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, employ)))
(employ4$massoc.detail$RR.crude.wald - employ4$massoc.detail$RR.mh.wald)/employ4$massoc.detail$RR.crude.wald
# health plan
health_plan2 <- epi.2by2(with(brfss, table(ace_score_02, decide_12, health_plan)))
(health_plan2$massoc.detail$RR.crude.wald - health_plan2$massoc.detail$RR.mh.wald)/health_plan2$massoc.detail$RR.crude.wald
health_plan4 <- epi.2by2(with(brfss, table(ace_score_04, decide_12, health_plan)))
(health_plan4$massoc.detail$RR.crude.wald - health_plan4$massoc.detail$RR.mh.wald)/health_plan4$massoc.detail$RR.crude.wald

# report adjusted PRs for each ace group
ace_1 <- epi.2by2(with(brfss, table(ace_score_01, decide_12, ment_health)))
data.frame(ace_1$massoc.detail$RR.mh.wald)

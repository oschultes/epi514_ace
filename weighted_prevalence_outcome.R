# prevalence of outcome (weighted)
# epi 514
# bridget waters
# created: may 9 2022
# last updated: may 15 2022

# this file calculates the prevalence of the outcome across exposure groups and across all covariates 
  # we ultimately didn't use these results for epi 514 because we were told to calculate unweighted prevalence values

# calculate prevalences of outcome across all ace scores and effect modifiers and assemble into table that will be exported as csv

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

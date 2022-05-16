
# results & table 2
# epi 514
# bridget waters
# created: may 9 2022
# last updated: may 15 2022

# description: Mantel-Haenszel stratified analyses and tables for results

# be sure to run part2data.R and dataexplore_table1.R prior to running this R script

# components
  # consolidate and recode variables in preparation for M-H analyses
  # Mantel-Haenszel stratified analyses to calculate prevalence ratios
  # calculate unweighted prevalence of outcome for all exposure groups and for all covariates

# packages in addition to those loaded in part2data and dataexplore_table1
library(gdata)
library(epiR)
library(broom)
# clear out environment except for data
keep(brfss, brfss_nonNA, sure = TRUE)
detach("package:gdata", unload = TRUE)

# create a new race variable that combines race/ethnicity groups with small sample sizes
  # combines American Indian or Alaskan Native only,non-Hispanic with Other race only, non-Hispanic and Multiracial, non-Hispanic
  # combines Asian only, non-Hispanic with Native Hawaiian or Other Pacific Islander only, non-Hispanic
brfss_nonNA$race_eth_mh <- as.numeric(brfss_nonNA$race_eth)
brfss_nonNA$race_eth_mh <- recode(brfss_nonNA$race_eth_mh, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 4L, `6` = 3L, `7` = 3L, `8` = 5L)
brfss_nonNA$race_eth_mh <- factor(brfss_nonNA$race_eth_mh, levels = 1:5, labels = c("White only, non-Hispanic",
                                                                  "Black only, non-Hispanic",
                                                                  "Other, non-Hispanic", 
                                                                  "Asian or Pacific Islander, non-Hispanic", 
                                                                  "Hispanic"))
summary(brfss_nonNA$race_eth_mh)
table(brfss_nonNA$race_eth, brfss_nonNA$race_eth_mh)

# create a 1/2 version of decide variable (outcome)
brfss_nonNA <- brfss_nonNA %>% mutate(
  decide_12 = case_when(
    as.numeric(decide) == 1 ~ 2, 
    as.numeric(decide) == 2 ~ 1, 
    TRUE ~ NA_real_
  )
)
brfss_nonNA$decide_12 <- factor(brfss_nonNA$decide_12, levels = 1:2, labels = c("Yes", "No"))
table(brfss_nonNA$decide, brfss_nonNA$decide_12)

# create indicator exposure variables for bivariate comparisons
# composite ace score
  # reference group for composite ace score is 0
brfss_nonNA$ace_score_01 <- factor(brfss_nonNA$ace_score_com_cat, levels = levels(brfss_nonNA$ace_score_com_cat)[2:1])
brfss_nonNA$ace_score_02 <- factor(brfss_nonNA$ace_score_com_cat, levels = levels(brfss_nonNA$ace_score_com_cat)[c(3,1)])
brfss_nonNA$ace_score_03 <- factor(brfss_nonNA$ace_score_com_cat, levels = levels(brfss_nonNA$ace_score_com_cat)[c(4,1)])
brfss_nonNA$ace_score_04 <- factor(brfss_nonNA$ace_score_com_cat, levels = levels(brfss_nonNA$ace_score_com_cat)[c(5,1)])

# check with crosstabs
table(brfss_nonNA$ace_score_com_cat, brfss_nonNA$ace_score_01)
table(brfss_nonNA$ace_score_com_cat, brfss_nonNA$ace_score_02)
table(brfss_nonNA$ace_score_com_cat, brfss_nonNA$ace_score_03)
table(brfss_nonNA$ace_score_com_cat, brfss_nonNA$ace_score_04)


# write a function to get the overall prevalence ratio of outcome across exposure groups

# make a list of all the levels of exposure and covariates to examine 
ace_scores <- c("ace_score_01", "ace_score_02", "ace_score_03", "ace_score_04")
covariates <- c("sex", "age_10yr_group", "race_eth_mh", "race_eth", "income", "education", "employ", "ment_health", "health_plan")

tidy_PR_overall <- function(x) {
  # using tidy() from broom package and epi.2by2 from epiR package
  # use parameters argument to pull the measures of association from epi.2by2
  tidy(epi.2by2(table(brfss_nonNA[, x], brfss_nonNA$decide_12)), parameters = c("moa")) %>% 
  # filter for the prevalence ratio
  filter(term == "RR.strata.wald") %>% 
  # round ot 2 digits after decimal place
  select(-term) %>% round(2) %>%
  # format the table to incorporate the prevalence ratio and 95% CI into one cell with parentheses
  mutate(covariate = "Overall", ace_score = x, 
           PR = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  select(covariate, ace_score, PR)
}

# create an empty tibble to run the for loop 
tidy_PRs <- tibble(covariate = NA, ace_score = NA, PR = NA)

# run the for loop across all ace scores
for (i in ace_scores){
  a <- tidy_PR_overall(i)
  tidy_PRs <- rbind(tidy_PRs, a)
}

tidy_PRs <- tidy_PRs %>% drop_na() %>% pivot_wider(names_from = ace_score, values_from = PR)

# write a function to get the prevalence ratio of outcome across exposure groups for each covariate
tidy_PR <- function(x, y){
  # using tidy() from broom package and epi.2by2 from epiR package
  # use parameters argument to pull the measures of association from epi.2by2
  tidy(epi.2by2(table(brfss_nonNA[, x], brfss_nonNA$decide_12, brfss_nonNA[, y])), parameters = c("moa")) %>% 
  # filter for the prevalence ratio
  filter(term == "RR.strata.wald") %>% 
  # round ot 2 digits after decimal place
  select(-term) %>% round(2) %>%
  # formatting to prepare to rbind
  mutate(covariate = levels(brfss_nonNA[, y]), ace_score = x)
}

# create an empty tibble to run the for loop
tidy_PRs_2 <- tibble(estimate = NA, conf.low = NA, conf.high = NA, covariate = NA, ace_score = NA)

# nested for loop
for (i in ace_scores){
  for (z in covariates){
    a <- tidy_PR(i, z)
    tidy_PRs_2 <- rbind(tidy_PRs_2, a)
  }
}

# formatting 
tidy_PRs_2 <- tidy_PRs_2 %>% drop_na() %>% unique() %>%
  mutate(PR = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  select(covariate, ace_score, PR) %>% 
  pivot_wider(names_from = ace_score, values_from = PR)

# format the levels of race/ethnicity
tidy_PRs_2 <- tidy_PRs_2 %>% slice(1:11, 14, 18, 17, 12, 15:16, 13, 19:35)

# bind the overall with the covariates
tidy_PRs <- rbind(tidy_PRs, tidy_PRs_2)

# write to csv
write.csv(tidy_PRs, "prevalence_ratios.csv", row.names = FALSE)

# p-values for Mantel-Haenszel test of homogeneity across strata

# write a function to pull the p-values from epi.2by2 
  # significant p-value means that the levels of the strata are significantly different according to the Mantel-Haenszel test of homogeneity
tidy_pval <- function(x, y){
  tidy(epi.2by2(table(brfss_nonNA[, x], brfss_nonNA$decide_12, brfss_nonNA[, y])), parameters = c("stat")) %>% 
    filter(term == "wRR.homog") %>% select(p.value) %>% round(3) %>% 
    mutate(covariate = y, ace_score = x) %>%
    select(covariate, ace_score, p.value)
}

# create an empty tibble to run the for loop
tidy_pvalues <- tibble(covariate = NA, ace_score = NA, p.value = NA)

# create a list of covariates that only includes the race_eth_mh race variable

covariates2 <- c("sex", "age_10yr_group", "race_eth_mh", "income", "education", "employ", "ment_health", "health_plan")

# nested for loop
for (i in ace_scores){
  for (z in covariates2){
    a <- tidy_pval(i, z)
    tidy_pvalues <- rbind(tidy_pvalues, a)
  }
}

# formatting
tidy_pvalues <- tidy_pvalues %>% drop_na() %>%
  pivot_wider(names_from = ace_score, values_from = p.value)

# write to csv
write.csv(tidy_pvalues, "pvalues_MH_homogeneity.csv", row.names = FALSE)

# calculate unweighted prevalence of the outcome across all ace_score groups
unwt_prev_overall <- brfss_nonNA %>% select(ace_score_com_cat, decide_12) %>% 
  group_by(ace_score_com_cat, decide_12) %>% summarise(n = n()) %>% ungroup() %>%
  group_by(ace_score_com_cat) %>% mutate(sum = sum(n)) %>% mutate(prop = round((n/ sum) * 100, 1), covariate = "Overall") %>%
  filter(decide_12 == "Yes") %>% select(ace_score_com_cat, covariate, prop) %>% 
  pivot_wider(names_from = ace_score_com_cat, values_from = prop)
  
# write a function to calculate the unweighted prevalence values for each covariate across ace score categories
unwt_prev <- function(x) {
  brfss_nonNA %>% select(ace_score_com_cat, decide_12, {{x}}) %>%
    group_by(ace_score_com_cat, decide_12, {{x}}) %>% summarise(n = n()) %>% ungroup() %>%
    group_by(ace_score_com_cat, {{x}}) %>% mutate(sum = sum(n)) %>% 
    mutate(prop = round((n/ sum) * 100, 1), covariate = {{x}}) %>% 
    mutate(covariate = as.character(covariate)) %>%
    filter(decide_12 == "Yes") %>% ungroup() %>% 
    select(ace_score_com_cat, covariate, prop) %>% drop_na() %>%
    pivot_wider(names_from = ace_score_com_cat, values_from = prop)
}

# run the function on all the covariates
sex <- unwt_prev(sex)
age <- unwt_prev(age_10yr_group)
race_eth_mh <- unwt_prev(race_eth_mh) %>% filter(covariate == "Other, non-Hispanic" | covariate == "Asian or Pacific Islander, non-Hispanic")
race_eth <- unwt_prev(race_eth)
income <- unwt_prev(income)
education <- unwt_prev(education)
employ <- unwt_prev(employ)
ment_health <- unwt_prev(ment_health)
health_plan <- unwt_prev(health_plan)

# bind race_eth tibbles together and order the rows correctly 
race_eth <- rbind(race_eth_mh, race_eth)
race_eth <- race_eth %>% slice(3:4, 1, 5, 9 ,8, 2, 6:7, 10)

# bind all covariates together
unwt_prevalences <- rbind(sex, age, race_eth, income, education, employ, ment_health, health_plan)

# calculate the overall unweighted prevalence for each covariate (not by ace score)
unwt_prev_nc <- function(x) {
  brfss_nonNA %>% select(decide_12, {{x}}) %>%
    group_by(decide_12, {{x}}) %>% summarise(n = n()) %>% ungroup() %>%
    group_by({{x}}) %>% mutate(sum = sum(n)) %>% 
    mutate(prop = round((n/ sum) * 100, 1), covariate = {{x}}) %>% 
    mutate(covariate = as.character(covariate)) %>%
    filter(decide_12 == "Yes") %>% ungroup() %>% 
    select(covariate, prop) %>% drop_na() 
}

# run the function on all the covariates
sex <- unwt_prev_nc(sex)
age <- unwt_prev_nc(age_10yr_group)
race_eth_mh <- unwt_prev_nc(race_eth_mh) %>% filter(covariate == "Other, non-Hispanic" | covariate == "Asian or Pacific Islander, non-Hispanic")
race_eth <- unwt_prev_nc(race_eth)
income <- unwt_prev_nc(income)
education <- unwt_prev_nc(education)
employ <- unwt_prev_nc(employ)
ment_health <- unwt_prev_nc(ment_health)
health_plan <- unwt_prev_nc(health_plan)

# bind race_eth tibbles together and order the rows correctly 
race_eth <- rbind(race_eth_mh, race_eth) 
race_eth <- race_eth %>% slice(3:4, 1, 5, 9 ,8, 2, 6:7, 10)

# bind all covariates together
unwt_prevalences_nc <- rbind(sex, age, race_eth, income, education, employ, ment_health, health_plan)

unwt_prevalences_nc <- unwt_prevalences_nc %>% rename(Overall = prop) %>% select(Overall)

unwt_prevalences <- cbind(unwt_prevalences, unwt_prevalences_nc)

# write to csv
write.csv(unwt_prevalences, "unwt_prevalences.csv", row.names = FALSE)

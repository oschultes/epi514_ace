# data cleaning part II
# epi 514
# mckenzi norris
# created: april 29 2022
# last updated: may 7 2022

# description: read in part 1's reweighted and combined data,
# create backup part1 variables before recoding values for the outcome, exposure, and covariate variables, which will
# be recoded and factored for missing, yes/no , and frequency responses

rm(list=ls())
brfss <- read.csv("reweighted_2019_2020.csv")
write.csv(brfss, "brfss.csv") #attempt to create new copy instead of overriding
View(brfss)

#recoding outcome
unique(brfss$decide) #lists unique values to check for errors
sum(is.na(brfss$decide)) #lists the number of NAs
brfss$old_decide <- brfss$decide #depreciates part1 variable
brfss$decide[brfss$decide==7] <- NA
brfss$decide[brfss$decide==9] <- NA
brfss$decide[brfss$decide==2] <- 0
brfss$decide <- factor(brfss$decide, levels = 0:1, labels = c("No", "Yes"))

#recoding exposure
brfss$old_ace_depres <- brfss$ace_depres #creates a depreciated part1 variable
brfss$ace_depres[brfss$ace_depres==7] <- NA
brfss$ace_depres[brfss$ace_depres==9] <- NA
brfss$ace_depres[brfss$ace_depres==2] <- 0

brfss$old_ace_drink <- brfss$ace_drink #creates a depreciated part1 variable
brfss$ace_drink[brfss$ace_drink==7] <- NA
brfss$ace_drink[brfss$ace_drink==9] <- NA
brfss$ace_drink[brfss$ace_drink==2] <- 0

brfss$old_ace_drugs <- brfss$ace_drugs #creates a depreciated part1 variable
brfss$ace_drugs[brfss$ace_drugs==7] <- NA
brfss$ace_drugs[brfss$ace_drugs==9] <- NA
brfss$ace_drugs[brfss$ace_drugs==2] <- 0

brfss$old_ace_prison <- brfss$ace_prison #creates a depreciated part1 variable
brfss$ace_prison[brfss$ace_prison==7] <- NA
brfss$ace_prison[brfss$ace_prison==9] <- NA
brfss$ace_prison[brfss$ace_prison==2] <- 0

brfss$old_ace_divor <- brfss$ace_divor #creates a depreciated part1 variable
brfss$ace_divor[brfss$ace_divor==7] <- NA
brfss$ace_divor[brfss$ace_divor==9] <- NA
brfss$ace_divor[brfss$ace_divor==2] <- 0
brfss$ace_divor[brfss$ace_divor==8] <- 0

brfss$old_ace_punch <- brfss$ace_punch #creates a depreciated part1 variable
brfss$ace_punch[brfss$ace_punch==1] <- 0
brfss$ace_punch[brfss$ace_punch==7] <- NA
brfss$ace_punch[brfss$ace_punch==9] <- NA
brfss$ace_punch[brfss$ace_punch==2] <- 1
brfss$ace_punch[brfss$ace_punch==3] <- 1

brfss$old_ace_hurt <- brfss$ace_hurt #creates a depreciated part1 variable
brfss$ace_hurt[brfss$ace_hurt==1] <- 0
brfss$ace_hurt[brfss$ace_hurt==7] <- NA
brfss$ace_hurt[brfss$ace_hurt==9] <- NA
brfss$ace_hurt[brfss$ace_hurt==2] <- 1
brfss$ace_hurt[brfss$ace_hurt==3] <- 1

brfss$old_ace_swear <- brfss$ace_swear #creates a depreciated part1 variable
brfss$ace_swear[brfss$ace_swear==1] <- 0
brfss$ace_swear[brfss$ace_swear==7] <- 0
brfss$ace_swear[brfss$ace_swear==9] <- 0
brfss$ace_swear[brfss$ace_swear==2] <- 1
brfss$ace_swear[brfss$ace_swear==3] <- 1

brfss$old_ace_touch_receive <- brfss$ace_touch_receive #creates a depreciated part1 variable
brfss$ace_touch_receive[brfss$ace_touch_receive==1] <- 0
brfss$ace_touch_receive[brfss$ace_touch_receive==7] <- NA
brfss$ace_touch_receive[brfss$ace_touch_receive==9] <- NA
brfss$ace_touch_receive[brfss$ace_touch_receive==2] <- 1
brfss$ace_touch_receive[brfss$ace_touch_receive==3] <- 1

brfss$old_ace_touch_give <- brfss$ace_touch_give #creates a depreciated part1 variable
brfss$ace_touch_give[brfss$ace_touch_give==1] <- 0
brfss$ace_touch_give[brfss$ace_touch_give==7] <- NA
brfss$ace_touch_give[brfss$ace_touch_give==9] <- NA
brfss$ace_touch_give[brfss$ace_touch_give==2] <- 1
brfss$ace_touch_give[brfss$ace_touch_give==3] <- 1

brfss$old_ace_sex <- brfss$ace_sex #creates a depreciated part1 variable
brfss$ace_sex[brfss$ace_sex==1] <- 0
brfss$ace_sex[brfss$ace_sex==7] <- NA
brfss$ace_sex[brfss$ace_sex==9] <- NA
brfss$ace_sex[brfss$ace_sex==2] <- 1
brfss$ace_sex[brfss$ace_sex==3] <- 1

#recoding covariates
brfss$old_sex <- brfss$sex #creates a depreciated part1 variable
brfss$sex[brfss$sex==2] <- 0
brfss$sex <- factor(brfss$sex, levels = 0:1, labels = c("Female", "Male"))

brfss$old_income <- brfss$income #creates a depreciated part1 variable
unique(brfss$income)
brfss$income[brfss$income==77] <- NA
brfss$income[brfss$income==99] <- NA
brfss$income[brfss$income==2] <- 1
brfss$income[brfss$income==3] <- 1
brfss$income[brfss$income==4] <- 2
brfss$income[brfss$income==5] <- 2
brfss$income[brfss$income==6] <- 3
brfss$income[brfss$income==7] <- 3
brfss$income[brfss$income==8] <- 4
brfss$income <- factor(brfss$income, levels = 1:4, labels = c("<$20K", "$20-34K", "$35-74K", "75K+"))

brfss$old_age_5yr_group <- brfss$age_5yr_group #creates a depreciated part1 variable
unique(brfss$age_5yr_group)
brfss$age_5yr_group[brfss$age_5yr_group==14] <- NA
brfss$age_5yr_group <- factor(brfss$age_5yr_group, levels = 1:13, labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64", "65-69", "70-74", "75-79", "80+"))

brfss$age_10yr_group <- as.numeric(brfss$age_5yr_group) #creates a depreciated part1 variable
unique(brfss$age_10yr_group)
brfss$age_10yr_group[brfss$age_10yr_group == 3] <- 2
brfss$age_10yr_group[brfss$age_10yr_group == 4] <- 3
brfss$age_10yr_group[brfss$age_10yr_group == 5] <- 3
brfss$age_10yr_group[brfss$age_10yr_group == 6] <- 4
brfss$age_10yr_group[brfss$age_10yr_group == 7] <- 4
brfss$age_10yr_group[brfss$age_10yr_group == 8] <- 5
brfss$age_10yr_group[brfss$age_10yr_group == 9] <- 5
brfss$age_10yr_group[brfss$age_10yr_group == 10] <- 6
brfss$age_10yr_group[brfss$age_10yr_group == 11] <- 6
brfss$age_10yr_group[brfss$age_10yr_group == 12] <- 7
brfss$age_10yr_group[brfss$age_10yr_group == 13] <- 7
brfss$age_10yr_group <- factor(brfss$age_10yr_group, levels = 1:7, labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))

brfss$old_age_65_plus <- brfss$age_65_plus #creates a depreciated part1 variable
unique(brfss$age_65_plus)
brfss$age_65_plus[brfss$age_65_plus==3] <- NA
brfss$age_65_plus[brfss$age_65_plus==1] <- 0
brfss$age_65_plus[brfss$age_65_plus==2] <- 1
brfss$age_65_plus <- factor(brfss$age_65_plus, levels = 0:1, labels = c("18-64", "65+"))

brfss$old_race_eth <- brfss$race_eth #creates a depreciated part1 variable
unique(brfss$race_eth)
brfss$race_eth[brfss$race_eth==9] <- NA
brfss$race_eth <- factor(brfss$race_eth, levels = 1:8, labels = c("White only, non-Hispanic", "Black only, non-Hispanic", "American Indian or Alaskan Native only, non-Hispanic", "Asian only, non-Hispanic", "Native Hawaiian or other Pacific Islander only, non-Hispanic", "Other race only, non-Hispanic", "Multiracial, non-Hispanic", "Hispanic"))

brfss$old_education <- brfss$education #creates a depreciated part1 variable
unique(brfss$education)
brfss$education[brfss$education==9] <- NA
brfss$education[brfss$education==2] <- 1
brfss$education[brfss$education==3] <- 1
brfss$education[brfss$education==4] <- 2
brfss$education[brfss$education==5] <- 3
brfss$education[brfss$education==6] <- 4
brfss$education <- factor(brfss$education, levels = 1:4, labels = c("Did not graduate high school", "High school graduate", "Some college", "College graduate"))

brfss$old_employ <- brfss$employ #creates a depreciated part1 variable
unique(brfss$employ)
brfss$employ[brfss$employ==9] <- NA
brfss$employ[brfss$employ==2] <- 1
brfss$employ[brfss$employ==3] <- 0
brfss$employ[brfss$employ==4] <- 0
brfss$employ[brfss$employ==5] <- 0
brfss$employ[brfss$employ==6] <- 0
brfss$employ[brfss$employ==7] <- 0
brfss$employ[brfss$employ==8] <- 0
brfss$employ <- factor(brfss$employ, levels = 0:1, labels = c("Not employed", "Employed"))

brfss$old_ment_health <- brfss$ment_health #creates a depreciated part1 variable
unique(brfss$ment_health)
brfss$ment_health[brfss$ment_health==77] <- NA
brfss$ment_health[brfss$ment_health==99] <- NA
brfss$ment_health[brfss$ment_health==88] <- 0
brfss$ment_health[brfss$ment_health<14] <- 0
brfss$ment_health[brfss$ment_health>13] <- 1
brfss$ment_health <- factor(brfss$ment_health, levels = 0:1, labels = c("<14 days", "14+ days"))

brfss$old_health_plan <- brfss$health_plan #creates a depreciated part1 variable
unique(brfss$health_plan)
brfss$health_plan[brfss$health_plan==7] <- NA
brfss$health_plan[brfss$health_plan==9] <- NA
brfss$health_plan[brfss$health_plan==2] <- 0
brfss$health_plan <- factor(brfss$health_plan, levels = 0:1, labels = c("No", "Yes"))

# recoding variables using dplyr, use recode
# there are specific things about numeric values as shown below, NA_integer_ is NA but as integer
# see https://dplyr.tidyverse.org/reference/recode.html for more information
brfss$old_children <- brfss$children #creates a depreciated part1 variable
brfss$children <- recode(brfss$children, `88` = 0L, `99` = NA_integer_)

brfss$old_adults_cell <- brfss$adults_cell #creates a depreciated part1 variable
brfss$adults_cell <- recode(brfss$adults_cell, `77` = NA_integer_, `99` = NA_integer_)

write.csv(brfss, "part2_brfss.csv")
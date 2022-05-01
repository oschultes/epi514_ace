# Association between ACEs and Cognitive Impairment
# Data cleaning Part 2
# EPI 514
# McKenzi Norris
# Created: April 29, 2022
# last updated: April 30, 2022

# Description: Read in Part 1's reweighted and combined data,
# Create backup Part 1 variables before recoding values for the outcome, exposure, and covariate variables, which will be
# Recoded and factored for missing, yes/no, and frequency responses with both 0/1 and 1/2 versions.

#import data
rm(list=ls())
brfss <- read.csv("reweighted_2019_2020.csv")
View(brfss)


#recode outcome
brfss$old_decide <- brfss$decide #depreciates part1 variable
unique(brfss$decide) #lists unique values to check for errors
brfss$decide[brfss$decide==7] <- NA
brfss$decide[brfss$decide==9] <- NA
  #1/2 Version
brfss$decide_12 <- brfss$decide
brfss$decide_12 <- factor(brfss$decide_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$decide[brfss$decide==2] <- 0
brfss$decide <- factor(brfss$decide, levels = 0:1, labels = c("No", "Yes"))


#recode exposure
brfss$old_ace_depres <- brfss$ace_depres #creates a depreciated part1 variable
unique(brfss$ace_depres)
  #1/2 Version
brfss$ace_depres_12 <- brfss$ace_depres
brfss$ace_depres_12[brfss$ace_depres_12==7] <- 2
brfss$ace_depres_12[brfss$ace_depres_12==9] <- 2
brfss$ace_depres_12 <- factor(brfss$ace_depres_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_depres[brfss$ace_depres==7] <- 0
brfss$ace_depres[brfss$ace_depres==9] <- 0
brfss$ace_depres[brfss$ace_depres==2] <- 0
brfss$ace_depres <- factor(brfss$ace_depres, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_drink <- brfss$ace_drink #creates a depreciated part1 variable
unique(brfss$ace_drink)
  #1/2 Version
brfss$ace_drink_12 <- brfss$ace_drink
brfss$ace_drink_12[brfss$ace_drink_12==7] <- 2
brfss$ace_drink_12[brfss$ace_drink_12==9] <- 2
brfss$ace_drink_12 <- factor(brfss$ace_drink_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_drink[brfss$ace_drink==7] <- 0
brfss$ace_drink[brfss$ace_drink==9] <- 0
brfss$ace_drink[brfss$ace_drink==2] <- 0
brfss$ace_drink <- factor(brfss$ace_drink, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_drugs <- brfss$ace_drugs #creates a depreciated part1 variable
unique(brfss$ace_drugs)
  #1/2 Version
brfss$ace_drugs_12 <- brfss$ace_drugs
brfss$ace_drugs_12[brfss$ace_drugs_12==7] <- 2
brfss$ace_drugs_12[brfss$ace_drugs_12==9] <- 2
brfss$ace_drugs_12 <- factor(brfss$ace_drugs_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_drugs[brfss$ace_drugs==7] <- 0
brfss$ace_drugs[brfss$ace_drugs==9] <- 0
brfss$ace_drugs[brfss$ace_drugs==2] <- 0
brfss$ace_drugs <- factor(brfss$ace_drugs, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_prison <- brfss$ace_prison #creates a depreciated part1 variable
unique(brfss$ace_prison)
  #1/2 Version
brfss$ace_prison_12 <- brfss$ace_prison
brfss$ace_prison_12[brfss$ace_prison_12==7] <- 2
brfss$ace_prison_12[brfss$ace_prison_12==9] <- 2
brfss$ace_prison_12 <- factor(brfss$ace_prison_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_prison[brfss$ace_prison==7] <- 0
brfss$ace_prison[brfss$ace_prison==9] <- 0
brfss$ace_prison[brfss$ace_prison==2] <- 0
brfss$ace_prison <- factor(brfss$ace_prison, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_divor <- brfss$ace_divor #creates a depreciated part1 variable
#brfss$ace_divor_12[brfss$ace_divor==8] <- NA
  #1/2 Version
brfss$ace_divor_12 <- brfss$ace_divor
brfss$ace_divor_12[brfss$ace_divor_12==7] <- 2
#brfss$ace_divor_12[brfss$ace_divor_12==8] <- 2
brfss$ace_divor_12[brfss$ace_divor_12==9] <- 2
brfss$ace_divor_12 <- factor(brfss$ace_divor_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_divor[brfss$ace_divor==7] <- 0
#brfss$ace_divor[brfss$ace_divor==8] <- 0
brfss$ace_divor[brfss$ace_divor==9] <- 0
brfss$ace_divor[brfss$ace_divor==2] <- 0
brfss$ace_divor <- factor(brfss$ace_divor, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_punch <- brfss$ace_punch #creates a depreciated part1 variable
  #1/2 Version
brfss$ace_punch_12 <- brfss$ace_punch
brfss$ace_punch_12[brfss$ace_punch_12==2] <- 3
brfss$ace_punch_12[brfss$ace_punch_12==1] <- 2
brfss$ace_punch_12[brfss$ace_punch_12==3] <- 1
brfss$ace_punch_12[brfss$ace_punch_12==7] <- 2
brfss$ace_punch_12[brfss$ace_punch_12==9] <- 2
brfss$ace_punch_12 <- factor(brfss$ace_punch_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_punch[brfss$ace_punch==1] <- 0
brfss$ace_punch[brfss$ace_punch==7] <- 0
brfss$ace_punch[brfss$ace_punch==9] <- 0
brfss$ace_punch[brfss$ace_punch==2] <- 1
brfss$ace_punch[brfss$ace_punch==3] <- 1
brfss$ace_punch <- factor(brfss$ace_punch, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_hurt <- brfss$ace_hurt #creates a depreciated part1 variable
  #1/2 Version
brfss$ace_hurt_12 <- brfss$ace_hurt
brfss$ace_hurt_12[brfss$ace_hurt_12==2] <- 3
brfss$ace_hurt_12[brfss$ace_hurt_12==1] <- 2
brfss$ace_hurt_12[brfss$ace_hurt_12==3] <- 1
brfss$ace_hurt_12[brfss$ace_hurt_12==7] <- 2
brfss$ace_hurt_12[brfss$ace_hurt_12==9] <- 2
brfss$ace_hurt_12 <- factor(brfss$ace_hurt_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_hurt[brfss$ace_hurt==1] <- 0
brfss$ace_hurt[brfss$ace_hurt==7] <- 0
brfss$ace_hurt[brfss$ace_hurt==9] <- 0
brfss$ace_hurt[brfss$ace_hurt==2] <- 1
brfss$ace_hurt[brfss$ace_hurt==3] <- 1
brfss$ace_hurt <- factor(brfss$ace_hurt, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_swear <- brfss$ace_swear #creates a depreciated part1 variable
  #1/2 Version
brfss$ace_swear_12 <- brfss$ace_swear
brfss$ace_swear_12[brfss$ace_swear_12==2] <- 3
brfss$ace_swear_12[brfss$ace_swear_12==1] <- 2
brfss$ace_swear_12[brfss$ace_swear_12==3] <- 1
brfss$ace_swear_12[brfss$ace_swear_12==7] <- 2
brfss$ace_swear_12[brfss$ace_swear_12==9] <- 2
brfss$ace_swear_12 <- factor(brfss$ace_swear_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_swear[brfss$ace_swear==1] <- 0
brfss$ace_swear[brfss$ace_swear==7] <- 0
brfss$ace_swear[brfss$ace_swear==9] <- 0
brfss$ace_swear[brfss$ace_swear==2] <- 1
brfss$ace_swear[brfss$ace_swear==3] <- 1
brfss$ace_swear <- factor(brfss$ace_swear, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_touch_receive <- brfss$ace_touch_receive #creates a depreciated part1 variable
  #1/2 Version
brfss$ace_touch_receive_12 <- brfss$ace_touch_receive
brfss$ace_touch_receive_12[brfss$ace_touch_receive_12==2] <- 3
brfss$ace_touch_receive_12[brfss$ace_touch_receive_12==1] <- 2
brfss$ace_touch_receive_12[brfss$ace_touch_receive_12==3] <- 1
brfss$ace_touch_receive_12[brfss$ace_touch_receive_12==7] <- 2
brfss$ace_touch_receive_12[brfss$ace_touch_receive_12==9] <- 2
brfss$ace_touch_receive_12 <- factor(brfss$ace_touch_receive_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_touch_receive[brfss$ace_touch_receive==1] <- 0
brfss$ace_touch_receive[brfss$ace_touch_receive==7] <- 0
brfss$ace_touch_receive[brfss$ace_touch_receive==9] <- 0
brfss$ace_touch_receive[brfss$ace_touch_receive==2] <- 1
brfss$ace_touch_receive[brfss$ace_touch_receive==3] <- 1
brfss$ace_touch_receive <- factor(brfss$ace_touch_receive, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_touch_give <- brfss$ace_touch_give #creates a depreciated part1 variable
  #1/2 Version
brfss$ace_touch_give_12 <- brfss$ace_touch_give
brfss$ace_touch_give_12[brfss$ace_touch_give_12==2] <- 3
brfss$ace_touch_give_12[brfss$ace_touch_give_12==1] <- 2
brfss$ace_touch_give_12[brfss$ace_touch_give_12==3] <- 1
brfss$ace_touch_give_12[brfss$ace_touch_give_12==7] <- 2
brfss$ace_touch_give_12[brfss$ace_touch_give_12==9] <- 2
brfss$ace_touch_give_12 <- factor(brfss$ace_touch_give_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_touch_give[brfss$ace_touch_give==1] <- 0
brfss$ace_touch_give[brfss$ace_touch_give==7] <- 0
brfss$ace_touch_give[brfss$ace_touch_give==9] <- 0
brfss$ace_touch_give[brfss$ace_touch_give==2] <- 1
brfss$ace_touch_give[brfss$ace_touch_give==3] <- 1
brfss$ace_touch_give <- factor(brfss$ace_touch_give, levels = 0:1, labels = c("No", "Yes"))

brfss$old_ace_sex <- brfss$ace_sex #creates a depreciated part1 variable
  #1/2 Version
brfss$ace_sex_12 <- brfss$ace_sex
brfss$ace_sex_12[brfss$ace_sex_12==2] <- 3
brfss$ace_sex_12[brfss$ace_sex_12==1] <- 2
brfss$ace_sex_12[brfss$ace_sex_12==3] <- 1
brfss$ace_sex_12[brfss$ace_sex_12==7] <- 2
brfss$ace_sex_12[brfss$ace_sex_12==9] <- 2
brfss$ace_ace_sex_12 <- factor(brfss$ace_sex_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$ace_sex[brfss$ace_sex==1] <- 0
brfss$ace_sex[brfss$ace_sex==7] <- 0
brfss$ace_sex[brfss$ace_sex==9] <- 0
brfss$ace_sex[brfss$ace_sex==2] <- 1
brfss$ace_sex[brfss$ace_sex==3] <- 1
brfss$ace_sex <- factor(brfss$ace_sex, levels = 0:1, labels = c("No", "Yes"))


#recoding covariates
brfss$old_sex <- brfss$sex #creates a depreciated part1 variable
  #1/2 Version
brfss$sex_12 <- brfss$sex
brfss$sex_12 <- factor(brfss$sex_12, levels = 1:2, labels = c("Male", "Female"))
  #0/1 Version
brfss$sex[brfss$sex==2] <- 0
brfss$sex <- factor(brfss$sex, levels = 0:1, labels = c("Female", "Male"))

brfss$old_income <- brfss$income #creates a depreciated part1 variable
unique(brfss$income)
brfss$income[brfss$income==77] <- NA
brfss$income[brfss$income==99] <- NA
  #1/2 Version
brfss$income_12 <- brfss$income
brfss$income_12[brfss$income_12<7] <- 2
brfss$income_12[brfss$income_12>6] <- 1
brfss$income_12 <- factor(brfss$income_12, levels = 1:2, labels = c("$50,000+", "<$50,000"))
  #0/1 Version
brfss$income[brfss$income<7] <- 0
brfss$income[brfss$income>6] <- 1
brfss$income <- factor(brfss$income, levels = 0:1, labels = c("<$50,000", "$50,000+"))

brfss$old_age_5yr_group <- brfss$age_5yr_group #creates a depreciated part1 variable
unique(brfss$age_5yr_group)
brfss$age_5yr_group[brfss$age_5yr_group==14] <- NA
brfss$age_5yr_group <- factor(brfss$age_5yr_group, levels = 1:13, labels = c("Age 18 to 24", "Age 25 to 29", "Age 30 to 34", "Age 35 to 39", "Age 40 to 44", "Age 45 to 49", "Age 50 to 54", "Age 55 to 59","Age 60 to 64", "Age 65 to 69", "Age 70 to 74", "Age 75 to 79", "Age 80 or older"))

brfss$old_age_65_plus <- brfss$age_65_plus #creates a depreciated part1 variable
unique(brfss$age_65_plus)
brfss$age_65_plus[brfss$age_65_plus==3] <- NA
  #1/2 Version
brfss$age_65_plus_12 <- brfss$age_65_plus
brfss$age_65_plus_12[brfss$age_65_plus_12==1] <- 3
brfss$age_65_plus_12[brfss$age_65_plus_12==2] <- 1
brfss$age_65_plus_12[brfss$age_65_plus_12==3] <- 2
brfss$age_65_plus_12 <- factor(brfss$age_65_plus_12, levels = 1:2, labels = c("Age 65 or older", "Age 18 to 64"))
  #0/1 Version
brfss$age_65_plus[brfss$age_65_plus==1] <- 0
brfss$age_65_plus[brfss$age_65_plus==2] <- 1
brfss$age_65_plus <- factor(brfss$age_65_plus, levels = 0:1, labels = c("Age 18 to 64", "Age 65 or older"))

brfss$old_race_eth <- brfss$race_eth #creates a depreciated part1 variable
unique(brfss$race_eth)
brfss$race_eth[brfss$race_eth==9] <- NA
brfss$race_eth <- factor(brfss$race_eth, levels = 1:8, labels = c("White", "Black", "American Indian or Alaskan Native", "Asian", "Native Hawaiian or other Pacific Islander", "Other race, non-Hispanic", "Multiracial, non-Hispanic", "Hispanic"))

brfss$old_education <- brfss$education #creates a depreciated part1 variable
unique(brfss$education)
brfss$education[brfss$education==9] <- NA
  #1/2 Version
brfss$education_12 <- brfss$education
brfss$education_12[brfss$education_12<6] <- 2
brfss$education_12[brfss$education_12==6] <- 1
brfss$education_12 <- factor(brfss$education_12, levels = 1:2, labels = c("4+ years of college", "<4 years of college"))
  #0/1 Version
brfss$education[brfss$education<6] <- 0
brfss$education[brfss$education==6] <- 1
brfss$education <- factor(brfss$education, levels = 0:1, labels = c("<4 years of college", "4+ years of college"))

brfss$old_employ <- brfss$employ #creates a depreciated part1 variable
unique(brfss$employ)
brfss$employ[brfss$employ==9] <- NA
  #1/2 Version
brfss$employ_12 <- brfss$employ
brfss$employ_12[brfss$employ_12<3] <- 1
brfss$employ_12[brfss$employ_12>2] <- 2
brfss$employ_12 <- factor(brfss$employ_12, levels = 1:2, labels = c("Employed", "Not employed"))
  #0/1 Version
brfss$employ[brfss$employ<3] <- 1
brfss$employ[brfss$employ>2] <- 0
brfss$employ <- factor(brfss$employ, levels = 0:1, labels = c("Not Employed", "Employed"))

brfss$old_ment_health <- brfss$ment_health #creates a depreciated part1 variable
unique(brfss$ment_health)
brfss$ment_health[brfss$ment_health==77] <- NA
brfss$ment_health[brfss$ment_health==99] <- NA
brfss$ment_health[brfss$ment_health==88] <- 0
  #1/2 Version
brfss$ment_health_12 <- brfss$ment_health
brfss$ment_health_12[brfss$ment_health_12<14] <- 2
brfss$ment_health_12[brfss$ment_health_12>13] <- 1
brfss$ment_health_12 <- factor(brfss$ment_health_12, levels = 1:2, labels = c("14+ days", "<14 days"))
  #0/1 Version
brfss$ment_health[brfss$ment_health<14] <- 0
brfss$ment_health[brfss$ment_health>13] <- 1
brfss$ment_health <- factor(brfss$ment_health, levels = 0:1, labels = c("<14 days", "14+ days"))

brfss$old_health_plan <- brfss$health_plan #creates a depreciated part1 variable
unique(brfss$health_plan)
brfss$health_plan[brfss$health_plan==7] <- NA
brfss$health_plan[brfss$health_plan==9] <- NA
  #1/2 Version
brfss$health_plan_12 <- brfss$health_plan
brfss$health_plan_12 <- factor(brfss$health_plan_12, levels = 1:2, labels = c("Yes", "No"))
  #0/1 Version
brfss$health_plan[brfss$health_plan==2] <- 0
brfss$health_plan <- factor(brfss$health_plan, levels = 0:1, labels = c("No", "Yes"))

write.csv(brfss, "part2_brfss.csv")
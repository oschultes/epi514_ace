
# data cleaning part I
# epi 514
# olivia schultes
# created: april 27 2022
# last updated: april 28 2022

# description: read in brfss data, get rid of unnecessary states/variables,
  # reweight and combine 2 years of data


# components
  # read in brfss data
  # choose relevant states & vars, reweight, combine 2019/2020 data, rename vars
  # chose & rename relevant vars 


# packages
library(survey)
library(haven)
library(dplyr)
library(data.table)

##########
# Read in BRFSS data (import SAS xport file)

# read in data from website, unzip files & resave as .csv
url19 = "https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip"
url20 = "https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip"

download.file(url19, "LLCP2019XPT.zip")
download.file(url20, "LLCP2020XPT.zip")

brfss19 = read_xpt(unzip("LLCP2019XPT.zip"))
brfss20 = read_xpt(unzip("LLCP2020XPT.zip"))

glimpse(brfss19)
glimpse(brfss20)

write.csv(brfss19, "LLCP2019.csv", row.names = FALSE)
write.csv(brfss20, "LLCP2020.csv", row.names = FALSE)

# Read in .csv files
brfss19 <- read.csv("LLCP2019.csv")
brfss20 <- read.csv("LLCP2020.csv")


#########
# Reweight data

obs_year = rbind(
  data.frame(cohort=2019, n=nrow(brfss19)),
  data.frame(cohort=2020, n=nrow(brfss20))
)

obs_year = obs_year %>%
  mutate(prop = n / sum(n))

# Make vector of states who administered ACE module in each year
# FIPS state codes
  # 2019: 1 (AL), 10 (DE), 12 (FL), 28 (MS), 45 (SC), 47 (TN), 51 (VA), 54 (WV)
  # 2020: 1 (AL), 11 (D.C.), 12 (FL), 13 (GA), 21 (KY), 28 (MS), 45 (SC), 48 (TX), 51 (VA)

ace2019 = c(1, 10, 12, 28, 45, 47, 51, 54)
ace2020 = c(1, 11, 12, 13, 21, 28, 45, 48, 51)

# make new dataframe with only states who administered ACE module
reduced19 = brfss19 %>%
  filter(X_STATE %in% ace2019)
reduced20 = brfss20 %>%
  filter(X_STATE %in% ace2020)

# check included states
unique(reduced19$X_STATE)
unique(reduced20$X_STATE)

# make vector with relevant variables
  # note 1: I included the extra variable "X_AGE65YR" because it will 
  # be a shortcut for the class project (less recoding)
  # note 2: we're only using one version, so we just need to consider var X_LLCPWT
  # there are other weighting variables used for alternative module versions 
  # i.e. X_LLCPWT2, but we don't need to use these
vars=c("X_PSU", "X_LLCPWT", "X_STSTR", "X_STATE", "DECIDE", 
       "ACEDEPRS", "ACEDRINK", "ACEDRUGS", "ACEPRISN", "ACEDIVRC", 
       "ACEPUNCH", "ACEHURT1", "ACESWEAR", "ACETOUCH", "ACETTHEM", 
       "ACEHVSEX", "SEXVAR", "INCOME2", "X_AGEG5YR", "X_AGE65YR", 
       "X_IMPRACE", "EDUCA", "EMPLOY1", "MENTHLTH", "HLTHPLN1")

# select relevant variables & create column with year
reduced19 = reduced19 %>%
  select(vars) %>%
  mutate(cohort = 2019)
reduced20 = reduced20 %>%
  select(vars) %>%
  mutate(cohort = 2020)

# merge 2019/2020 datasets
brfss = rbind(reduced19, reduced20)

# merge combined dataset with proportion of observations
brfss = merge(brfss, obs_year, by="cohort", all=T)

# adjust the weights by multiplying individual year weights by calculated proportions
brfss$svy_weight = brfss$X_LLCPWT * brfss$prop

# rename vars
# names(brfss) <- tolower(names(brfss))

oldnames = names(brfss)
newnames = c("year", "psu", "old_weight", "strat", "state", 
             "decide", "ace_depres", "ace_drink", "ace_drugs",
             "ace_prison", "ace_divor", "ace_punch", "ace_hurt",
             "ace_swear", "ace_touch_receive", "ace_touch_give",
             "ace_sex", "sex", "income", "age_5yr_group", "age_65_plus", 
             "race_eth", "education", "employ", "ment_health",
             "health_plan",  "year_obs", "year_weight", "svy_weight")
brfss %>% rename_with(.col = oldnames, ~ newnames)

# store new/old variable names in codebook
codebook = data.frame(brfss_names = oldnames, new_names = newnames)


# export reduced dataset as .csv
write.csv(brfss, "reweighted_2019_2020.csv", row.names = FALSE)


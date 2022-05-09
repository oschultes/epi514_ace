
# results & table 2
# epi 514
# bridget waters
# created: may 9 2022
# last updated: may 9 2022

# description: Mantel-Haenszel stratified analyses and tables for results

# be sure to run part2data.R and dataexplore_table1.R prior to running this R script

# components
  # recode variables in preparation for M-H analyses

# packages in addition to those loaded in part2data and dataexplore_table1
library(epiR)

# create a new race variable that combines race/ethnicity groups with small sample sizes
  # combines American Indian or Alaskan Native only,non-Hispanic with Other race only, non-Hispanic and Multiracial, non-Hispanic
  # combines Asian only, non-Hispanic with Native Hawaiian or Other Pacific Islander only, non-Hispanic
brfss$old_race_eth2 <- brfss$race_eth
brfss$race_eth <- as.numeric(brfss$old_race_eth2)
brfss$race_eth <- recode(brfss$race_eth, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 4L, `6` = 3L, `7` = 3L, `8` = 5L)
brfss$race_eth <- factor(brfss$race_eth, levels = 1:5, labels = c("White only, non-Hispanic",
                                                                  "Black only, non-Hispanic",
                                                                  "Other, non-Hispanic", 
                                                                  "Asian or Pacific Islander, non-Hispanic", 
                                                                  "Hispanic"))
summary(brfss$race_eth)
table(brfss$old_race_eth2, brfss$race_eth)

# create indicator exposure variables for bivariate comparisons
# variables need to be coded as 1/2 instead of 1/0 for epi.2by2

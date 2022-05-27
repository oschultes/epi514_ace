
# forest plot
# epi 514
# olivia schultes
# created: may 26 2022
# last updated: may 27 2022

# description: create forest plot from PRs


# components
  # read in PR data & process for plotting
  # create forest plot


# packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(wesanderson)


#########
# prep data for plotting

# read in data
pr = read.csv("prevalence_ratios.csv")

# remove variables not in table, recode labels, & add group variable for plotting
pr = pr %>%
  filter(!(covariate == "American Indian or Alaskan Native only, non-Hispanic" | 
             covariate == "Multiracial, non-Hispanic" |
             covariate == "Other race only, non-Hispanic" |
             covariate == "Asian only, non-Hispanic" |
             covariate == "Native Hawaiian or other Pacific Islander only, non-Hispanic" |
             covariate == "Not employed" |
             covariate == "Employed" |
             covariate == "0 days" |
             covariate == "1-14 days" |
             covariate == "14+ days")) %>%
  mutate(covariate = case_when(covariate == "White only, non-Hispanic" ~ "White NH",
                               covariate == "Black only, non-Hispanic" ~ "Black NH",
                               covariate == "Other, non-Hispanic" ~ "Other NH",
                               covariate == "Asian or Pacific Islander, non-Hispanic" ~ "Asian/PI NH",
                               covariate == "No" ~ "Not currently insured",
                               covariate == "Yes" ~ "Currently insured",
                               covariate == "Some college" ~ "Some higher education",
                               covariate == "College graduate" ~ "4+ years of higher education",
                               TRUE ~ covariate),
         group = factor(c("Overall", rep("Sex",2), rep("Age",6), rep("Race/\nEthnicity",5),
                          rep("Income",6), rep("Education\ncompleted",4), rep("Insurance\nstatus",2)),
                        levels = c("Overall", "Sex", "Age", "Race/\nEthnicity", "Income", "Education\ncompleted", "Insurance\nstatus")))
  
# convert covariate to ordered factor
pr = pr %>%
  mutate(covariate = factor(covariate,
                            levels = c("Overall", "Female", "Male", "18-39", "40-49", 
                                       "50-59", "60-69", "70-79", "80+", "White NH", "Black NH", 
                                       "Hispanic", "Asian/PI NH", "Other NH", "<$20K", "$20-35K", 
                                       "$35-50K", "$50-75K", "$75-100K", "$100K+", "Did not graduate high school",
                                       "High school graduate", "Some higher education", "4+ years of higher education", 
                                       "Not currently insured", "Currently insured"),
                            ordered=TRUE))


# gather values to long format, separate three values into columns and remove formatting
pr = pr %>%
  gather(key, value, c(ace_score_01, ace_score_02, ace_score_03, ace_score_04)) %>%
  separate(value, into = c("PR", "CI"), sep = "\\s", extra = "merge") %>%
  separate(CI, into = c("CI_lower", "CI_upper"), sep = "\\s", extra = "merge") %>%
  mutate(PR = as.numeric(PR),
         CI_lower = as.numeric(str_sub(CI_lower,2,-2)),
         CI_upper = as.numeric(str_sub(CI_upper,1,-2)))



###########
# plot

ggplot(data=pr, aes(x=covariate, y=PR, ymin=CI_lower, ymax=CI_upper, 
                    color=key)) + 
  geom_pointrange(fatten=.3,          # fatten changes size of dot but not line lol
                  position = position_dodge(width = .75)) +
  geom_hline(yintercept= 1, lty=2) +
  labs(x="") +
  facet_wrap(~group, ncol=1, scales = "free_y", strip.position = "left") +
  scale_color_discrete(name = "ACE score", labels = c("1", "2", "3", "4+")) +
  theme_minimal() +
  theme(strip.placement = "outside",
        axis.text = element_text(colour = "black")) +
  coord_flip()
  










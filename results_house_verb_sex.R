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
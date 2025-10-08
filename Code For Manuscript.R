set.seed(1234)
library(mediation)
library(tidyverse)
library(haven)
library(broom)
library(stats)
library(gt)
library(gtsummary)
library(sensmediation)
library(lme4)
library(survey)
library(kableExtra)



# hbsc <- import the data



## Focus on only the subpopulation that we will discuss (not province 12, or extreme grades)

data_weight_na <- hbsc |> select(grade, sex, A34_4, A30, id1, id2, id3, id4, wtcan, Race, A9, Urban_status, family, FAS_SESquintile) |> mutate(A30 = if_else(A30 == 1, 1, 0)) |>
  rename(academic_achievement = A30, 
         arts_participation = A34_4,
         immigration_status = A9) |> 
  filter((sex != 3) & (grade != 5 & grade != 11) & id1!=12) 



## Reweight for this subpopulation

data_weight_na$wtcan <- data_weight_na$wtcan / mean(data_weight_na$wtcan)


## Account for incomplete data by province, grade, and sex (all of which are completely observed, and which have no completely empty crosstabs)

original_props <- data_weight_na %>%
  group_by(id1, grade, sex) %>%
  summarise(
    total = n()                      # Total count of rows in each group
  )

cleaned_totals <- data_weight_no_na %>%
  group_by(id1, grade, sex) %>%
  summarise(
    total_clean = n()                      # Total count of rows in each group in cleaned data
  )


new_data_clean <- data_weight_no_na %>%
  left_join(original_props, by = c("id1", "grade", "sex")) %>%
  left_join(cleaned_totals, by = c("id1", "grade", "sex")) %>%
  mutate(
    # Calculate the new weights (w2) based on the population proportions from the original data
    w2 = total/total_clean,
    adjusted_weight = wtcan*w2
  )


new_data_clean <- new_data_clean %>%
  mutate(adjusted_weight = adjusted_weight/mean(adjusted_weight))







descriptive_hbsc <- new_data_clean %>%
  mutate(academic_achievement = factor(academic_achievement, levels = c(1,0), labels = c("Excellent", "Other")),
         arts_participation = factor(arts_participation, levels = c(1,2), labels = c("Yes", "No")),
         sex = factor(sex, levels = c(1:2), labels = c("Male", "Female")),
         Race = factor(Race, levels = c(1:8), labels = c("White", "Black", "Latin America", "Indigenous (First Nations, Metis, or Inuit)", "East and Southeast Asian", "East Indian and South Asian", "Arab and West Asian", "Other")),
         immigration_status = factor(immigration_status, levels = c(1:5), labels = c("I was born in Canada", "1 to 2 years", "3 to 5 years", "6 to 10 years", "11 or more years")),
         id1 = factor(id1),
         grade = factor(grade),
         Urban_status = factor(Urban_status, levels = c(1:4), labels = c("Rural area (< 1,000)", "Small population centre (1,000 to 29,999)", "Medium population centre (30,000 to 99,999)", "Large urban population centre (>= 100,000)")),
         family = factor(family, levels = c(1:6), labels = c("Mother and father", "Mother and partner", "Mother only", "Father and partner", "Father only", "Other")),
         FAS_SESquintile = factor(FAS_SESquintile, levels = c(1:5),  labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")))

table_summary_no_na <- descriptive_hbsc %>% dplyr::select(c(sex,grade, arts_participation,academic_achievement, FAS_SESquintile, Urban_status, Race, family, immigration_status, id1, id2, id3, adjusted_weight)) |> drop_na()

survey_design <- svydesign(
  ids = ~id2 + id3,    # Cluster variables
  strata =~id1,
  weights = ~adjusted_weight, # Sampling weights
  data = table_summary_no_na
)

mediation_hbsc <- new_data_clean %>%
  mutate(academic_achievement = factor(academic_achievement),
         arts_participation = ifelse(arts_participation == 2, 0, 1),
         sex = factor(ifelse(sex == 1, 0, 1)),
         Race = factor(Race, levels = c(1:8), labels = c("White", "Black", "Latin America", "Indigenous (First Nations, Metis, or Inuit)", "East and Southeast Asian", "East Indian and South Asian", "Arab and West Asian", "Other")),
         immigration_status = factor(immigration_status, levels = c(1:5), labels = c("I was born in Canada", "1 to 2 years", "3 to 5 years", "6 to 10 years", "11 or more years")),
         id1 = factor(id1),
         id2 = factor(id2),
         id3 = factor(id3),
         id4 = factor(id4),
         wtcan = wtcan,
         grade = factor(grade),
         Urban_status = factor(Urban_status, levels = c(1:4), labels = c("Rural area (< 1,000)", "Small population centre (1,000 to 29,999)", "Medium population centre (30,000 to 99,999)", "Large urban population centre (>= 100,000)")),
         family = factor(family, levels = c(1:6), labels = c("Mother and father", "Mother and partner", "Mother only", "Father and partner", "Father only", "Other")),
         FAS_SESquintile = factor(FAS_SESquintile, levels = c(1:5),  labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")))

data_model <- mediation_hbsc |> select(c(grade, sex, arts_participation, academic_achievement, id1, id2, id3, wtcan, w2, adjusted_weight, Race, immigration_status, Urban_status, family, FAS_SESquintile))




table_1 <- survey_design |>
  tbl_svysummary(
    include = -c(id1, id2, id3, adjusted_weight),
    label = list(
      grade = "Grade", 
      academic_achievement = "Academic Achievement", 
      arts_participation = "Arts Participation", 
      Race = "Racial or Cultural Background", 
      Urban_status = "Urban/rural status", 
      family = "Family Structure", 
      FAS_SESquintile = "Relative Family Affluence", 
      immigration_status = "Immigration Status"
    ),
    type = list(
      all_dichotomous() ~ "categorical",
      all_continuous() ~ "continuous2"
    ),
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)",   # unweighted n, weighted %
      all_continuous()  ~ "{mean} ({sd})"
    ),
    percent = "cell",
    digits = all_categorical() ~ c(0, 1),
    missing_text = "Missing"
  ) |>
  bold_labels()

table_1

table_2_by_arts_participation <- survey_design |>
  tbl_svysummary(
    by = arts_participation,
    include = c(sex, academic_achievement),
    type = list(
      all_dichotomous() ~ "categorical",
      all_continuous() ~ "continuous2"
    ),
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)",   # unweighted n, weighted %
      all_continuous()  ~ "{mean} ({sd})"
    ),
    percent = "cell",
    digits = all_categorical() ~ c(0, 1),
    missing_text = "Missing"
    ) |>
      bold_labels()

table_2_by_arts_participation

table_3_by_arts_participation <- survey_design |>
  tbl_svysummary(
    by = academic_achievement,
    include = c(sex, arts_participation),
    type = list(
      all_dichotomous() ~ "categorical",
      all_continuous() ~ "continuous2"
    ),
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)",   # unweighted n, weighted %
      all_continuous()  ~ "{mean} ({sd})"
    ),
    percent = "cell",
    digits = all_categorical() ~ c(0, 1),
    missing_text = "Missing"
  ) |>
  bold_labels()
  

table_3_by_arts_participation

### Mediation Analysis

#### Mediator Model
mediator_model <- glm(
  arts_participation ~ sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status, 
  data = data_model, 
  weights = data_model$adjusted_weight, 
  family = binomial(link = "probit"))

#### Outcome Model
outcome_model <- glm(
  academic_achievement ~ arts_participation + sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status, 
  data = data_model, 
  weights = data_model$adjusted_weight, 
  family = binomial(link = "probit"))


#### Combining Mediator and Outcome Model
mediation_model <- mediate(mediator_model, outcome_model, treat = "sex", mediator = "arts_participation", sims = 1000)

summary(mediation_model)


### Sensitivity Analysis

#### Mediator-Outcome Confounding
sensitivity_mediation <- sensmediation(mediator_model, outcome_model, type = "my", exp.name = "sex1", med.name = "arts_participation", Rho = seq(-0.9, 0.9, 0.1))

plot(sensitivity_mediation, ylab = "Average Causal Mediating Effect (ACME)")


#### Interaction Model

int_mediator_model <- glm(arts_participation ~ sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status, data = data_model, weights = data_model$adjusted_weight, family = binomial(link = "probit"))

int_outcome_model <- glm(academic_achievement ~ arts_participation * sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status, data = data_model, weights = data_model$adjusted_weight, family = binomial(link = "probit"))

int_mediation_model <- mediate(int_mediator_model, int_outcome_model, treat = "sex", mediator = "arts_participation", sims = 1000)

summary(int_mediation_model)


test.TMint(int_mediation_model, conf.level = .95)

#### Expanded Sets of Confounders

data_expanded_na <- updated_hbsc <- hbsc |> 
  mutate(behavioural = if_else(A20_3 == 1, 1, 0, missing = 0), 
         communicational = if_else((A20_2 == 1 | A20_5 == 1 | A20_6 == 1 | A20_8 == 1 | A20_9 == 1), 1, 0, missing = 0),
         intellectual = if_else((A20_7 == 1 | A20_11 == 1), 1, 0, missing = 0),
         physical = if_else((A20_10 == 1 | A20_4 == 1), 1, 0, missing = 0), 
         mental = if_else((A20_12 == 1), 1, 0, missing = 0)) |>
  mutate("multiple or others" = if_else(((behavioural == 1 & (communicational == 1 | intellectual == 1| physical == 1| mental == 1)) | (communicational == 1 & (intellectual == 1| physical == 1| mental == 1)) | (intellectual == 1 & (physical == 1|mental == 1)) | (physical ==1 & mental == 1) | A20_13 == 1), 1, 0, missing = 0)) |>
  select(grade, sex, A34_4, A30, id1, id2, id3, id4, wtcan, Race, A9, Urban_status, family, FAS_SESquintile, bullyvictim, behavioural, communicational, intellectual, physical, mental, `multiple or others`, LS1, LS2, A72_2, A72_2WE) |> mutate(A30 = if_else(A30 == 1, 1, 0)) |>
  rename(academic_achievement = A30, 
         arts_participation = A34_4,
         immigration_status = A9) |> 
  filter((sex != 3) & (grade != 5 & grade != 11) & id1!=12) 


## Reweight for this subpopulation

data_expanded_na$wtcan <- data_expanded_na$wtcan / mean(data_expanded_na$wtcan)

data_expanded_no_na <- data_expanded_na |> drop_na()

## Account for incomplete data by province, grade, and sex (all of which are completely observed, and which have no completely empty crosstabs)

expanded_original_props <- data_expanded_na %>%
  group_by(id1, grade, sex) %>%
  summarise(
    total = n()                      # Total count of rows in each group
  )


expanded_cleaned_totals <- data_expanded_no_na %>%
  group_by(id1, grade, sex) %>%
  summarise(
    total_clean = n()                      # Total count of rows in each group in cleaned data
  )


expanded_data_clean <- data_expanded_no_na %>%
  left_join(expanded_original_props, by = c("id1", "grade", "sex")) %>%
  left_join(expanded_cleaned_totals, by = c("id1", "grade", "sex")) %>%
  mutate(
    # Calculate the new weights (w2) based on the population proportions from the original data
    w2 = total/total_clean,
    adjusted_weight = wtcan*w2
  )



expanded_data_clean <- expanded_data_clean %>%
  mutate(adjusted_weight = adjusted_weight/mean(adjusted_weight))


expanded_data_clean <- expanded_data_clean %>%
  mutate(academic_achievement = factor(academic_achievement),
         arts_participation = ifelse(arts_participation == 2, 0, 1),
         sex = factor(ifelse(sex == 1, 0, 1)),
         Race = factor(Race, levels = c(1:8), labels = c("White", "Black", "Latin America", "Indigenous (First Nations, Metis, or Inuit)", "East and Southeast Asian", "East Indian and South Asian", "Arab and West Asian", "Other")),
         immigration_status = factor(immigration_status, levels = c(1:5), labels = c("I was born in Canada", "1 to 2 years", "3 to 5 years", "6 to 10 years", "11 or more years")),
         id1 = factor(id1),
         id2 = factor(id2),
         id3 = factor(id3),
         id4 = factor(id4),
         wtcan = wtcan,
         grade = factor(grade),
         bullyvictim = factor(bullyvictim),
         behavioural = factor(behavioural),
         communicational = factor(communicational),
         intellectual = factor(intellectual),
         physical = factor(physical),
         mental = factor(mental),
         `multiple or others` = factor(`multiple or others`),
         LS1 = factor(LS1),
         LS2 = factor(LS2),
         A72_2 = factor(A72_2),
         A72_2WE = factor(A72_2WE),
         Urban_status = factor(Urban_status, levels = c(1:4), labels = c("Rural area (< 1,000)", "Small population centre (1,000 to 29,999)", "Medium population centre (30,000 to 99,999)", "Large urban population centre (>= 100,000)")),
         family = factor(family, levels = c(1:6), labels = c("Mother and father", "Mother and partner", "Mother only", "Father and partner", "Father only", "Other")),
         FAS_SESquintile = factor(FAS_SESquintile, levels = c(1:5),  labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")))

  
##### Mediator Model

expand_mediator_model <- glm(arts_participation ~ sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status + bullyvictim + behavioural + communicational + intellectual + physical + mental + `multiple or others` + LS1 + LS2 + A72_2 + A72_2WE, data = expanded_data_clean, weights = expanded_data_clean$adjusted_weight, family = binomial(link = "probit"))

##### Outcome Model
  
expand_outcome_model <- glm(academic_achievement ~ arts_participation + sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status + bullyvictim + behavioural + communicational + intellectual + physical + mental + `multiple or others` + LS1 + LS2 + A72_2 + A72_2WE, data = expanded_data_clean, weights = expanded_data_clean$adjusted_weight, family = binomial(link = "probit"))


##### Combining Mediator and Outcome Model
  
expand_mediation_model <- mediate(expand_mediator_model, expand_outcome_model, treat = "sex", mediator = "arts_participation", sims = 1000)

summary(expand_mediation_model)



#### With random effects

##### Mediator Model
random_mediator_model <- glmer(arts_participation ~ sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status + (1|id2), control = glmerControl("bobyqa", optCtrl = list(maxfun = 1e5)), data = data_model, weights = data_model$adjusted_weight, family = binomial(link = "probit"))

  
##### Outcome Model
random_outcome_model <- glmer(academic_achievement ~ arts_participation + sex + grade + id1 + FAS_SESquintile + Urban_status + Race + family + immigration_status + (1 + arts_participation | id2), control = glmerControl("bobyqa", optCtrl = list(maxfun = 1e5)), data = data_model, weights = data_model$adjusted_weight, family = binomial(link = "probit"))


##### Combining Mediator and Outcome Model
  
random_mediation_model <- mediate(random_mediator_model, random_outcome_model, treat = "sex", mediator = "arts_participation", sims = 1000)

summary(random_mediation_model)




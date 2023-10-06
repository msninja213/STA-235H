social = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework3/data/social_insure_adap.csv")
nsw_rct = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework3/data/train_rct.csv")
nsw_obs = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework3/data/train_cps.csv")

library(tidyverse)
library(modelsummary)
library(estimatr)
library(MatchIt)

#data wrangling 
social = social %>%
  select(takeup_survey, age, agpop, educ, rice_inc, disaster_prob, male, 
         intensive, risk_averse, disaster, delay, network_obs, network_rate_presession) %>%
  mutate(takeup_survey = ifelse(takeup_survey == 'yes', 1, 0)) %>%
  mutate(disaster = ifelse(disaster == 'yes', 1, 0)) 

#Q6
xtabs(~intensive, data = social)
xtabs(~delay, data = social)

#Q7
social_bal = social %>%
  select(age, agpop, educ, rice_inc, disaster_prob, male, 
         intensive, risk_averse, disaster, delay, network_obs, network_rate_presession)

datasummary_balance(~intensive, data = social_bal, dinm_statistic = "p.value", fmt = 3)

datasummary_balance(~delay, data = social_bal, dinm_statistic = "p.value", fmt = 3)

#Q8
lm1 = lm_robust(takeup_survey ~ intensive, data = social)
summary(lm1)

#Q11
#first info session
social1 = social %>%
  filter(delay == 0)

lm_social1 = lm_robust(takeup_survey ~ intensive, data = social1)
summary(lm_social1)

#second info session
social2 = social %>%
  filter(delay == 1)

lm_social2 = lm_robust(takeup_survey ~ intensive, data = social2)
summary(lm_social2)

#Q14
social2 = social %>%
  filter(delay == 1)

lm_newsocial2 = lm_robust(takeup_survey ~ intensive + network_obs 
                          + network_rate_presession, data = social2)
summary(lm_newsocial2)

#Q16
xtabs(~treat, data = nsw_rct)
xtabs(~delay, data = social)

#Q17
rct_bal = nsw_rct %>%
  select(treat, age, education, black, hispanic, married, nodegree, re75)

datasummary_balance(~treat, data = rct_bal, dinm_statistic = "p.value", fmt = 3)

#Q18
#no lm_robust because re78 isnt binary
lm_rct = lm(re78 ~ treat, data = nsw_rct)
summary(lm_rct)

#Q19
lm_obs = lm(re78 ~ treat, data = nsw_obs)
summary(lm_obs)

#Q22
#data wrangling
nsw_obs = nsw_obs %>%
  mutate(unemployed75 = ifelse(re75 <= 0, 1, 0)) 

#FORMULA FOR PROPENSITY SCORE
obs_m1 = matchit(treat ~ age + education + black + hispanic + married 
                 + nodegree + re75 + unemployed75, data = nsw_obs,
                 method = "nearest", caliper = 0.3) 
summary(obs_m1)
obs_m1 = match.data(obs_m1) #MATCHED DATASET

#Q25
matched_lm = lm_robust(re78 ~ treat, data = obs_m1)  
summary(matched_lm)



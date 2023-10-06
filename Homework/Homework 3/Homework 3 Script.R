social = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework3/data/social_insure_adap.csv")

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
  select(takeup_survey, age, agpop, educ, rice_inc, disaster_prob, male, 
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







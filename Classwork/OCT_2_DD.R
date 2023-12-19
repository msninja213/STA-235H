library(tidyverse)
library(modelsummary)
library(estimatr)

netflix = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week7/1_DiffInDiff/data/netflix.csv")

#Q1:Create a binary variable treat for the states that were part of the pilot (1) and those that were not (0)
#Q2: Create a binary variable post that takes the value of 1 if the information was collected after the pilot rollout (May 2022), and 0 in another case.
netflix = netflix %>%
  mutate(treat = ifelse(state == 'CA' | state == 'NY', 1, 0)) %>%
  mutate(post = ifelse(survey == 'June2022' | survey == 'July2022', 1, 0))

#check data 
netflix %>%
  select(state, treat) %>%
  table()

netflix %>%
  select(survey, post) %>%
  table()

#checking for balance
netflix_bal = netflix %>%
  select(treat, income, employed, female)

#just treatment variable 
datasummary_balance(~treat, data = netflix_bal, dinm_statistic = "p.value", fmt = 3)
#income and employed are unbalanced. diff mean is high, and significant p-value (low)

#Q4: calculate the 4 averages 
netflix %>%
  group_by(treat, post) %>%
  summarize(mean(subscribed))

#a) What was the change in subscriptions before and after the pilot a) for CA and NY, and b) other states?
#For CA and NY:  0.635−0.543=0.092
#For other states: 0.515−0.488=0.027

#b) What is the difference in subscriptions between the treatment and the control group in a) April 2022, and b) July 2022?
#treat: NY/Cali vs other states
#post: when treatment was assigned 
#A) before pilot rollout (April). So it's  0.543 - 0.488 = 0.055
#B) After pilot rollout (July). So it's 0.635 - 0.515 = 0.12

#Q5 -- bc outcomes are binary
lm_dd = lm_robust(subscribed ~ treat*post, data = netflix)
summary(lm_dd)
#on average, when the pilot was implemented in NY and California, the rate of 
#subscriptions increased by 6.5 percentage points compared to not having the policy 

#ON AVERAGE, INTERVENTION, ONLY FOR CALIFORNOA AND NY 
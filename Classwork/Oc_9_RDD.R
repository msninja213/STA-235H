library(tidyverse)
library(rdrobust)

mlda = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week8/1_RDD/data/mlda_18_24.csv")

mlda = mlda %>%
  mutate(r = age_days - 7670) %>%
  mutate(treat = ifelse(age_days >= 7670, 1, 0))

#Q2 --> check discountinuity 
rdplot(
  y = mlda$treat, x = mlda$r, c = 0, 
  title = 'RD Plot', x.label = 'days since 21st birthday', y.label = 'treat',
)

#Q3 --> robustness check (smoothness of covariates across threshold)
rdplot(
  y = mlda$female, x = mlda$r, c = 0, 
  title = 'RD Plot', x.label = 'days since 21st birthday', y.label = 'proportion of female arrests',
)
#there is a discontinuity on the 21st birthday and around that time 
#looking at the actual proportion values, the discontinuity is actually very small
#arrests are pretty uniformly distributed. is it a problem? 

#Use RDRobust to check ^^
rd_female = rdrobust(
  y = mlda$female, x = mlda$r, c = 0
)
summary(rd_female)
#not an issue.p-value is insignificant (large p-value)

#Q4__> calculuate efect
lm_alcohol = lm(alcohol ~ treat*r, data = mlda)
summary(lm_alcohol)
#treatment: whether you can legally drink
#control: you cant drink
#cutoff: being exactly 21 years old
#on average, being legally able to drink decreases the number of alcohol arrests by 15.9
#compared to people who are not legally able to drink for people exactly 21 years old 

#compare treatment and control, then explain cutoff at the end 


#NEW CHANGES PUSHED PUSHED PUSHED 
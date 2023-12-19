#Causal inference packages
library(tidyverse)
library(modelsummary)
library(estimatr)
library(vtable)
library(MatchIt)
library(rdrobust)

#Prediction packages
library(caret)
library(rpart)
library(rattle)
library(rsample)
library(modelr)

#TASK 3  
oregon = read.csv("https://raw.githubusercontent.com/maibennett/website_github/master/exampleSite/content/results/data/OHIE_ed.csv")

#3.1) Create a balance table showing that the treated and control group are comparable. 
#Show your code (everything you need for creating the table after loading the data) 
#AND your table here.

#3.1:
#3.3a) Run a simple regression to assess the effect of healthcare access on being 
#employed 12 months later. Fill in the blank with the corresponding single line of code:
  
m1 = matchit(treatment ~ race_bl + birthyear_bl + female_bl + health_gen_bl + employ_bl + 
              hhinc_cat_bl + edu_bl + hhsize_bl + health_gen_fu + employ_fu + hhinc_cat_fu, data = oregon,
            method = "nearest", caliper = 0.2)

summary(m1)

d_m1 = match.data(m1)

d_m1 %>%
  select(treatment) %>%
  table ()

d_m1_bal = d_m1 %>%
  select(treatment, race_bl, birthyear_bl, female_bl, health_gen_bl, employ_bl, 
           hhinc_cat_bl, edu_bl, hhsize_bl, health_gen_fu, employ_fu, hhinc_cat_fu)

datasummary_balance(~treatment, data = d_m1_bal, fmt = 3, dinm_statistic = "p.value")

#3.3a) Run a simple regression to assess the effect of healthcare access on being 
#employed 12 months later. Fill in the blank with the corresponding single line of code:

lm_ohie = lm_robust(employ_bl ~ treatment, data = d_m1) #robust because employ is binary
summary(lm_ohie)

#######################################################################
#Task 3.2
oregon_obs = read.csv("https://raw.githubusercontent.com/maibennett/website_github/master/exampleSite/content/results/data/OHIE_ed_obs.csv")

m2 = matchit(medicaid ~ race_bl + birthyear_bl + female_bl + health_gen_bl + employ_bl + 
               hhinc_cat_bl + edu_bl + employ_fu, data = oregon_obs,
             method = "nearest")

summary(m2)

d_m2 = match.data(m2)

d_m2 %>%
  select(medicaid) %>%
  table ()

d_m2_bal = d_m2 %>%
  select(medicaid, race_bl, birthyear_bl, female_bl, health_gen_bl, employ_bl, 
         hhinc_cat_bl, edu_bl, employ_fu)

datasummary_balance(~medicaid, data = d_m2_bal, fmt = 3, dinm_statistic = "p.value")

#3.3a) Run a simple regression to assess the effect of healthcare access on being 
#employed 12 months later. Fill in the blank with the corresponding single line of code:

lm_ohie2 = lm_robust(employ_bl ~ medicaid, data = d_m2)
summary(lm_ohie2)



#################################################################
#TASK 4
ad = read.csv("https://raw.githubusercontent.com/maibennett/website_github/master/exampleSite/content/results/data/ad_data.csv")

#Task 4.1
set.seed(123)
split = initial_split(ad, prop = 0.75, strata = "clicks")
train.data = training(split)
test.data = testing(split)

train.data %>%
  summarize(mean(clicks))

#4.2: linear model (regression task)
lm_linear_model = lm(clicks ~ position + time_of_day + category + quality_rating + platform + age, data = ad)
summary(lm_linear_model)

rmse(lm_linear_model, test.data) #test model on testing data

#4.3A: Forward stepwise model 
set.seed(123)
lm.fwd = train(clicks ~.,
               data = ad,
               trControl = trainControl(method = "cv", number = 10),
               method = "leapForward",
               tuneGrid = data.frame(nvmax = 1:14))

lm.fwd$bestTune
lm.fwd$finalModel
rmse(lm.fwd, test.data)

varImp(lm.fwd, scale = TRUE)

#4.4A: LASSO
set.seed(123)
lasso = train(clicks ~.,
              data = ad,
              trControl = trainControl(method = "cv", number = 10),
              method = "glmnet",
              preProcess = "scale",
              tuneGrid = expand.grid(alpha = 1,
                                     lambda = seq(16, 18, by = 100)))

## Best tuning parameter
lasso$bestTune
lasso$finalModel

## Coefficients
coefs = coef(lasso$finalModel, lasso$bestTune$lambda)

coefs@p[2] - 1

rmse(lasso, test.data)

#4.5: Random Forest
tuneGrid = expand.grid(
  mtry = 1:10, # Number of random covariates that will test --> optimal is 10
  splitrule = "variance", # Split rule (Important: for regressions use "variance", for classification use "gini")
  min.node.size = 2 # Min observations in each node.
)

set.seed(123)
rf = train(clicks ~.,
           data = ad,
           trControl = trainControl(method = "cv", number = 10),
           method = "ranger",
           num.trees = 50,
           tuneGrid = tuneGrid)

plot(rf)

rf$finalModel
rf$bestTune

rmse(rf, test.data)
varImp(rf, scale = TRUE)


0.01+(2*0.005*12)

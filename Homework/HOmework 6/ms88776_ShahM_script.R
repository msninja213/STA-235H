################################################################################
### Title: "Homework 6 Submission"
### Course: STA 235H
### Semester: Fall 2023
### Name: Malaika Shah
################################################################################

# Clears memory
rm(list = ls())
# Clears console
cat("\014")

#Causal inference packages
library(tidyverse)
#library(xgboost)
library(dplyr)
library(caret)
#Prediction packages
library(modelr)
library(dplyr)

#run boosting parallel
library(parallel)
library(doParallel)

##########################################################################################################
##############                                                                              ##############
##############      TASK 1: predict the loan amount an individual gets (REGRESSION TASK)    ##############
##############                                                                              ##############
##########################################################################################################

##########################################################################################################
##############      1.1: CLEAN AND WRANGLE DATA                                             ##############      
##########################################################################################################

hmda_part1 = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework6/data/hmda_train_r.csv")

hmda_new = hmda_part1 %>%
  select(-as_of_year, -respondent_id, -agency_name, -agency_abbr, -loan_type_name,
         -loan_purpose_name, -loan_purpose, -owner_occupancy, -owner_occupancy_name, 
         -preapproval_name, -action_taken_name, -applicant_ethnicity_name, 
         -co_applicant_ethnicity_name, -applicant_race_name_1, -co_applicant_race_name_1, 
         -applicant_sex_name, -co_applicant_sex_name) %>%
  drop_na() %>%
  mutate(state_abbr = factor(state_abbr))


##########################################################################################################
##############      1.2: SPLIT DATA                                                         ##############      
##########################################################################################################

#for some reason, the slice function wasn't working on my code. I had to use a different 
#method to split the data. My friends also had an issue with the slice function not working
set.seed(100)
train_id = sample(1:nrow(hmda_new), 0.8 * nrow(hmda_new))

train.data = hmda_new %>% dplyr::slice(train_id)
test.data = hmda_new %>% dplyr::slice(-train_id) #clashes with xgboost package 

##########################################################################################################
##############      1.3: MODEL 1, BOOSTING (RMSE = 133.571)                                 ##############      
##########################################################################################################

cl = makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)

tuneGrid = expand.grid(
  n.trees = c(50, 100, 150, 500), # Number of trees that will test
  interaction.depth = c(1, 2), # Number of splits it will attempt
  shrinkage = c(0.01, 0.1), # Learning rates
  n.minobsinnode = 10 # Min observations in each node.
)

set.seed(100)

boost_reg = train(loan_amount_000s ~ .,
              data = train.data,
              trControl = trainControl(method = "cv", number = 10),
              method = "gbm",
              tuneGrid = tuneGrid)

stopCluster(cl)
registerDoSEQ()

boost_reg$finalModel

boost_reg$bestTune
rmse(boost_reg, test.data)


##########################################################################################################
##############      1.4: MODEL 2, LASSO (RMSE = 122.82)                                     ##############      
##########################################################################################################

set.seed(100)
lasso = train(loan_amount_000s ~ .,
              data = train.data,
              trControl = trainControl(method = "cv", number = 10),
              method = "glmnet",
              preProcess = "scale",
              tuneGrid = expand.grid(alpha = 1,
                                     lambda = seq(0, 0.4, by = 0.001)))

## Best tuning parameter
lasso$bestTune

lasso$finalModel

## Coefficients
coefs = coef(lasso$finalModel, lasso$bestTune$lambda)

coefs@p[2] - 1

## Accuracy
rmse(lasso, train.data)


# Preferred regression model
preferred_reg_model <- train(
  loan_amount_000s ~ .,
  data = hmda_new,
  method = "glmnet",  # Use the method you prefer
  tuneGrid = expand.grid(alpha = 1,
                         lambda = seq(0, 0.4, by = 0.001))
)

##########################################################################################################
##############                                                                              ##############
##############    TASK 2: predict if they get loan approval or not (CLASSIFICATION TASK)    ##############
##############                                                                              ##############
##########################################################################################################

##########################################################################################################
##############      2.1: CLEAN AND WRANGLE DATA                                             ##############      
##########################################################################################################

hmda_part2 = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework6/data/hmda_train_c.csv")

hmda_new2 = hmda_part2 %>%
  select(-as_of_year, -respondent_id, -agency_name, -agency_abbr, -loan_type_name, 
         -loan_purpose_name, -owner_occupancy_name, -preapproval_name, -applicant_ethnicity_name,
         -co_applicant_ethnicity_name, -applicant_race_name_1, -co_applicant_race_name_1, -applicant_sex_name, 
         -co_applicant_sex_name) %>%
  drop_na() %>%
  mutate(state_abbr = factor(state_abbr))

##########################################################################################################
##############      2.2: SPLIT THE DATA                                                     ##############      
##########################################################################################################

#SPLITITNG DATA
set.seed(100)
train_id = sample(1:nrow(hmda_new2), 0.8 * nrow(hmda_new2))

train.data = hmda_new2[train_id, ]
test.data = hmda_new2[-train_id, ]
  
##########################################################################################################
##############      2.3: MODEL 1, CLASSIFICATION TREE (accuracy is 80.11%)                ##############      
##########################################################################################################

set.seed(100)

ct = train(
  factor(action_taken_name) ~., #only use factor for binary outcome variable, not continous 
  data = train.data, #fit hyperparameter on training data set 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10), #cross validation 
  tuneLength = 20
)
plot(ct)

set.seed(100)
ct = train(factor(action_taken_name) ~.,
                  data = train.data,
                  method = "rpart", 
                  trControl = trainControl("cv", number = 10), 
                  tuneGrid = expand.grid(cp = seq(0, 0.002, length = 5)))

pred.values = ct %>%
  predict(test.data)

mean(pred.values == test.data$action_taken_name)
           

##########################################################################################################
##############      2.4: MODEL 2, BOOSTING (83.05% accuracy) (preferred model)               ##############      
##########################################################################################################

cl = makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)

tuneGrid_class = expand.grid(
  n.trees = c(50, 100, 150, 200), # Number of trees that will test
  interaction.depth = c(1, 2), # Number of splits it will attempt
  shrinkage = c(0.01, 0.1), # Learning rates
  n.minobsinnode = 10 # Min observations in each node.
)

set.seed(100)
boost_class = train(factor(action_taken_name) ~ ., 
                    data = train.data,
                    method = 'gbm',  # We are using extreme gradient boosting,
                    tuneGrid = tuneGrid_class, # Use the correct tuneGrid
                    trControl = trainControl("cv", number = 10))

stopCluster(cl)
registerDoSEQ()

# Final Model information
boost_class$finalModel

# Best Tuning parameters?
boost_class$bestTune

pred.values.boost = boost_class %>% predict(test.data)
mean(pred.values.boost == test.data$action_taken_name)


#preferred model
preferred_class_model <- train(
  factor(action_taken_name) ~ .,
  data = train.data,
  method = "gbm",
  tuneGrid = tuneGrid_class,
  trControl = trainControl("cv", number = 10),
)

save(preferred_reg_model, preferred_class_model, file = "C:/Users/msnin/OneDrive/Documents/STA-235H/Homework/HOmework 6/ms88776_ShahM_models.RData")




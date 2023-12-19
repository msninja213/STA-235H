# Clears memory
rm(list = ls())
# Clears console
cat("\014")

### Load libraries
# If you don't have one of these packages installed already, you will need to 
#run install.packages() line
library(tidyverse)
library(caret)
library(modelr)
library(rpart)
library(rattle)
library(rsample) #This helps us divide our data by strata

# AMAZON - REGRESSION
amz = read.csv("https://raw.githubusercontent.com/maibennett/website_github/master/exampleSite/content/files/data/amz_luggage.csv")
amz %>% select(-title) %>% head(.)

# QUESTION 1
# what's the difference between best selling luggage and non-best selling luggage with 4.5 stars? stars is the coefficient * number of stars 
# running 2 different regressions basically. For non-best sellers, isBestSeller = 0. Price = intercept + stars*4.5 = 103
# For best sellers, isBestSeller = 1. Price equals intercept + isBestSeller + stars*4.5 + interaction = 147
# just equals isBestSeller + interaction * 4.5= 29.4236 + 11.5*4.5 = 81

# QUESTION 2
# what's the interaction between prices and stars?
# = stars + isBestSeller:stars = 4.95 + 11.5092 = 16.46

# QUESTION 3
# what is the interpretation of the association between prices and stars?
# on average, a 1 star increase for a BEST-SELLING luggage increases price by $16.46, holding all other variables constant 



# CAUSAL INFERENCE - ACADEMIC PROBATION
probation = read.csv("https://raw.githubusercontent.com/maibennett/website_github/master/exampleSite/content/files/data/probation.csv")

# QUESTION 4A:
#   what is the percentage of students that drop out of school who are not on academic probabtion?
#just the intercept - 0.037 + 0.071 * 0 = 0.037

# QUESTION 4B:
#   what is the percentage of students that drop out of school who are on academic probation?
#   just the intercept + coefficient - 0.037 + 0.071 * 1 = 0.108
 
# QUESTION 4C: 
#   is the difference in dropout rates statistically significant? 
#   --> yes, just asking if the interaction term is statistically significant? yes it is
   
# QUESTION 4D:
#   interpret the probation coefficient:
#     on average, students on academic probation have a 7.2% higher probability of 
#     dropping out than students not on academic probation, holding all other 
#     variables constant (binary variable needs contrast) 

# QUESTION 5:
#   interpret the coefficient of interest 
#     on average, students on probation their first year have a 0.282 point higher increase 
#     in their GPA their second year compared to students not on probation their first year

# QUESTION 6:
#   is this a causal effect?
#     not a causal effect because there are unobservable confounders that we have not accounted for 
#     for example, we might not have accounted for vulnerable students more likely to be on academic probation


###### PREDICTION 

# Clears memory
rm(list = ls())
# Clears console
cat("\014")

### Load libraries
library(tidyverse)
library(caret)
library(rpart)
library(rattle)
library(rsample)
library(ranger)

candy = read.csv("https://raw.githubusercontent.com/maibennett/website_github/master/exampleSite/content/files/data/candy_r.csv")

# We will use a seed of 100 and a cross-validation of 10-fold.
set.seed(100)

candy = candy %>% select(-competitorname)
candy = candy %>% mutate(popularity = factor(popularity, levels = c("Low", "Average", "High")))

split = initial_split(candy, prop = 0.9, strata = "winpercent")
train.data = training(split)
test.data = testing(split)

set.seed(100)

# Single decision tree

dt = train(winpercent ~ ., data = train.data,
           method = "rpart",
           tuneGrid = expand.grid(cp = seq(0,0.1,length = 100)),
           trControl = trainControl(method = "cv", number = 10))

fancyRpartPlot(dt$finalModel, caption = "Decision Tree for Candy")

#use RMSE to determine how well the model did because this is a regression task 
rmse(dt, test.data)


# Random forest

tuneGrid = expand.grid(
  mtry = 1:13,
  splitrule = "variance",
  min.node.size = 5
)
set.seed(100)
rf = train(winpercent ~ ., data = train.data,
           method = "ranger",
           num.trees = 100,
           tuneGrid = tuneGrid,
           importance = "permutation",
           trControl = trainControl(method = "cv", number = 10))
rf$bestTune
rmse(rf, test.data) %>% round(., 2)


#HW 6 STUFF
##########################################################################################################
##############      MODEL 3: FORWARD STEPWISE SELECTION (rmse = 178)                        ##############      
##########################################################################################################

set.seed(100) # Set a seed for replication

train.control = trainControl(method = "cv", number = 10) #set up a 10-fold cv

lm.fwd = train(loan_amount_000s ~ ., data = train.data, # We take out unsubscribe because it's also an outcome (happens *after* logins)
               method = "leapForward", # "leapForward" is for Forward Stepwise and "leapBackward" is for backwards.
               tuneGrid = data.frame(nvmax = 1:5), #We include 5 variables, because that's all the predictors we are using for our model.
               trControl = train.control) 

lm.fwd$results

# We can see the number of covariates that is optimal to choose:
lm.fwd$bestTune

# And how does that model looks like:
summary(lm.fwd$finalModel)

# If we want the RMSE
rmse(lm.fwd, test.data)


##########################################################################################################
##############      MODEL 4: REGRESSION TREE (RMSE = 141)                                   ##############      
##########################################################################################################

set.seed(100)

#first try setting tune length to 20 to find the best neighborhood for CP  
rt = train(
  loan_amount_000s ~., #only use factor for binary outcome variable, not continous 
  data = train.data, #fit hyperparameter on training data set 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10), #cross validation 
  tuneLength = 20
)

# Use this line of code to plot your performance against the complexity parameter
plot(rt)

# Now do the same thing again, but using tuneGrid instead of tuneLength because we know what neighborhood our tune should be around 

#now run with tune grid, knowing that best complexity parameter is around 0-0.02
set.seed(100) #need to reset seed 
rt = train(
  loan_amount_000s ~., 
  data = train.data, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid= expand.grid(cp = seq(0, 0.025, length = 100))
)
plot(rt)

#find best tuning parameter 
rt$bestTune
rmse(rt, test.data) 













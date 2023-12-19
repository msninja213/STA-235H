library(tidyverse)
library(caret)
library(modelr)

marketing = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week10/1_ModelSelection/data/marketing.csv")

# For simplicity, we will drop some variables, but you can use them all afterward if you want!
marketing = marketing %>% select(Customer, Customer.Lifetime.Value, Response, 
                                 Coverage, Education, EmploymentStatus,
                                 Gender, Income, Location.Code, Marital.Status, 
                                 Monthly.Premium.Auto, 
                                 Months.Since.Last.Claim, Number.of.Policies,
                                 Policy.Type, Sales.Channel, Total.Claim.Amount, 
                                 Renew.Offer.Type)

#Q1: Let’s prepare the data. Drop Customer from your dataset (we will not use it for prediction), 
#and transform Response into a binary variable (1 if Response is Yes and 0 in another case).
marketing = marketing %>%
  select(- Customer) %>%
  mutate(Response = ifelse(Response == 'Yes', 1, 0)) 

marketing %>% select(Response) %>% table()

#Q3: Now we are ready to roll. Using set.seed(100), create a training dataset that 
#is 75% of your data, and a testing dataset that is 25% of your data. 
set.seed(100)

n = nrow(marketing)
train = sample(x = 1:n, size = n*0.75) 

train.data = marketing %>% slice(train) #use only the rows that were selected for training
test.data = marketing %>% slice(-train) #the rest are used for testing

nrow(test.data) #Number of rows (observations) in our training dataset

#Q4: Run a linear regression using all your covariates as predictors and Customer.Lifetime.Value 
#as the outcome (make sure you exclude Response as well). Fit model on training set, get RMSE on the testing dataset
lm_clv <- lm(Customer.Lifetime.Value ~. - Response, data = train.data) #COMPLETE THE MODEL. NOTE: Pay special attention to what data you should be using
rmse(lm_clv, test.data) 

#Q5: Run the same model, but now use a cross-validation approach with 5-folds. 
#In this case, you can use the entire dataset for this.
set.seed(100)

train.control = trainControl(method = "cv", number = 5) #COMPLETE

lm_clv_cv = train(Customer.Lifetime.Value ~. -Response, data = marketing, method="lm", trControl = train.control)
lm_clv_cv

# Compare models:
coef(lm_clv)
coef(lm_clv_cv$finalModel) #coef with linear model with cv

#Q6: Finally, let’s do a (forward) Stepwise selection model (using again a 5-fold CV). 
#Remember that you need to identify how many predictors we have in this case 
#(and factor variables count for more than 1 depending on the number of levels!):
set.seed(100)

nvars = length(lm_clv_cv$coefnames) #This could be one way of doing it... Q: How many variables do we have (max)?

train.control = trainControl(method = "cv", number = 5) #COMPLETE

lm.fwd = train(Customer.Lifetime.Value ~. -Response, data = train.data, 
               method = "leapForward",
               tuneGrid = data.frame(nvmax = 1:nvars), 
               trControl = train.control)

lm.fwd$bestTune #This is the number of variables we will be using

lm.fwd #(same as lm.fwd$results) 

summary(lm.fwd$finalModel)
rmse(lm.fwd, test.data)

# If we want to recover the coefficient names, we can use the coef() function:
coef(lm.fwd$finalModel, lm.fwd$bestTune$nvmax)












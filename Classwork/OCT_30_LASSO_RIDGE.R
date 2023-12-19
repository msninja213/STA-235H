library(tidyverse)
library(caret)
library(modelr)

#fitting binary variables 

# Clears memory
rm(list = ls())
# Clears console
cat("\014")

marketing = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week10/1_ModelSelection/data/marketing.csv")

# For simplicity, we will drop some variables, but you can use them all afterward if you want!
marketing = marketing %>% select(Customer, Customer.Lifetime.Value, Response, Coverage, Education, EmploymentStatus,
                                 Gender, Income, Location.Code, Marital.Status, Monthly.Premium.Auto, 
                                 Months.Since.Last.Claim, Number.of.Policies,
                                 Policy.Type, Sales.Channel, Total.Claim.Amount, Renew.Offer.Type)

marketing = marketing %>% select(-Customer) %>% mutate(Response = ifelse(Response=="No", 0, 1))

marketing = marketing %>% mutate_if(is.character, as.factor)

set.seed(100) #Remember always to set seed before something random (in this case, sample())

n = nrow(marketing) #Number of obs in our marketing dataset

train = sample(1:n, n*0.75) #Row numbers for 75% of our data (randomly selected)

train.data = marketing %>% slice(train)
test.data = marketing %>% slice(-train)

#RIDGE REGRESSION 
lambda_seq = seq(0,0.5,length = 100) 
set.seed(100)

train.control = trainControl(method = "cv", number = 10) #cross validating over 10 folds (dividing data into 10 sets for the model to iterate over)

ridge = train(factor(Response) ~ . , data = train.data, 
                method = "glmnet",
              preProcess = "scale", 
              trControl = train.control, #code above 
              tuneGrid = expand.grid(alpha = 0,
                                     lambda = lambda_seq)) #expand.grid creates a matrix with all 0s as alpha and lamda sequence as lamda 

plot(ridge)
ridge$bestTune #to find the lamda value that has the highest accuracy 

# Predict accuracy of model:
pred.values = ridge %>% predict(test.data)
show(pred.values)
# How many do we get right? (on average)
mean(pred.values == test.data$Response) #if predicated value = actual response, you get a 1. what's the percentage accuracy? 
#number of variables a ridge has is literally just the number of variables 

#LASSO
lambda_seq = seq(0,0.5,length = 100) 
set.seed(100)

train.control = trainControl(method = "cv", number = 10)
lasso = train(factor(Response) ~ . , data = train.data, 
              method = "glmnet",
              preProcess = "scale", 
              trControl = train.control, #code above 
              tuneGrid = expand.grid(alpha = 1, #ALPHA = 1 FOR LASSO
                                     lambda = lambda_seq)) #expand.grid creates a matrix with all 0s as alpha and lamda sequence as lamda 
plot(lasso)
lasso$bestTune

# Predict response:
pred.values = lasso %>% predict(test.data)
show(pred.values)
# How many do we get right? (on average)
mean(pred.values == test.data$Response) #if predicated value = actual response, you get a 1. what's the percentage accuracy? 


# You can also store them in an object, and then look at p!
# This is the number of coefficients (variables + intercept)
coefs = coef(lasso$finalModel, lasso$bestTune$lambda)
coefs@p[2] - 1

#for binary lasso and ridge, compare the accuracy of the models (want to use ridge in this case) 


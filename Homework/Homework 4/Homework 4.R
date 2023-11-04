#TASK 2 
texas = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework4/data/texas10college.csv")

library(tidyverse)
library(caret)
library(modelr)
library(estimatr)
library(rdrobust)

#Q7-8
texas = texas %>% 
  mutate(r = 0.1-ranking, 
         treat = ifelse(r<0,0,1))

xtabs(~treat, data=texas)

#Q9
rdplot(y = texas$college, x = texas$r, c=0,
       title = "RD Plot to Check For Discontinuity", x.label = "Ranking (distance from being in the top 10%)", y.label = "College Attendance")

#Q11
summary(rdrobust(texas$hh_income, texas$r, c = 0))

#Q15
lm_rd = lm(earnings ~ treat*r, data = texas)
summary(lm_rd)

#Q18
summary(rdrobust(texas$earnings, texas$r, c = 0))


#TASK 3 
airbnb <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework4/data/airbnb.csv")

airbnb <- airbnb %>%
  select(-address, -last_modified, -latitude, -longitude, -location, -name, -currency, -rate_type) %>%
  filter(!is.na(reviews) & !is.na(overall_satisfaction))

#Q22
set.seed(112)
n = nrow(airbnb) # Will tell us how many observations we have
train = sample(x = 1:n, size = n*0.7)

# slice() selects rows from a dataset based on the row number.
train.data = airbnb %>% slice(train) #use only the rows that were selected for training

test.data = airbnb %>% slice(-train) #the rest are used for testing

#Q23
nrow(test.data) 
nrow(train.data)

#Q24
test.data %>%
  summarise(avg_reviews=mean(reviews))

#Q25
lm1 = lm(price ~ accommodates, data = train.data) #Train the model on the TRAINING DATASET
rmse(lm1, test.data) #rmse() in the `modelr` package takes model we are using and the data.

lm2 = lm(price ~ reviews + overall_satisfaction, data = train.data) #Train the model on the TRAINING DATASET
rmse(lm2, test.data) #rmse() in the `modelr` package takes model we are using and the data.

#Q29
lm3 = lm(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms + bathrooms, data = train.data) #using all covariates
rmse(lm3, test.data)

#Q32
# We will typically use 5 or 10-fold cross validation
set.seed(112) # Set seed for replication!
train.control = trainControl(method = "cv", number = 10) #This is a function from the package caret. We are telling our data that we will use a cross validation approach (cv) with 10 folds (number). Use ?trainControl to see the different methods we could use!
lm4 = train(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms + bathrooms, data = airbnb, method="lm",
                     trControl = train.control) #See that here (in the train function), we just pass all the data. The function will divide it in folds and do all that!
lm4 #find coefs

#Q34
set.seed(112) # Set a seed for replication
train.control = trainControl(method = "cv", number = 10) #set up a 10-fold cv
nvars = length(lm4$coefnames)

lm.bwd = train(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms + bathrooms, data = train.data, # We take out unsubscribe because it's also an outcome (happens *after* logins)
               method = "leapBackward", 
               tuneGrid = data.frame(nvmax = 1:nvars), #We include 5 variables, because that's all the predictors we are using for our model.
               trControl = train.control) 

lm.bwd


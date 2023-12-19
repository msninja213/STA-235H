library(tidyverse)
library(caret)
library(modelr)

# You will need to install these two packages:
library(rpart)
library(rattle)

marketing = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week10/1_ModelSelection/data/marketing.csv")

# For simplicity, we will drop some variables, but you can use them all afterward if you want!
marketing = marketing %>% select(Customer, Customer.Lifetime.Value, Response, Coverage, Education, EmploymentStatus,
                                 Gender, Income, Location.Code, Marital.Status, Monthly.Premium.Auto, 
                                 Months.Since.Last.Claim, Number.of.Policies,
                                 Policy.Type, Sales.Channel, Total.Claim.Amount, Renew.Offer.Type)

marketing = marketing %>% 
  select(-Customer) %>% 
  mutate(Response = ifelse(Response=="No", 0, 1))

marketing = marketing %>% 
  mutate_if(is.character, as.factor)

#PREDICTING CUSTOMER LIFETIME VALUE. REGRESSION TREE 
set.seed(100) #Remember always to set seed before something random (in this case, sample())

n = nrow(marketing) #Number of obs in our marketing dataset

train = sample(1:n, nrow(marketing)*0.75) #Row numbers for 75% of our data (randomly selected)

train.data = marketing %>% 
  slice(train)
test.data = marketing %>% 
  slice(-train)

#Q5: Best Complexity parameter?
set.seed(100)

#first try setting tune length to 20 to find the best neighborhood for CP  
rt = train(
  Customer.Lifetime.Value ~. - Response, #only use factor for binary outcome variable, not continous 
  data = train.data, #fit hyperparameter on training data set 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5), #cross validation 
  tuneLength = 20
)

# Use this line of code to plot your performance against the complexity parameter
plot(rt)

# Now do the same thing again, but using tuneGrid instead of tuneLength because we know what neighborhood our tune should be around 

#now run with tune grid, knowing that best complexity parameter is around 0-0.02
rt = train(
  Customer.Lifetime.Value ~. - Response, 
  data = train.data, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid= expand.grid(cp = seq(0, 0.025, length = 100))
)
plot(rt)

#find best tuning parameter 
rt$bestTune
rmse(rt, test.data) #use RMSE to judge because regression task 

# You can also plot the tree:
fancyRpartPlot(rt$finalModel, caption = "Regression Tree for Customer Lifetime Value")

##########################################################################################
#new task: predict response (classification task)
#). Fit a regression tree, starting with tuneLength = 50 to find the best neighborhood 
#for your complexity parameter cp (plot your complexity parameter), and then use the argument 
#tuneGrid to find the best model that you can. Use a 5-fold CV approach, and use a set.seed(100). 
#What is the best complexity parameter in this case? How does your model perform?

set.seed(100)

#first try setting tune length to 50 to find the best neighborhood for CP  
ct = train(
  factor(Response) ~., #only use factor for binary outcome variable, not continous 
  data = train.data, #fit hyperparameter on training data set 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5), #cross validation 
  tuneLength = 50
)
plot(ct)


set.seed(100) #reset seed 
#rerun to find tuning param, given we know it's around 0-0.01
ct = train(
  factor(Response) ~., 
  data = train.data, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid= expand.grid(cp = seq(0, 0.01, length = 100))
)
plot(ct)

#find best tuning parameter 
ct$bestTune

#determine accuracy of tree
pred.values = ct %>% predict(test.data)
mean(pred.values == test.data$Response) 

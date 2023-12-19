library(modelr)
library(tidyverse)

set.seed(100) #selects the same training set over and over

n = nrow(hbo)

train = sample(1:n, n=0.8) #training set = 80% of dataset 

train.data = hbo


library(tidyverse)
library(modelr) 

hbo <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week10/1_ModelSelection/data/hbomax.csv")  
set.seed(123) #Always set seed for replication! 

n <- nrow(hbo) 

train <- sample(1:n, n*0.8) #randomly select 80% of the rows 

train.data <- hbo %>% slice(train) 
test.data <- hbo %>% slice(-train) 

lm_simple <- lm(logins ~ succession + city, data = train.data) 
summary(lm_simple) 

rmse(lm_simple, test.data)
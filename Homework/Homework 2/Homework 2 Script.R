library(tidyverse)
library(mosaic)
library(vtable)
library(dplyr)
library(estimatr)
library(modelsummary)

airbnb = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework2/data/airbnb_reviews.csv")

#Q6
airbnb <- airbnb %>% 
  distinct(city, .keep_all = TRUE)

result <- airbnb %>%
  count(city)

#Q7
airbnb <- airbnb %>%
  filter(city == 'New York' & price != 0 & review_scores_rating != 'NA' & minimum_nights < 30 & room_type != 'Hotel room')

#Q8
ggplot(airbnb)+
  geom_histogram(aes(x= log(price)), bins = 35, color = 'pink')+
  theme_minimal()+
  labs(x = 'Log of Nightly Airbnb Price',
       y = 'Count of Airbnbs',
       title = 'Log Price of Airbnb Nightly Price')

#Q9
airbnb <- airbnb%>%
  mutate(outlier_price = ifelse(log(price) > 8, 1, 0)) 
  
xtabs(~outlier_price, data = airbnb)
View(airbnb)

#Q11
airbnb <- airbnb %>% 
  mutate(log_price = log(price))

lm1 = lm(log_price ~ review_scores_rating + num_reviews + minimum_nights + accommodates + room_type, data = airbnb)
summary(lm1)

#Q13
ggplot(data = airbnb, aes(x = num_reviews, y = log(price))) + geom_point(color = "blue") + 
  geom_smooth(aes(color = "Linear Model"), method = "lm", se = FALSE) + 
  geom_smooth(aes(color = "Quadratic Model"), method = "lm", formula = y ~ x + I(x^2), se = FALSE) + 
  theme_minimal() + 
  labs(x = "Number of Reviews", y = "Log of Price per Night ($)", title = "Scatter Plot between Log of Nightly Price and Number of Reviews") + 
  scale_color_manual(values = c("purple", "orange"), name = "Regression Line")

#Q14
lm_avginc = lm(formula= log(price) ~ review_scores_rating + num_reviews + minimum_nights + accommodates + room_type + I(num_reviews^2), data = airbnb)
summary(lm_avginc)

#task 3
bank = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework2/data/bank.csv")

#Q16
bank <- bank %>% 
  mutate( balance = balance / 1000, deposit = ifelse(deposit == "yes",1,0), 
          tertiary_edu = ifelse(education == "tertiary", 1,0), default = ifelse(default == "yes", 1, 0), 
          any_loan = ifelse( housing == "yes" | loan == "yes", 1,0))

lm2 = lm_robust(deposit ~ age + tertiary_edu + balance + default + any_loan + campaign , data=bank)
summary(lm2)

#Q17
unique(bank$education)
unique(bank$default)
unique(bank$campaign)
unique(bank$housing)
unique(bank$loan)

bank = bank %>% mutate(tertiary_educ = ifelse(education == "tertiary", 1, 0)) %>% mutate(any_loan = ifelse(loan == "yes", 1, 0))
view(bank)

lm4 = lm_robust(deposit ~ age + tertiary_educ + balance + default + any_loan + campaign, data = bank)
summary(lm4)

#Q20
lm5 = lm_robust(deposit ~ age + tertiary_educ + balance + default + any_loan + campaign + balance:any_loan, data = bank)
summary(lm5)



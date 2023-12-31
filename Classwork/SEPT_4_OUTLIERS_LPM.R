######################################################################
### Title: "Week 3 - Outliers and Linear Probability Models"
### Course: STA 235H
### Semester: Fall 2023
### Professor: Magdalena Bennett
#######################################################################

# Clears memory
rm(list = ls())
# Clears console
cat("\014")
# scipen=999 removes scientific notation; scipen=0 turns it on.
options(scipen = 0)

### Load libraries
# If you don't have one of these packages installed already, you will need to run install.packages() line
library(tidyverse)
library(vtable)
library(AER) #package that includes some interesting data
library(estimatr) #package to run linear regressions with robust SE

################################################################################
######################## In-Class Exercise #####################################
################################################################################

###################### OUTLIERS ################################################

### HMDA Example

# This is the data from 2017 HMDA for Bastrop county (https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=tx&records=first-lien-owner-occupied-1-4-family-records&field_descriptions=labels)
# (you can also find the whole dataset for Austin by changing the name of the file to hmda_2017_austin.csv)

loans <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week3/2_OLS_Issues/data/hmda_2017_austin_bastrop.csv", stringsAsFactors = FALSE)

# You can find information about the variables here: https://files.consumerfinance.gov/hmda-historic-data-dictionaries/lar_record_codes.pdf

# Let's look at loans that were approved (action_taken = 1) 
# for home purchase (loan_purpose = 1) (hint: you will need to subset your data)
loans = loans %>%
  filter(action_taken == 1 & loan_purpose == 1)

# Q: How could we see if we have outliers? Create a histogram of loan_amount_000s
ggplot(data = loans, aes(x= loan_amount_000s))+
  geom_histogram(color = 'pink', fill = 'lightpink', bins = 50)+
  theme_minimal() + xlab('Loan Amount in Thousands of Dollars')

# Show a scatter plot of loan amount vs applicant's income:
ggplot(data = loans, aes(x = applicant_income_000s, y = loan_amount_000s))+
  geom_point(color = 'pink')+
  theme_minimal()+ #this sets the scale of the axis according to the data
  xlab('Applicant income in Thousands of Dollars')+ 
  ylab('Loan Amount in Thousands of Dollars')

# Fit a regression line to the previous plot:
ggplot(data = loans, aes(x = applicant_income_000s, y = loan_amount_000s))+
  geom_point(color = 'pink')+
  theme_minimal()+ #this sets the scale of the axis according to the data
  xlab('Applicant income in Thousands of Dollars')+ 
  ylab('Loan Amount in Thousands of Dollars')+
  geom_smooth(method = 'lm', se = FALSE, color = 'pink')

# Fit a regression line but *excluding* the clear outliers for income
loans = loans %>%
  mutate(outliers_income = ifelse(applicant_income_000s > 700, 1, 0))

loans %>% 
  select(outliers_income)%>%
  table()

loans_without_outliers = loans %>%
  filter(outliers_income ==0)

ggplot(data = loans, aes(x = applicant_income_000s, y = loan_amount_000s))+
  geom_point(color = 'pink')+
  theme_minimal()+ #this sets the scale of the axis according to the data
  xlab('Applicant income in Thousands of Dollars')+ 
  ylab('Loan Amount in Thousands of Dollars')+
  geom_smooth(method = 'lm', se = FALSE, color = 'pink')+
  geom_smooth(data = loans_without_outliers, aes(x = applicant_income_000s, y = loan_amount_000s), 
              method = 'lm', se = FALSE, color = 'purple') #doing this overlays the regressions 

# Q: Run a regression with and without outliers. Do your results change qualitatively?
lm_w_outliers = lm(loan_amount_000s ~ applicant_income_000s, data = loans)
lm_without_outliers = lm(loan_amount_000s ~ applicant_income_000s, data = loans_without_outliers)

###################### LINEAR PROBABILITY MODELS ###############################

data(HMDA) # This dataset is loaded from the AER package

# To know what the variables are, you can type ?HMDA on the console
head(HMDA)

#1) Use ifelse to set it to 1 if deny is "yes" and 0 in another case
HMDA = HMDA %>% #category to numeric
  mutate(deny_num = ifelse(deny == 'yes', 1, 0))

HMDA = HMDA %>%
  mutate(deny = as.numeric(deny)-1)

lm_deny1 = lm(deny ~ pirat + afam, data = HMDA)
summary(lm_deny1)

HMDA %>%
  select(deny) %>%
  table()

HMDA %>%
  select(deny_num) %>%
  table()

## Linear Probability Model (LPM)

# Q: Run a LPM using deny (the numeric version) as the outcome, and pirat (payment to income ratio), 
# chist (credit history), single, hschool (high school diploma),
# insurance, and race as the covariates.

lm_deny = lm_robust(deny_num ~ pirat + chist + single + hschool + afam + insurance, data = HMDA)
summary(lm_deny)  
#on average holding all other variables constant, a 1 unit increase in payment to income ratio (pirat) is 
#associated with a 48% pp increase in the probability of getting the loan denied 

#or, on average holding all other variables constant, a 1 unit increase in the payment to income ratio 
#increases the a probabilty of getting denied by 0.48.

#high school: if someone has a hs diploma vs someone without one, on average, they have a 12 pp less likely chance to get their loan denied than someone without a hs diploma
#if someone has a hs diploma vs someone without one, on average, they have a 0.12 less probabilty of getting their loan denied, holding all other variables constant 


lm_deny2 = lm_robust(deny_num ~ pirat + afam, data = HMDA)
summary(lm_deny2) #conclusion doesn't change with robust, but the SE changes
  
# Q: interpret the coefficient for pirat, hschool, and afam.
  
  
  #################### EXERCISE ON YOUR OWN ######################################

## Ames Housing dataset: Data for the housing market in Ames, Iowa.
## You can check the codebook here: https://sta235.com/Classes/Week3/2_OLS_Issues/data/ames_codebook.csv

housing <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week3/2_OLS_Issues/data/AmesHousing.csv")

# Only keep single family housing: (Bldg.Type)
housing <- housing %>% filter(Bldg.Type=="1Fam")

# Create 1) a histogram for Lot Area and 2) a scatter plot between SalePrice (y) and lot area (x). 
# Q) How many outliers (in terms of lot area) do you have? 
ggplot(data = housing, aes(x= Lot.Area))+
  geom_histogram(color = 'pink', fill = 'lightpink', bins = 50)+
  theme_minimal() + xlab('Lot Area')

ggplot(data = housing, aes(x= SalePrice, y = Lot.Area))+
  geom_point(color = 'pink', fill = 'lightpink')+
  theme_minimal() + 
  xlab('Sale Price')+
  ylab('Lot Area')

# Run a regression with your entire data between Sale Price, Lot Area, Year Built, and Bedrooms above ground. 
# Q: What is the association between sale price and lot area in this model?
lm1 = lm(SalePri)

lm_all = lm() # COMPLETE
summary(lm_all)

# Run the same regression as before, but exclude the outliers (in terms of lot area)
# Q: What is the association between sale price and lot area in this model? Is it the similar as before?

lm_wo_outliers = lm() # COMPLETE
summary(lm_wo_outliers)


# Create a dummy variable (price500) that takes the value of 1 if the sale price is greater than $500,000 and 0 in another case
housing = housing %>% # COMPLETE
  
  # Run a regression with price500 as the outcome, and lot area, number of bedrooms above ground, year built, 
  # overall quality of the materials, and pool area as covariates.
  
  lm_price = #COMPLETE
  
  # Q: Interpret the coefficient for `Overall.Qual`
  # Q: Run the same regression as before, but use lm() instead of lm_robust(). Is there our change in the coefficient for Lot.Area?
  # Should we use lm() or lm_robust() then? Or it doesn't matter?
######################################################################
### Title: "Week 4 - Multiple Regression: Polynomials"
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

################################################################################
### In-class exercises
################################################################################

## Ames Housing dataset: Data for the housing market in Ames, Iowa.
## You can check the codebook here: https://sta235.com/Classes/Week3/2_OLS_Issues/data/ames_codebook.csv

housing = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week3/2_OLS_Issues/data/AmesHousing.csv")

# Only keep single family housing: (Bldg.Type)
housing = housing %>% filter(Bldg.Type=="1Fam")

# Concentrate only on
# lot units that are under 20,000 sqft:

housing = housing %>% filter(Lot.Area<20000)

# Q1: Create a scatter plot between SalePrice (Y) and Lot.Area (X) and
# fit a linear model. Complete the following code:

ggplot(data = housing, aes(x = Lot.Area, y= SalePrice))+ #COMPLETE THIS LINE
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple4") +
  theme_minimal()

# Q2: Create a scatter plot between SalePrice (Y) and Lot.Area (X) and
# fit a quadratic model between Area and Price. Complete the following code:

# In this case, formula = y ~ x + I(x^2) is including the quadratic line!
ggplot(data = housing, aes(x = Lot.Area, y= SalePrice)) + #COMPLETE THIS LINE
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "purple4") +
  theme_minimal()

# Q3: Fit a regression of SalePrice, Lot.Area, and Lot.Area^2.
# Interpret the coefficient for Lot.Area:
lm_quad = lm(SalePrice ~ Lot.Area + I(Lot.Area^2), data = housing)
summary(lm_quad)

# Note: To include a quadratic term in a formula, you
# wrap the variable in I(): e.g. I(Lot.Area^2).

# What is the association between Price and Area
# for an increase of 10,000 to 10,001 sqft?
2.598e+01 + (2*-5.770e-04*10000)

#Increasing SQFT from 10,000 to 10,0001 results in a $14.438 increase in price (didn't take log of this function)

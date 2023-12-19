##########################################################
### Title: "Week 1 - Simple Linear Regression"
### Course: STA 235H
### Semester: Fall 2023
### Professor: Magdalena Bennett
##########################################################

# Clears memory
rm(list = ls())
# Clears console
cat("\014")

### Load libraries
# If you don't have one of these packages installed already, you will need to run install.packages() line
library(tidyverse)
library(ggplot2)

################################################################################
### In-class exercises
################################################################################

# Load data
rawData <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Classes/Week1/2_OLS/data/bechdel.csv")

# Select movies post 1990
bechdel <- rawData %>% 
  filter(Year>1989)

# Passes Bechdel test:

## 1) Create a binary variable called bechdel test that takes the value "PASS" if the rating == 3, and "FAIL" otherwise.
bechdel <- bechdel %>% mutate(bechdel_test = ifelse(rating==3, "PASS", "FAIL"),
                              Adj_Revenue = Adj_Revenue/10^6,
                              Adj_Budget = Adj_Budget/10^6)

# Q: Why do we divide revenue and budget by 10^6? Does it change our results and how?

# Let's run some models

lm_simple <- lm(Adj_Revenue ~ bechdel_test, data = bechdel)

summary(lm_simple) 
# Q: Recover the coefficient and interpret it
# On average, movies that pass the Bechdel test have an adjusted revenue that is -17 million dollars less than a movie that doesn't pass the Bechdel test.

# Now include other covariates (like budget, metascore, and imdb rating)

lm_multi <- lm(Adj_Revenue ~ bechdel_test + Adj_Budget + Metascore + imdbRating, data = bechdel)#... complete
summary(lm_multi)
# Q: Show the results of your new model. Interpret the coefficients.
#on average, movies that pass the bechdel test have an adjusted revenue that is 11 million dollars higher than movies that do not pass the bechdel test

#QImagine now that you have an hypothesis that Bechdel movies also get more bang 
#for their buck, e.g. they get more revenue for an additional dollar in their budget

lm_interaction <- lm(Adj_Revenue ~ bechdel_test*Adj_Budget + Metascore + imdbRating, data = bechdel)
summary(lm_interaction)
#Q. What is the association between budget and revenue for movies that pass the Bechdel test?
#there is a 7.55 million dollar increase 

#Q. What is the difference in the association between budget and revenue for movies 
#that pass vs movies that don't pass the Bechdel test?
#0.05 million, is not statistically significant 


##############################################################
### Title: Bootcamp code
### Author: Magdalena Bennett
### Date Created: 08/23/2023
### Last edit: [08/23/2023] - Created code
##############################################################

#Clear memory
rm(list = ls())

#Clear the console
cat("\014")

#Turn off scientific notation (turn back on with 0)
options(scipen = 999)

# Load packages
library(tidyverse) #includes dplyr and ggplot2!
library(vtable)
library(ggplot2)

# If there is a package you don't have installed, you can use install.packages("tidyverse")
# Only run once! (no need to install packages every time you run your code)

# Load data (this is loading data directly from Github)
sales = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/bootcamp/data/US_Regional_Sales_Data.csv")

## Inspecting your data

# Exercise 1: Let's explore the data. How many variables and observations do we have? What type of variables do we have? 
# 16 variables, factorous variables 

# Exercise 2: Install the package vtable, load it, and run the code sumtable(sales). What do you get? Use the ?sumtable to see the options for this function.
sumtable(sales)
#some mean missing values, no values for std, min, IQR 


## Data wrangling
# - data wrangling functions: 
# 1) mutate(var = var1 + var2)
# 2) filter(var == 1)
# 3) group_by(var1, var2)
# 4. select(var1, var2)
# 5) rename(var_new = var_old)

# Exercise 1: Unit cost and unit price should be numeric. Let's change this! (hint: you can use the function gsub() to replace "," for "", and as.numeric() to transform a variable!).
## Keep the same names for the variables and the dataset.
sales = sales %>%
  mutate(unit_cost = gsub(",", "", unit_cost)) %>%
  mutate(unit_price = gsub(",", "", unit_price)) %>%

sales = sales %>%
  mutate(unit_cost = as.numeric(unit_cost)) %>%
  mutate(unit_price = as.numeric(unit_price)) %>%

# Exercise 2: What are the different values for the sales channel in this dataset? Use the function table() to see!
## Create a new dataset for in-store and online sales. Call it "sales_min". How many variables do we have?
sales %>%
  select(sales_channel) %>%
  table 

sales_min = sales %>%
  filter(sales_channel %in% c("In-Store", "Online"))


# Exercise 3: Use the original dataset "sales", and create a new variable called "minority", 
## which takes the value of 1 if the sales channel is in-store or online, and 0 in another case.

  

# Exercise 4: What is the average price for sales made through a minority channel vs a non-minority channel?




## Plotting data!

# Exercise 1: Create a scatter plot between unit cost (x axis) and unit price (y axis)
ggplot(data = sales, aes(x = unit_cost, y = unit_price)) +
  geom_point(color = "deepskyblue") + 
  theme_minimal() + 
  xlab("Unit Cost (USD)") + ylab("Unit Price (USD)")
  

# Exercise 2: Now, let's make that plot pretty. Use theme_minimal() to get rid of the grey background. Color the points with the color "deepskyblue3",
## and change the axis titles to something more informative (e.g. Unit price ($)). This can be done with xlab() and ylab().


# Exercise 3: Using the same code as before, now we want to color observations based off type of the sales channel.
## Write some code that does that (e.g. you will need to change your aesthetics!)
ggplot(data = sales, aes(x = unit_cost, y = unit_price, color = sales_channel)) +
  geom_point() + 
  theme_minimal() + 
  xlab("Unit Cost (USD)") + ylab("Unit Price (USD)")

# Exercise 4: Finally, using the same code as in exercise 2, include a regression line in this plot using geom_smooth().



## Regressions

# Let's load a new dataset: The Gapminder

gapminder = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/bootcamp/data/gapminder.csv")

# Exercise 1: What type of data do we have?
#numeric and categorical 

# Exercise 2: Transform population into millions (divide pop by 10^6), and then regress life expectancy on gdp per capita and population. What do you obtain?
gapminder = gapminder %>%
  mutate(pop = pop/10^6) %>%
  lm1 = lm(lifeExp ~ gdpPercap + pop, data = gapminder)

summary(lm1)

# Exercise 3: Include now continent in the previous regression. Do your results change? How does it look when you include a factor variable in a regression?



## Bringing everything together

# Exercise 1: Create a new variable called gdpPercap_log, which is the logarithm of the GDP per capita. Now plot life expectancy against the log(GDP per capita),
## and describe the relationship.


# Exercise 2: Using the same plot as before, now color the points by continent and make the size proportional by population (in millions).


# Exercise 3: Do the same thing as before (exercise 2), but only for Europe!


# Exercise 4: Finally, run a regression that helps you estimate the association between life expectancy and GDP per capita, conditional on population, 
## for the year 2007 and then, another regression for the year 1982.


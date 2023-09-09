library(tidyverse)
library(mosaic)

#Task 2
grad = read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/Homework1/data/college_grads.csv")

#Question 1
nrow(grad)
ncol(grad)

#Question 2
grad <- grad %>%
  group_by(Major_category) %>%
  summarize(COUNT = n()) %>%
  
grad <- grad %>%
  mutate(ShareWomen = Women/Total, 
         Total = Total/100,
         Women = Women/100,
         Men = Men/100) 

  favstats(~Low_wage_jobs, data = grad)

#Question 11 
new_grad <- grad %>%
  filter(Major_category == 'Biology & Life Science')
  mean(df.)
  favstats(~ShareWomen, data = new_grad)
  #answer is 0.5871929 
  
new_grad <- grad %>%
  filter(Major_category == 'Engineering')
  favstats(~ShareWomen, data = new_grad)
  #answer is 0.2388887 
  
#Question 12
ggplot(data = grad)+
  geom_point(aes(x=ShareWomen, y=Median))+
             labs(x = 'Proportion of Women per Major',
                  y = 'Median Earnings for Full Time Workers',
                  title = 'Proportion of Women vs Median Earnings for Full Time Workers')

lm2 <- lm(Full_time ~ ShareWomen, data = grad)
summary(lm2)  

#Question 13
grad <- grad %>%
  mutate(MathMajor = ifelse(Major_category == 'Computers & Mathematics' | Major_category == 'Engineering',
         yes = 1, no = 0)) %>%
  mutate(ShareCollegeJobs = College_jobs/Total)

#Q14
xtabs(~MathMajor, data = grad)

#Q15
lm3 <- lm(Median ~ Total + Women + ShareCollegeJobs + MathMajor, data = grad)
summary(lm3)
  
#Question 17 + 19
lm4 <- lm(Median ~ Total + ShareCollegeJobs + Women*MathMajor, data = grad)
summary(lm4)

#Question 20
lm5 <- lm(Median ~ Women*MathMajor, data = grad)
summary(lm5)

#Question 22
ggplot(grad)+
  geom_histogram(aes(x= Full_time), bins = 25)+
  labs(x = 'Earnings for Full Time Workers',
       y = 'Frequency',
       title = 'Median Earnings of Full Time Workers')

Earnings <- grad$Full_time
log_Earnings <- log(Earnings)

ggplot(grad)+
  geom_histogram(aes(x= log_Earnings), bins = 25)+
  labs(x = 'Log Earnings for Full Time Workers',
     y = 'Frequency',
     title = 'Median Log Earnings of Full Time Workers')

#Question 23
Median <- grad$Median
log_Median <- log(Median)
lm6 <- lm(log_Median ~ Total + ShareCollegeJobs + MathMajor * Women, data = grad)
summary(lm6)


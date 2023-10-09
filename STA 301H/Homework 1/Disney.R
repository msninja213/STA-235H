#1 - Disney+
library(tidyverse)

#Part A
ggplot(data = disney)+
  geom_boxplot(aes(y= logins))+
  facet_wrap(~unsubscribe)+
  labs(
    title = "Distribution of Logins Based Off Customer Subscription Status",
    x = "Subscription Status",
    y = "Count")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
        )

#Part B
ggplot(data = disney)+
  geom_histogram(aes(age))+
  facet_wrap((~mandalorian))+
  labs(
    title = "Distribution of Age For Subscribers Who Viewed The
Mandalorian vs Who Did Not",
    x = "Age",
    y = "Count"
  )

#Part C
ggplot(data = disney)+
  #geom_bar(aes(unsubscribe, fill = mandalorian), color = 'black')+
  geom_bar(aes(mandalorian, fill = mandalorian), color = 'black')+
  facet_wrap(~unsubscribe)+
  labs(
    title = "Subscription Status of Customers Who Viewed The Mandalorian
vs Those Who Did Not",
    x = "Viewed The Mandalorian",
    y = "Count"
  )

#Part D
#1
xtabs(~mandalorian, data = disney) %>%
  prop.table

#2
xtabs(~unsubscribe, data = disney) %>%
  prop.table

#3
#probability that unsubscribed given seen mandalorian 
p_mandalorian = 0.707
p_unsubscribe_mandalorian = 0.2356
p_unsubscribe_mandalorian / p_mandalorian

#4
#probability that mandalorian given unsubscribed 
p_unsubscribed = 0.2704
p_unsubscribe_mandalorian = 0.2356
p_unsubscribe_mandalorian / p_unsubscribed

#5
xtabs(~unsubscribe + mandalorian, data = disney) %>%
  prop.table








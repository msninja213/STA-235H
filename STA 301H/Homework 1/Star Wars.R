#Star Wars Characters
library(tidyverse)
require(mosaic)

favstats(~height, data=SW)

#Part A, z-score
mean = 172.8675
sd = 36.29042
(66 - mean)/ sd

#Part B
mean = 172.8675
sd = 36.29042
(1.52*sd) + mean

#Part C
ggplot(data = SW)+
  geom_histogram(aes(x = mass), binwidth = 15)+
  labs(
    title = "Distribution of Mass",
    x = "Mass",
    y = "Count")
  

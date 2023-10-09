library(tidyverse)
library(ggplot2)


ggplot(power_christmas.1, aes(x=hour,y=ERCOT))+
  geom_line()

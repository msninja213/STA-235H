library(tidyverse)

# power_christmas.1 <- transform(power_christmas.1,               
#                 Year = format(date),
#                 DayOfYear = as.numeric( format(date)),
#                 ERCOT = c(diff(ERCOT),NA))

ggplot(data = power_christmas.1)+
  geom_line(aes(x=hour, y=ERCOT))+
      facet_wrap(~date)
  

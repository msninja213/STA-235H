#Part 3, Bigmac
library(tidyverse)
library(mosaic)

bigmac = bigmac %>%
  filter(dollar_ex > 0) %>%
  mutate(price_usd = local_price/dollar_ex)

bigmac = bigmac %>% mutate(year = lubridate::year(date))

avg_bigmac = bigmac %>%
  group_by(name, year) %>%
  summarize(avg_usd = mean(price_usd, na.rm=TRUE))

ggplot(data = avg_bigmac)+
  geom_histogram(aes(x = avg_usd), binwidth = 0.2)+
  labs(
    title = "Distribution of Price",
    x = "Price",
    y = "Count")

favstats(~avg_usd, data=avg_bigmac)

library(tidyverse)

#KC 6, P(plays Franz Ferdinand)
xtabs(~franz.ferdinand, data = plays_top50) %>%
  prop.table%>%
  round(3)

#P(plays Bob Dylan | plays the Beatles)
xtabs(~bob.dylan + the.beatles, data= plays_top50) %>%
  prop.table(margin=2) %>%
  round(3)

#P(plays Coldplay or plays Muse)
xtabs(~coldplay + muse, data= plays_top50) %>%
  prop.table%>%
  round(3)
library(xml2)
library(tidyverse)

## ---- plot_xml_temperature
instrumental_temperature %>% 
  filter(year < 1939) %>% 
  gather(key = key, value = temperature, -year) %>%
  ggplot(aes(x = year, y = temperature, colour = key)) +
  geom_line()

instrumental %>% full_join(instrumental_temperature) %>% 
  ggplot(aes(x = year, y = Aug - old)) + geom_point()

instrumental %>% full_join(instrumental_temperature) %>%
  ggplot(aes(x = old, y = Aug)) + 
  geom_point() + 
  geom_abline()

recon %>% 
  full_join(instrumental_temperature) %$% 
  cor(temperature, old)

recon %>% 
  full_join(instrumental_temperature) %$% 
  cor(temperature, new)

## ---- temperature_correlation
all_correlation <- recon %>%
  full_join(instrumental_temperature) %$% 
  cor(temperature, old)

incorrect_correlation <- recon %>%
  full_join(instrumental_temperature) %>% 
  filter(year < 1939) %$% 
  cor(temperature, old)

correct_correlation <- recon %>% 
  full_join(instrumental_temperature) %>% 
  filter(year < 1939) %$% 
  cor(temperature, new)


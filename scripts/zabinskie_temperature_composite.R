library(xml2)
library(tidyverse)

## ---- load_xml_temperature
temperature_chart <- read_xml("data/chart1.xml")

bad_format <- temperature_chart %>% 
  xml_find_all("//c:numCache") %>% 
  xml_find_all("//c:v") %>% 
  xml_double()

bad_format <- bad_format[!is.na(bad_format)]
instrumental_temperature <- bad_format %>% matrix(ncol = 4) %>%
  as_data_frame() %>% 
  select(year = V1, old = V2, new = V4)

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


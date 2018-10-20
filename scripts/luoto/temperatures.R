library(readr)
library(tidyverse)
library(magrittr)

#short series
normclim <- dir(pattern = "\\.csv$", path = "luoto/data/", full.names = TRUE) %>% 
  set_names(substr(., 13, 22)) %>% 
  map_df(read_csv, .id = "station") %>% 
  filter(m == 7) %>% 
  select(station, year = Year, Jul = `Monthly mean temperature (degC)`)

#long series
helsinki <- read_table("luoto/data/helsinki_t2974.dat", comment = "#", col_names = c("year", month.abb)) 

#helsinki_k
# %                   Raw Data           QC    Continuity     Adjusted Data      Regional Expectation
# % Year, Month, Temperature, Anomaly, Failed,   Breaks,   Temperature, Anomaly, Temperature, Anomaly
  helsinki_k <- read_table("luoto/data/13544-TAVG-Data.txt", skip = 71, col_names = c("year", "month", "raw_temperature", "raw_anomaly", "QC_failed", "continuity_breaks", "adj_temperature", "adj_anomaly","regional_temperature", "regional_anomaly")) %>% 
    filter(!is.na(raw_temperature)) %>% 
    filter(month == 7) %>% 
    select(-month)
  

#digitised series
inst <- read.table("luoto/data/measured.txt") %>% 
  select(temp = V1, year = V2) %>% 
  mutate(year = round(year))


#short plot
ggplot(normclim, aes(x = year, y = Jul, colour = station)) +  
  geom_line() + 
  geom_smooth(se = FALSE) +
  geom_line(data = inst, aes(y = temp), colour = "red") +
  geom_line(data = helsinki, aes(colour = "Helsinki S")) +
  xlim(1959, NA) 

#cross plot
normclim %>% spread(key = station, value = Jul) %>% 
  ggplot(aes(x = Helsinki_K, y = Joutsa_Sav)) + geom_point() + 
  geom_smooth()
normclim %>% spread(key = station, value = Jul) %$% 
  cor(x = Helsinki_K, y = Joutsa_Sav, use = "pair")


#helsinki_k plot
helsinki_k %>% 
  select(year, raw_temperature, adj_temperature) %>% 
  gather(key = type, value = Jul, - year) %>% 
  ggplot(aes(x = year, y = Jul, colour = type))+
  geom_line() + 
  geom_line(data = helsinki, aes(colour = "H")) +
  geom_line(data = inst, aes(y = temp), colour = "red") + geom_smooth(se = FALSE, span = 0.2) +
  geom_smooth(data = helsinki, aes(colour = "H"), span = 0.2) +
  geom_smooth(data = inst, aes(y = temp), colour = "red", span = 0.2) 

#long plot
helsinki %>% 
  ggplot(aes(x = year, y = Jul)) + 
  geom_line() +
  geom_line(data = inst, aes(y = temp), colour = "red") +
  geom_smooth(se = FALSE) +
  geom_smooth(data = inst, aes(y = temp), colour = "pink", se = FALSE) 

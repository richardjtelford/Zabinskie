#data from http://climate.weather.gc.ca/climate_data/monthly_data_e.html?hlyRange=1953-01-01%7C2013-11-14&dlyRange=1946-02-01%7C2013-11-13&mlyRange=1946-01-01%7C2013-11-01&StationID=1709&Prov=NU&urlExtension=_e.html&searchType=stnProv&optLimit=yearRange&StartYear=1910&EndYear=2018&selRowPerPage=25&Line=16&Month=10&Day=28&lstProvince=NU&timeframe=3&Year=2013

baker_clim <- read_csv("data/baker/eng-monthly-011946-112013.csv", skip = 18) %>% 
  select(year = Year, month = Month, temperature = `Mean Temp (°C)`) %>% 
  mutate(month = as.integer(month))

baker_norm <- baker_clim %>%
  filter(between(year, 1971, 2000)) %>%
  group_by(month) %>%
  summarise(norm = mean(temperature, na.rm = TRUE))

baker_anom <- baker_clim %>% left_join(baker_norm, by = "month") %>% 
  mutate(anomaly = temperature - norm)


baker_annual_anom <- baker_anom %>% 
  group_by(year) %>% 
  summarise(anomaly = mean(anomaly, na.rm = TRUE)) %>% 
  filter(between(year, 1950, 2007)) %>% 
  mutate(smo_anomaly = zoo::rollmean(anomaly, k = 5, na.pad = TRUE, align = "center"),
         smo2 = fitted(loess(anomaly ~ year, span = 0.25, data = .)))

baker_annual_anom %>% 
  ggplot(aes(x = year, y = anomaly)) +
  geom_line() +
  geom_line(aes(y = smo_anomaly), colour = "grey50") +
  coord_flip()

baker_annual_anom %>% 
  ggplot(aes(x = year, y = anomaly)) +
  geom_line() +
  geom_smooth(span = 0.23, aes(y = anomaly + 0.6)) +
  coord_flip()




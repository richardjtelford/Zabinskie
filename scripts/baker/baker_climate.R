baker <- read.table(file = "data/baker/t71926.dat") %>% set_names(c("year", month.abb))
baker[baker < -999] <- NA
baker %>% select(-year) %>% 
  cor(use = "pair") %>% 
  as.data.frame() %>% 
  rownames_to_column("month1") %>% 
  gather(key = month, value = cor, -month1) %>% 
  mutate(cor = if_else(month == month1, NA_real_, cor)) %>% 
  mutate(month = factor(month, levels = month.abb),
         month1 = factor(month1, levels = month.abb)) %>% 
  ggplot(aes(x = month1, y = month, fill = cor)) + 
  geom_raster() + 
  scale_fill_gradient2()


baker <- baker %>% 
  gather(key = month, value = temperature, -year) %>% 
  mutate(temperature = if_else(temperature < -999, NA_real_, temperature))

baker %>% ggplot(aes(x = year, y = temperature, colour = month)) +
  geom_line() +
  geom_smooth(method = "lm")


baker %>% group_by(year) %>% summarise(mean = mean(temperature, na.rm = TRUE)) %>% ggplot(aes(x = year, y = mean)) + geom_line()

bind_rows(
annual = baker %>% 
  left_join(baker %>% group_by(month) %>% summarise(norm = mean(temperature, na.rm = TRUE))) %>% 
  mutate(anomaly = temperature - norm) %>% 
  group_by(year) %>% 
  summarise(mean = mean(anomaly, na.rm = TRUE)), 

summer = baker %>% 
  left_join(baker %>% group_by(month) %>% summarise(norm = mean(temperature, na.rm = TRUE))) %>% 
  mutate(anomaly = temperature - norm) %>% 
  filter(month %in% c("Jun", "Jul", "Aug")) %>% 
  group_by(year) %>% 
  summarise(mean = mean(anomaly, na.rm = TRUE)), 
.id = "season") %>% 
  ggplot(aes(x = year, y = mean, colour = season)) + geom_line()




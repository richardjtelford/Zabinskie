#actual instrumental data
sil_monthly <- read.table(file = "silvaplana/data/homog_mo_SIA.txt", skip = 27, header = TRUE) %>%
 select(year = Year, temperature = Temperature, month = Month) %>% 
  mutate(month = factor(month, labels = month.abb))

sil_monthly %>% ggplot(aes(x = year, y = temperature, colour = month)) +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "Paired")

sil_monthly %>% 
  spread(key = month, value = temperature) %>% 
  select(-year) %>% 
  cor(use = "pair") %>% 
  as.data.frame() %>%
  rownames_to_column(var = "month") %>% 
  gather(key = month1, value = correlation, -month) %>% 
  mutate(month = factor(month, levels = month.abb), 
         month1 = factor(month1, levels = month.abb)) %>% 
  mutate(correlation = if_else(month == month1, NA_real_, correlation)) %>% 
  ggplot(aes(x = month, y = month1, fill = correlation)) + 
  geom_raster() +
  scale_fill_viridis() + 
  coord_equal() +
  theme(axis.title = element_blank())

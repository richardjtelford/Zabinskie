sovetsk <- read_station("data/sovetsk_t26600.dat") %>% 
  select(year, Aug) %>%
  mutate(Aug = if_else(Aug == -999.9, NA_real_, Aug)) %>% 
  filter(between(year, 1896, 2010))

#zabinskie with sovetsk
bind_rows(sovetsk = sovetsk, Authors = instrumental, .id = "station") %>%
  filter(year <= max(sovetsk$year)) %>% 
  group_by(station) %>% 
  ggplot(aes(x = year, y = Aug, colour = station))+ 
  geom_path() + 
  geom_point(size = 1) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  geom_vline(xintercept = 1939, linetype = "dashed", colour = "grey") +
  labs(x = "Year CE", y = "August Temperature anomaly °C", colour = "Composite")

#comparison with my composite
sovetsk %>% left_join(fat_composite %>% select(year, Aug),
                      by = "year",
                      suffix = c(".sov", ".reg")) %$% 
  cor(Aug.sov, Aug.reg)

#change resolution to match recon
sovetsk_as_zab_correct <- sovetsk %>% 
  left_join(recon %>% 
              select(-temperature) %>% 
              mutate(recon_year = year)) %>% 
  fill(recon_year, .direction = "down") %>% 
  group_by(recon_year) %>% 
  summarise(meanAug = mean(Aug)) %>% 
  arrange(desc(recon_year))

sovetsk_as_zab_published <- sovetsk %>% 
  inner_join(recon %>% 
               select(-temperature) %>% 
               mutate(recon_year = year)) %>% 
  arrange(desc(recon_year))

####
#mean for multiple years/ single years
sovetsk_as_zab <- full_join(
  sovetsk_as_zab_correct %>% select(year = recon_year, meanAug),
  sovetsk_as_zab_published %>% select(year = recon_year, spotAug = Aug)
) %>% 
  full_join(instrumental %>% rename(Aug.Zab = Aug)) 


#thinned
sovetsk_as_zab %>% 
  filter(year <= max(sovetsk$year)) %>% 
filter(between(year, 1896, 1938)) %>% 
select(year, `Authors'` = Aug.Zab, `Sovetsk - Thinned` = spotAug, `Sovetsk - Mean` = meanAug) %>% 
gather(key = data, value = Aug, -year) %>% 
ggplot(aes(x = year, y = Aug, colour = data)) +
geom_path() +
geom_point(size = 1) + 
scale_x_continuous(expand = c(0.01, 0)) + 
labs(x = "Year CE", y = "August Temperature anomaly °C", colour = "Composite")

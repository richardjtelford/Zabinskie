
## ---- calculate_lake_air_correlations
#filter metadata
max_area <- 2
min_depth <- 5
min_latitude <- 40
min_years = 10

meta <- laketemps:::gltc_metadata %>% 
  mutate(latitude = if_else(is.na(latitude) & Lake.name == "Chub", 45.18, latitude)) %>% #fill missing latitude with approx value from adjacent lakes
  filter(surface.area.km2 < max_area, 
         max.depth.m > min_depth, 
         grepl("[Ll]ake", lake.or.reservoir),
         abs(latitude) > min_latitude)

#process values 
values <- laketemps:::gltc_values %>% 
  select(-recordID) %>% 
  filter(!is.na(siteID),
         variable %in% c("Lake.Temp.Summer.InSitu", "Air.Temp.Mean.Summer.CRU")) %>% 
  semi_join(meta) %>% 
  spread(key = variable, value = value) %>%
  group_by(siteID) %>% 
  filter(!is.na(Lake.Temp.Summer.InSitu)) %>% 
  filter(n() >= min_years)  #remove short records

#calculate correlations
correlations <- values %>% 
 summarise(correlation = cor(Air.Temp.Mean.Summer.CRU, Lake.Temp.Summer.InSitu), n = n(), sd = sd(Lake.Temp.Summer.InSitu))

## ---- correlations_check
median(correlations$correlation)
nrow(correlations)

correlations %>% 
  ggplot(aes(x = correlation)) + 
  geom_histogram() +
  labs(x = "Correlation with Summer Lake Temperature") 


correlation_meta <- correlations %>% 
  left_join(meta)

mp <- map_data("world")
ggplot(correlation_meta, aes(x = longitude, y = latitude, colour = correlation)) + 
  geom_map(map = mp, data = mp, mapping = aes(map_id = region), inherit.aes = FALSE, fill = "grey80") +
  geom_point() 

correlation_meta %>% slice(which.min(correlation))

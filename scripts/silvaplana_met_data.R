## ---- silvaplana_met_data
silva_met <- read_table("data/homog_mo_SIA.txt", skip = 27) 

silva_met %>% 
  filter(Month == 7, Year <= 2001) %>% 
  ggplot(aes(x = Year, y = Temperature)) +
  geom_point() +
  geom_line()

silva_met %>% 
  filter(Month == 7, Year <= 2001) %$% 
  acf(Temperature)
  

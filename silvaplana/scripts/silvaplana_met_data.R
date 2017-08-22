## ---- silvaplana_met_data
silva_met <- read_table("data/homog_mo_SIA.txt", skip = 27) 

silva_july <- silva_met %>% 
  filter(Month == 7, Year <= 2001)

## ---- met_plots
silva_july %>% 
  ggplot(aes(x = Year, y = Temperature)) +
  geom_point() +
  geom_line()

silva_july %$% 
  acf(Temperature)
  

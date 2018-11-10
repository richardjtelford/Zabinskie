## ---- silvaplana_met_data
silva_met <- read_table("data/silvaplana/homog_mo_SIA.txt", skip = 27) 

silva_july <- silva_met %>% 
  filter(Month == 7, Year <= 2001)

## ---- met_plots
silva_july %>% 
  ggplot(aes(x = Year, y = Temperature)) +
  geom_point() +
  geom_line()

silva_july %$% 
  acf(Temperature)
  

## ---- overlay_jopl_6a_met 

fig6a <- readPNG("data/silvaplana/images/JoPL_6a.png")

jopl <- ggplot(silva_july, aes(x = Year, y = Temperature)) +
  annotation_custom(
    rasterGrob(fig6a, width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1816, xmax = 2011, ymin = 4.2, ymax = 16.25) +
  geom_point(colour = scales::hue_pal()(2)[1], size = 1) +
  geom_line(colour = scales::hue_pal()(2)[1]) + 
  scale_x_continuous(breaks = seq(1860, 2000, 20)) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 50, l = 40))

makefig(jopl)


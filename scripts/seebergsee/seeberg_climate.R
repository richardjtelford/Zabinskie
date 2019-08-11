


seeberg_climate %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  geom_point() + 
  geom_line() +
  geom_smooth(se = FALSE, span = 0.2) +
  scale_x_continuous(expand = c(0.01, 0))


## ---- overplot
makefig <- function(g){
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}

library(magick)
library(grid)
see_fig6 <- image_read("data/seebergsee/seebergsee_fig_6.png")

seeberg_climate_smo <- map_df(c(3, 8), function(w)
  seeberg_climate %>% 
  select(Year, anomaly) %>% 
  zoo::rollapply(width  = w, FUN = mean) %>% 
    as_tibble() %>% 
  mutate(width = w)
  ) %>% 
  mutate(width = as.factor(width))

see_clim_plot <- seeberg_climate %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  annotation_custom(
    rasterGrob(as.raster(see_fig6), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1889, xmax = 2006, ymin = -5.1, ymax = 3.2) +
  geom_point(size = 2, shape = 1) +
  geom_line(aes(colour = width), seeberg_climate_smo, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1900, 2000, 20)) +
  scale_y_continuous(breaks = -4:4) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
  labs(y = "Anomaly °C")

makefig(see_clim_plot)


## ---- calib_in_space
see_fig5 <- image_read("data/seebergsee/1-s2.0-S0277379111001090-gr5.jpg") %>% 
  image_crop(geometry = "384x262+0+0")

see_clim_plot5 <- seeberg_climate %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  annotation_custom(
    rasterGrob(as.raster(see_fig5), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1889, xmax = 2006, ymin = -6.9, ymax = 5.1) +
  geom_point(size = 2, shape = 1) +
  geom_line(aes(colour = width), seeberg_climate_smo, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1900, 2000, 20)) +
  scale_y_continuous(breaks = -4:4) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 60, l = 10)) +
  labs(y = "Anomaly °C")

makefig(see_clim_plot5)



## ---- digitise_cis ####


see_clim_plot5 <- sbs_digitised %>% 
  ggplot(aes(x = year, y = july)) + 
  annotation_custom(
    rasterGrob(as.raster(see_fig5), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1889, xmax = 2006, ymin = -6.9, ymax = 5.1) +
  geom_point(size = 2, shape = 1) +
  geom_line() +
  geom_line(aes(y = cis), col = 2) +
  geom_point(aes(y = cis), col = 2, size = 0.5) +
  scale_x_continuous(breaks = seq(1900, 2000, 20)) +
  scale_y_continuous(breaks = -4:4) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 60, l = 10)) +
  labs(y = "Anomaly °C") +
  geom_hline(yintercept = seq(-4,2, .5))

makefig(see_clim_plot5)


sbs_digitised %>% filter(round(year) <= 1960) %$% cor(july, cis)
sbs_digitised %>% filter(round(year) <= 1980) %$% cor(july, cis)

## -- recon_2012 ####
sbs_2012 <- read_table("data/seebergsee/seebergsee2012.txt", skip = 105) %>% select(Dates, anomaly = `11-year`)

tibble(Reconstruction  = sbs_mod$fitted.values[, 2], 
           #  cv = sbs_mod$predicted[, 2], 
           Instrumental =rev(sbs_digitised$july), 
           year = rev(sbs_digitised$year)) %>% 
 ggplot(aes(x = year, y = Reconstruction)) + 
  geom_line() +
  geom_line(data = filter(sbs_2012, Dates > 1880), aes(x = Dates, y = anomaly), colour = 2)

## ---- digitised_seebergsee_climate

tibble(Reconstruction  = sbs_mod$fitted.values[, 2], 
         #  cv = sbs_mod$predicted[, 2], 
           Instrumental =rev(sbs_digitised$july), 
           year = rev(sbs_digitised$year)) %>% 
  gather(key = type, value = temperature, -year) %>% 
  ggplot(aes(x = year, y = temperature, linetype = type, colour = type)) +
    geom_line() +
    labs(x = "Year CE", y = "July temperature anomaly, °C", colour = "", linetype = "") +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0), legend.title = element_blank(), legend.margin = margin(1,1,1,1, unit = "pt"))


## ---- sbs_temperature_versions
ggplot(seeberg_climate, aes(x = Year, y = anomaly, colour = "CHD")) +
  geom_point() +
  geom_line(aes(y = zoo::rollmean(seeberg_climate$anomaly, k = 3, fill = NA))) +
  geom_line(aes(x = year, y = july, colour = "ILT"), data = sbs_digitised) +
  labs(x = "Year CE", y = "July temperature anomaly, °C", colour = "Source") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), legend.title = element_blank(), legend.margin = margin())




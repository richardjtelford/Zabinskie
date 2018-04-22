## ---- seebergsee_climate
seeberg_climate0 <- read_table("seebergsee/homog_mo_CHD.txt", skip  = 27)
# seeberg_normal <- seeberg_climate %>%  
  # filter(between(Year, 1961, 1990), Month == 7) %>% 
  # summarise(normal = mean(Temperature)) %>% pull(normal)

seeberg_climate <- seeberg_climate0 %>% 
  filter(Month == 7, between(Year, 1901, 2005)) %>% 
  mutate(
    normal = mean(Temperature),
    anomaly = Temperature - normal
    ) 

seeberg_climate %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  geom_point() + 
  geom_line() +
  geom_smooth(se = FALSE, span = 0.2) +
  scale_x_continuous(expand = c(0.01, 0))


## ---- all_months
seeberg_climate_12 <- seeberg_climate0 %>% 
  group_by(Month) %>% 
  filter(between(Year, 1901, 2005)) %>% 
  mutate(
    normal = mean(Temperature),
    anomaly = Temperature - normal
  ) 

seeberg_climate_12 %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  geom_point() + 
  geom_line() +
  geom_line(aes(x = year, y = july), data = sbs_digitised, colour = "red") +
  geom_smooth(se = FALSE, span = 0.2) +
  scale_x_continuous(expand = c(0.01, 0)) +
  facet_wrap(~Month)

seeberg_climate0 %>% 
  group_by(Year) %>%
  summarise(Temperature = mean(Temperature)) %>% 
  filter(between(Year, 1901, 2005)) %>% 
  mutate(
    normal = mean(Temperature),
    anomaly = Temperature - normal
  ) %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  geom_point() + 
  geom_line() +
  geom_line(aes(x = year, y = july), data = sbs_digitised, colour = "red") +
  geom_smooth(se = FALSE, span = 0.2) +
  scale_x_continuous(expand = c(0.01, 0)) 

seeberg_climate0 %>% 
  filter(between(Month, 5, 9), between(Year, 1901, 2005)) %>% 
  group_by(Year) %>%
  summarise(Temperature = mean(Temperature)) %>% 
  mutate(
    normal = mean(Temperature),
    anomaly = Temperature - normal
  ) %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  geom_point() + 
  geom_line() +
  geom_line(aes(x = year, y = july), data = sbs_digitised, colour = "red") +
  geom_smooth(se = FALSE, span = 0.2) +
  scale_x_continuous(expand = c(0.01, 0)) 


## ---- overplot
makefig<-function(g){
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}

library(magick)
library(grid)
see_fig6 <- image_read("seebergsee/seebergsee_fig_6.png")

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
see_fig5 <- image_read("seebergsee/1-s2.0-S0277379111001090-gr5.jpg") %>% 
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
sbs_digitised <- read.table("seebergsee/seebergsee_climate") %>% 
  rename(year = V1, july = V2) %>% 
  slice(-40)#bad click
cis <- c(.7, 2, .7, 1.8, .2, -1.2, -.3, -.3, 1.7, -.1, .6, -4, 2.1, 1.1, 1.3, 1.2, 1.2, -1, 0, 0, 1.5, .9, 1.3, 1.6, 1.6)

sbs_digitised <- sbs_digitised %>% 
  mutate(cis = c(cis, rep(NA, nrow(.) -length(cis))))

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


library(magrittr)
sbs_digitised %>% filter(round(year) <= 1960) %$% cor(july, cis)
sbs_digitised %>% filter(round(year) <= 1980) %$% cor(july, cis)

## -- recon_2012 ####
sbs_2012 <- read_table("seebergsee/seebergsee2012.txt", skip = 105) %>% select(Dates, anomaly = `11-year`)

data_frame(Reconstruction  = sbs_mod$fitted.values[, 2], 
           #  cv = sbs_mod$predicted[, 2], 
           Instrumental =rev(sbs_digitised$july), 
           year = rev(sbs_digitised$year)) %>% 
 ggplot(aes(x = year, y = Reconstruction)) + geom_line() +
  geom_line(data = filter(sbs_2012, Dates > 1880), aes(x = Dates, y = anomaly), colour = 2)

## ---- digitised_seebergsee_climate
sbs_digitised <- read.table("seebergsee/seebergsee_climate") %>% 
  rename(year = V1, july = V2) %>% 
  slice(-40)#bad click

ggplot(sbs_digitised, aes(x = year, y = july)) + geom_point() + geom_line()

#seeberg_merged$merged_depth

sbs_mod <- WAPLS(sqrt(seeberg_pc), rev(sbs_digitised$july)) %>% crossval()
sbs_perf <- sbs_mod%>% performance()
#sbs_perf
#sbs_perf$object[2, 2] ^ 0.5
#sbs_perf$crossval[2, 2] ^ 0.5

data_frame(Reconstruction  = sbs_mod$fitted.values[, 2], 
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



## ---- random_temperature
nrep <- 1000
random_perf <- replicate(nrep, {
  mod_r <- WAPLS(sqrt(seeberg_pc), rnorm(nrow(seeberg_pc)))
  perf_r <- mod_r %>% performance()
  perf_r$object[2, 2]
})

random_hist <- ggplot(data_frame(r = random_perf^0.5), aes(x = r)) + 
  geom_histogram() + 
  geom_vline(xintercept = performance(sbs_mod)$object[2, 2]^0.5)


#sqrt(performance(sbs_mod)$object[2, 2] )

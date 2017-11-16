## ---- seebergsee_climate
seeberg_climate <- read_table("seebergsee/homog_mo_CHD.txt", skip  = 27)
seeberg_normal <- seeberg_climate %>%  
  filter(between(Year, 1961, 1990), Month == 7) %>% 
  summarise(normal = mean(Temperature)) %>% pull(normal)

seeberg_climate <- seeberg_climate %>% 
  filter(Month == 7, between(Year, 1901, 2005)) %>% 
  mutate(
    normal = seeberg_normal,
    anomaly = Temperature - normal
    ) 

seeberg_climate %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  geom_point() + 
  geom_line() +
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


see_clim_plot <- seeberg_climate %>% 
  ggplot(aes(x = Year, y = anomaly)) + 
  annotation_custom(
    rasterGrob(as.raster(see_fig6), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1889, xmax = 2006, ymin = -5.1, ymax = 3.2) +
  geom_point(size = 2, shape = 1) +
  geom_smooth(span = 0.1, se = FALSE) +
  scale_x_continuous(breaks = seq(1900, 2000, 20)) +
  scale_y_continuous(breaks = -4:4) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

makefig(see_clim_plot)


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

## ---- seebergsee_climate
seeberg_climate <- read_table("seebergsee/homog_mo_CHD.txt", skip  = 27)
seeberg_normal <- seeberg_climate %>%  
  filter(between(Year, 1961, 1990), Month == 7) %>% 
  summarise(normal = mean(Temperature)) %>% pull(normal)

seeberg_climate <- seeberg_climate %>% 
  filter(Month == 7, between(Year, 1900, 2005)) %>% 
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

sbs_mod <- WAPLS(sqrt(seeberg_pc), rev(sbs_digitised$july))
sbs_perf <- crossval(sbs_mod) %>% performance()
#sbs_perf
#sbs_perf$object[2, 2] ^ 0.5
#sbs_perf$crossval[2, 2] ^ 0.5




## ---- random_temperature
random_perf <- replicate(1000, {
  mod_r <- WAPLS(seeberg_merged %>% select(-merged_depth) %>% sqrt(), rnorm(nrow(seeberg_merged)))
  perf_r <- mod_r %>% performance()
  perf_r$object[2, 2]
})

ggplot(data_frame(r2 = random_perf), aes(x = r2)) + geom_histogram() + geom_vline(xintercept = performance(mod)$object[2, 2])

mean(random_perf >  performance(mod)$object[2, 2])
sqrt(performance(mod)$object[2, 2] )

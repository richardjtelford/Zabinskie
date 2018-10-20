## ---- seebergsee_digitised
seeberg_count <- read.csv("seebergsee/seebergsee_counts.csv")
seeberg_count[is.na(seeberg_count)] <- 0

strat.plot(d = seeberg_count %>% select(-depth, -total, -total4, -merged_depth), yvar = seeberg_count$depth, y.rev = TRUE, plot.line = FALSE, plot.bar = TRUE, scale.percent = TRUE)

seeberg_merged <- seeberg_count %>% select(-total, -total4, -depth) %>% 
  gather(key = species, value = count, -merged_depth) %>% 
  group_by(species, merged_depth) %>% 
  summarise(count = sum(count)) %>% 
  spread(key = species, value = count) %>% 
  filter(rowSums(.[,-1]) > 0) 

seeberg_pc <- seeberg_merged[, -1] / rowSums(seeberg_merged[, -1]) * 100
seeberg_pc <- seeberg_pc %>% select_("Chironomus.plumosus", "Dicrotendipes", "Microtendipes", "Parachironomus", "Pseudochironomus", "Paratanytarsus", "Tanytarsus.sp", "Tanytasus.lugens", "Tanytrsus.mendax", "Tanytarsus.sp..C",  "Corynoneura", "Corynoneura.arctica", "Corynoneura.edwardsi", "Cricotopus", "Eukiefferiella", "Limnophyes", "Orthocladius", "Parakiefferiella", "Psectrocladius.sordidellus", "Pseudosmittia", "Smittia", "Telopelopia", "Procladius")

strat.plot(d = seeberg_pc, yvar = seeberg_merged$merged_depth, y.rev = TRUE, plot.line = FALSE, plot.bar = TRUE, scale.percent = TRUE)

#nrow(seeberg_pc)

## count sums
seeberg_sums <- seeberg_count %>% group_by(merged_depth) %>% summarise(sum = sum(total), sum4 = sum(total4)) 

#mean(seeberg_sums$sum4 < 30)
#mean(seeberg_sums$sum < 30)
#max(seeberg_sums$sum)


## ---- age_depth
seeberg_chron <- read.csv("seebergsee/seebergsee_age_depth.csv")
seeberg_chron %>% 
  mutate(years = -(age - max(age))) %>% 
  ggplot(aes(x = years, y = depth)) + 
  geom_line() + 
  scale_y_reverse()

seeberg_chron2011 <- data_frame(depth = c(4, 16), years = 2005 - c(1970, 1920))

used_chron <- sbs_digitised %>% 
  mutate(years = 2005 - year) %>% 
  arrange(years) %>% 
  bind_cols(
    seeberg_count %>% 
      select(depth, merged_depth) %>% 
      group_by(merged_depth) %>% 
      summarise(depth = mean(depth))
    )

see_fig3 <- image_read("seebergsee/seebergsee_fig_3.png")

see_chrono_plot <- seeberg_chron %>% 
  mutate(years = -(age - max(age))) %>% 
  ggplot(aes(x = years, y = depth)) + 
  annotation_custom(
    rasterGrob(as.raster(see_fig3), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = -12, xmax = 127, ymin = -25.5, ymax = 0.5) +
  geom_point(size = 2, shape = 1, colour = scales::muted("blue")) +
  geom_line(colour = scales::muted("blue")) +
  geom_point(data = seeberg_chron2011, col = scales::muted("red")) +
  geom_line(colour = scales::muted("blue")) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_reverse(breaks = seq(0, 20, 5)) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
  labs(x = "Years", y = "Depth cm")

makefig(see_chrono_plot )




## ---- sbs_ca
sbs_ca <- cca(sqrt(seeberg_pc))




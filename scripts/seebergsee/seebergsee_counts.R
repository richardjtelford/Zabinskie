## ---- seebergsee_digitised
#import digitised count data
seeberg_read_counts <- function(){ 
  f <- file_in("data/seebergsee/seebergsee_counts.csv")
  seeberg_count <- read.csv(f)
  seeberg_count[is.na(seeberg_count)] <- 0
  return(seeberg_count)
}

#calculate number of occurrences in the count data 
seeberg_calc_noccur <- function(seeberg_count){
  seeberg_n <- seeberg_count %>% 
    select(-depth, -merged_depth, -total, -total4) %>% 
    gather(key = taxon, value = cnt) %>% 
    summarise(n = sum(cnt > 0)) 
  return(seeberg_n)
}

# strat.plot(d = seeberg_count %>% select(-depth, -total, -total4, -merged_depth), yvar = seeberg_count$depth, y.rev = TRUE, plot.line = FALSE, plot.bar = TRUE, scale.percent = TRUE)

#merge counts as in paper
seeberg_merge_counts <- function(seeberg_count){
  seeberg_merged <- seeberg_count %>% 
    select(-total, -total4, -depth) %>% 
    gather(key = species, value = count, -merged_depth) %>% 
    group_by(species, merged_depth) %>% 
    summarise(count = sum(count)) %>% 
    spread(key = species, value = count) %>% 
    filter(rowSums(.[,-1]) > 0) 
  seeberg_merged
}

#calculate percent
seeberg_calc_percent <- function(seeberg_merged){
  seeberg_pc <- seeberg_merged[, -1] / rowSums(seeberg_merged[, -1]) * 100
  #reorder taxa to match published figure
  seeberg_pc <- seeberg_pc %>% 
    select_("Chironomus.plumosus", "Dicrotendipes", "Microtendipes", "Parachironomus", "Pseudochironomus", "Paratanytarsus", "Tanytarsus.sp", "Tanytasus.lugens", "Tanytrsus.mendax", "Tanytarsus.sp..C",  "Corynoneura", "Corynoneura.arctica", "Corynoneura.edwardsi", "Cricotopus", "Eukiefferiella", "Limnophyes", "Orthocladius", "Parakiefferiella", "Psectrocladius.sordidellus", "Pseudosmittia", "Smittia", "Telopelopia", "Procladius")
  return(seeberg_pc)
}
#strat.plot(d = seeberg_pc, yvar = seeberg_merged$merged_depth, y.rev = TRUE, plot.line = FALSE, plot.bar = TRUE, scale.percent = TRUE)

## calculate count sums
seeberg_calc_countsums <- function(seeberg_count){
  seeberg_sums <- seeberg_count %>% 
    group_by(merged_depth) %>% 
    summarise(sum = sum(total), sum4 = sum(total4))
  return(seeberg_sums)
}
#mean(seeberg_sums$sum4 < 30)
#mean(seeberg_sums$sum < 30)
#max(seeberg_sums$sum)


## ---- seebergsee_climate load original climate data
seeberg_load_climate <- function(){
  f <-  file_in("data/seebergsee/homog_mo_CHD.txt")
  seeberg_climate0 <- read_table(f, skip  = 27)
  
  seeberg_climate <- seeberg_climate0 %>% 
    filter(Month == 7, between(Year, 1901, 2005)) %>% 
    mutate(
      normal = mean(Temperature),
      anomaly = Temperature - normal
    ) 
  
  return(seeberg_climate)
}

#load digitised climate data
seeberg_load_digitised_climate <- function(){
  f <- file_in("data/seebergsee/seebergsee_climate")
  sbs_digitised <- read.table(f) %>% 
    rename(year = V1, july = V2) %>% 
    slice(-40)#bad click
  cis <- c(.7, 2, .7, 1.8, .2, -1.2, -.3, -.3, 1.7, -.1, .6, -4, 2.1, 1.1, 1.3, 1.2, 1.2, -1, 0, 0, 1.5, .9, 1.3, 1.6, 1.6)
  
  sbs_digitised <- sbs_digitised %>% 
    mutate(cis = c(cis, rep(NA, nrow(.) -length(cis)))) %>% #pad with NA
    arrange(desc(year))#sort to match assemblage data
    
  return(sbs_digitised)
}


##climate plots
seeberg_plot_climate <- function(seeberg_digitised_climate, seeberg_climate){
  g <- bind_rows(Paper = seeberg_digitised_climate %>% select(year, anomaly = july),
            Station = seeberg_climate %>% select(year = Year, anomaly), .id = "what") %>% 
    ggplot(aes(x = year, y = anomaly, colour = what)) + 
    geom_point() +
    geom_line() +
    scale_colour_manual(values = RColorBrewer::brewer.pal(9, "Set1")[c(1, 9)]) + 
    scale_x_continuous(expand = c(0.02, 0)) +
    labs(x = "Year CE", y = "July temperature anomaly °C") +
    theme(legend.position = c(0.01, 0.99), legend.justification = c(0, 1), legend.title = element_blank())
  return(g)
}

## ---- random_temperature reconstruction
seeberg_calc_random_perform <- function(seeberg_pc, seeberg_nrep){
  random_perf <- replicate(seeberg_nrep, {
    mod_r <- WAPLS(sqrt(seeberg_pc), rnorm(nrow(seeberg_pc)))
    perf_r <- mod_r %>% performance()
    perf_r$object[2, 2]
  })
}


# random_hist <- ggplot(data_frame(r = random_perf^0.5), aes(x = r)) + 
#   geom_histogram() + 
#   geom_vline(xintercept = performance(sbs_mod)$object[2, 2]^0.5)


#sqrt(performance(sbs_mod)$object[2, 2] )




## ---- age_depth
# seeberg_chron <- read.csv("data/seebergsee/seebergsee_age_depth.csv")
# seeberg_chron %>% 
#   mutate(years = -(age - max(age))) %>% 
#   ggplot(aes(x = years, y = depth)) + 
#   geom_line() + 
#   scale_y_reverse()
# 
# seeberg_chron2011 <- data_frame(depth = c(4, 16), years = 2005 - c(1970, 1920))
# 
# used_chron <- sbs_digitised %>% 
#   mutate(years = 2005 - year) %>% 
#   arrange(years) %>% 
#   bind_cols(
#     seeberg_count %>% 
#       select(depth, merged_depth) %>% 
#       group_by(merged_depth) %>% 
#       summarise(depth = mean(depth))
#     )
# 
# see_fig3 <- image_read("seebergsee/seebergsee_fig_3.png")
# 
# see_chrono_plot <- seeberg_chron %>% 
#   mutate(years = -(age - max(age))) %>% 
#   ggplot(aes(x = years, y = depth)) + 
#   annotation_custom(
#     rasterGrob(as.raster(see_fig3), width=unit(1,"npc"), height=unit(1,"npc")),
#     xmin = -12, xmax = 127, ymin = -25.5, ymax = 0.5) +
#   geom_point(size = 2, shape = 1, colour = scales::muted("blue")) +
#   geom_line(colour = scales::muted("blue")) +
#   geom_point(data = seeberg_chron2011, col = scales::muted("red")) +
#   geom_line(colour = scales::muted("blue")) +
#   scale_x_continuous(breaks = seq(0, 100, 20)) +
#   scale_y_reverse(breaks = seq(0, 20, 5)) +
#   theme_classic() +
#   theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
#   labs(x = "Years", y = "Depth cm")
# 
# makefig(see_chrono_plot )

library("tidyverse")

## ---- regional_composite

read_station <- function(file){
  x <- read.table(file, skip = 5, header = FALSE)
  names(x) <- c("year", month.abb)
  x
}

#station data
kanuas <- read_station("data/Kaunas_26629.dat")
warsaw <- read_station("data/warsaw_12375.dat")
vilnius <- read_station("data/vilnius26730.dat")
kalingrad<- read_station("data/kalingrad26702.dat")

##
allStations <- kanuas %>% mutate(station = "Kanuas") %>%
  bind_rows(warsaw %>% mutate(station = "Warsaw")) %>% 
  bind_rows(vilnius %>% mutate(station = "Vilnius")) %>%
  bind_rows(kalingrad %>% mutate(station = "Kalingrad")) %>% 
  gather(key = month, value = temperature, -year, -station) %>%
  mutate(temperature = if_else(temperature == -999.9, NA_real_, temperature)) %>% 
  filter(between(year, 1896, 2010))


composite <- allStations %>% 
  group_by(station, month) %>% 
  mutate(temperature = temperature - mean(temperature, na.rm = TRUE)) %>% 
  group_by(year, month) %>% 
  summarise(temperature = mean(temperature, na.rm = TRUE)) %>% 
  mutate(station = "Mine")

fat_composite <- composite %>% 
  spread(key = month, value = temperature) %>%
  mutate(summer = rowMeans(data.frame(Jun, Jul, Aug)))

fat_composite_as_zab_correct <- fat_composite %>% 
  ungroup() %>% 
  left_join(recon %>% 
              select(-temperature) %>% 
              mutate(recon_year = year)) %>% 
  fill(recon_year, .direction = "down") %>% 
  group_by(recon_year) %>% 
  summarise_at(vars(one_of(c(month.abb, "summer"))), mean) %>% 
  arrange(desc(recon_year))

fat_composite_as_zab_published <- fat_composite %>% 
  ungroup() %>% 
  inner_join(recon %>% 
              select(-temperature) %>% 
              mutate(recon_year = year)) %>% 
  arrange(desc(recon_year))


## ---- check
cor(fat_composite$Aug, fat_composite$Jun)
cor(fat_composite$Aug, fat_composite$Jul)
cor(fat_composite$Aug, fat_composite$Sep)
cor(fat_composite$Aug, fat_composite$summer)



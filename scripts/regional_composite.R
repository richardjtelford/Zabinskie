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
allStations <- bind_rows(
  Kanuas = kanuas,
  Warsaw =  warsaw, 
  Vilnius = vilnius,
  Kalingrad = kalingrad, 
    .id = "station") %>% 
  gather(key = month, value = temperature, -year, -station) %>%
  mutate(temperature = if_else(temperature == -999.9, NA_real_, temperature)) %>% 
  filter(between(year, 1896, 2010))


normals <- allStations %>% 
  filter(year > 1950, year <= 1980) %>% 
#  group_by(station) %>% 
#  summarise(na = sum(is.na(temperature)))
  group_by(station, month) %>% 
  summarise(normal = mean(temperature, na.rm = TRUE))

composite <- allStations %>% 
  left_join(normals) %>% 
  group_by(station, month) %>% 
  mutate(temperature = temperature - normal) %>% 
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
#              select(-temperature) %>% 
              mutate(recon_year = year)) %>% 
  arrange(desc(recon_year))

composite_as_zab <- full_join(
  fat_composite_as_zab_correct %>% select(year = recon_year, meanAug = Aug),
  fat_composite_as_zab_published %>% select(year = recon_year, spotAug = Aug, recon= temperature)
) %>% 
  full_join(instrumental_temperature %>% rename(Aug.Zab = old)) %>% 
  mutate_at(.vars = vars(matches("Aug")), scale, scale = FALSE)

## ---- temperature_correlations
#vs instrumental
i_pre_thinned <- composite_as_zab %>% filter(year < 1939) %$% cor(Aug.Zab, spotAug)
i_pre_mean <- composite_as_zab %>% filter(year < 1939) %$% cor(Aug.Zab, meanAug)
i_post <- composite_as_zab %>% filter(year >= 1939) %$% cor(Aug.Zab, spotAug)

#vs reconstruction 
pre_thinned <- composite_as_zab %>% filter(year < 1939) %$% cor.test(recon, spotAug)
pre_mean <- composite_as_zab %>% filter(year < 1939) %$% cor.test(recon, meanAug)
post <- composite_as_zab %>% filter(year >= 1939) %$% cor(recon, spotAug)

## ---- check
cor(fat_composite$Aug, fat_composite$Jun)
cor(fat_composite$Aug, fat_composite$Jul)
cor(fat_composite$Aug, fat_composite$Sep)
cor(fat_composite$Aug, fat_composite$summer)

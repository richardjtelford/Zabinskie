## correlation in space
library("raster")
library("readxl")
library("tidyverse")

## ---- correlation_in_space

#Poland
poland <- read_excel("data/zabinskie2015cit.xls", sheet = "Poland-Canada Lakes", skip = 1) %>% 
  select(Lake, Latitude, Longitude) %>% 
  filter(!is.na(Lake)) %>% 
  mutate(lat_deg = gsub("(\\d+)º.*", "\\1", Latitude), 
         lat_min = gsub(".*º(\\d+)'.*", "\\1", Latitude),
         lat_sec = gsub(".*'(\\d+\\.\\d+).*", "\\1", Latitude),
        
         long_deg = gsub("(\\d+)º.*", "\\1", Longitude), 
         long_min = gsub(".*º(\\d+)'.*", "\\1", Longitude),
         long_sec = gsub(".*'(\\d+\\.\\d+).*", "\\1", Longitude)
  ) %>% 
    mutate_at(vars(matches("_")), as.numeric) %>% 
    mutate(lat = lat_deg + lat_min / 60 + lat_sec/3600, 
           long = long_deg + long_min / 60 + long_sec/3600) %>%     select(Lake, lat, long) %>% 
  mutate(country = "Poland")
  
mp <- map_data(map = "world", region = "Poland")

ggplot(poland, aes(x = long, y = lat)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  geom_point()

#Canada part 
canada1 <- read_excel("data/zabinskie2015cit.xls", sheet = "Poland-Canada Lakes", skip = 9) %>% 
  select(LAKE, `Latitude/Longitude`, Location) %>% 
  filter(!is.na(`Latitude/Longitude`)) %>% 
  mutate(`Latitude/Longitude` = gsub("’|′", "'",`Latitude/Longitude`)) %>% 
  separate(`Latitude/Longitude`, into = c("Latitude", "Longitude"), sep = "N", remove = FALSE) %>% 
  mutate(Longitude = gsub(", ", "", Longitude)) %>% 
  mutate(lat_deg = gsub("(\\d+)°.*", "\\1", Latitude), 
         lat_min = gsub(".*° *(\\d+)'.*", "\\1", Latitude),
         
         long_deg = gsub("(\\d+)°.*", "\\1", Longitude), 
         long_min = gsub(".*° *(\\d+)'.*", "\\1", Longitude)
  ) %>% 
  select(-`Latitude/Longitude`) %>% 
  mutate_at(vars(matches("_")), as.numeric) %>%    
  mutate(lat = lat_deg + lat_min / 60, 
         long = -(long_deg + long_min / 60)) %>%
  select(Lake = LAKE, lat, long)


mp <- map_data(map = "world", region = "Canada")

ggplot(canada1, aes(x = long, y = lat)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  geom_point()

#canada2
canada2 <- read.table("data/canada_locations.txt")

canada <- canada2 %>% 
  select(long = V1, lat = V2) %>% 
  bind_rows(canada1) %>% 
  mutate(country = "Canada")

dim(canada)#couple missing

ggplot(canada, aes(x = long, y = lat)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  geom_point()

all_sites <- bind_rows(poland, canada)
all_sites2 <- all_sites
coordinates(all_sites2) <- ~ long + lat

##worldclim
climate <- sapply(1:12, function(mon){
  mon <- ifelse(mon < 10, paste0(0, mon), mon)
  file <- paste0("data/worldclim/wc2.0_10m_tavg_",mon,".tif")
  wclim <- raster::raster(x = file)

  raster::extract(wclim, all_sites2)
})

colnames(climate) <- month.abb

climate <- bind_cols(all_sites, as.data.frame(climate))
climate <- climate %>% 
  mutate(summer = rowMeans(data.frame(Jun, Jul, Aug)))

cor(climate$Jun, climate$Aug, use = "pair")
cor(climate$summer, climate$Aug, use = "pair")

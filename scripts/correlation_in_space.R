## correlation in space
#library("raster") - fights with dplyr over select
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
  mutate(country = "Poland") %>% 
  mutate(Lake = if_else(Lake == "KIE_W", "KIE-W", Lake)) %>%  # match format in other tables
  semi_join(sites)#remove low count & extra 2 sites

mp <- map_data(map = "world", region = "Poland")

ggplot(poland, aes(x = long, y = lat)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  geom_point()

#Canada part 
canada0 <- read_excel("data/zabinskie2015cit.xls", sheet = "Poland-Canada Lakes", skip = 9) %>% 
  select(Lake = LAKE, `Latitude/Longitude`, Location)
  

canada1 <- canada0 %>% 
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
  select(Lake, lat, long) %>% 
  mutate(long = if_else(Lake == "South4", -84.2, long))#putting lake on land to match location in map in Rolland et al 2008


mp <- map_data(map = "world", region = "Canada")

ggplot(canada1, aes(x = long, y = lat)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  geom_point()

#canada2

canada2 <- read.table("data/canada_locations.txt") %>% 
  select(long = V1, lat = V2) %>% 
  bind_rows(.[9, ]) %>% #lakes 9 & 10 very close together
  arrange(desc(lat)) 

canada2 <- canada0 %>% filter(grepl("^[Ll]ake {0,1}\\d{1,2}$", Lake)) %>% #remove L08 lakes
  mutate(Lake = gsub(" ", "", Lake), gsub("l", "L", Lake)) %>% 
  select(Lake) %>% bind_cols(canada2 %>% 
                           slice(-c(1, 10, 13, 19, 38, 43, 54, 55, 60:63)) # remove lakes on map but without data/southern L08 sites)
)

#canada06
canada06 <- read_csv("data/CCIN12504_20151119_JOPL-D-14-00075_location-version2-2015.csv") %>% select(Lake = NAME, lat = LAT, long = LONG)

ggplot(canada06, aes(x = long, y = lat, label = Lake)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80", inherit.aes = FALSE) + 
  geom_point() +
  ggrepel::geom_text_repel(size = 2, min.segment.length = unit(0.1, "lines")) +
  coord_quickmap() + 
  labs(x = "", y = "") +
  xlim(-80, -75)


# #canada 06
# canada06 <- read.table("data/Quebec environmental data.csv", sep = ",", header = TRUE)#sites appear to be mislablled
# 
# canada06 <- canada06 %>% 
#   filter(!grepl("^M", Lake)) %>% 
#   mutate(Lake = paste0("Lake", Lake)) %>% 
#   select(Lake, lat = Lat, long = Long) %>% 
#   semi_join(sites)
  

ggplot(canada2, aes(x = long, y = lat, label = Lake)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80", inherit.aes = FALSE) + 
  geom_point() +
  ggrepel::geom_text_repel(size = 2, min.segment.length = unit(0.1, "lines")) +
  coord_quickmap() + 
  labs(x = "", y = "") +
  xlim(-80, -75)

#misplaced lakes (in Hudson Bay)


canada <- canada2 %>% 
  bind_rows(canada1) %>% 
  mutate(country = "Canada")

dim(canada)#few missing

ggplot(canada, aes(x = long, y = lat, label = Lake)) +
  geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80", inherit.aes = FALSE) + 
  geom_point()+
  ggrepel::geom_label_repel()



all_sites <- sites %>% 
  left_join(bind_rows(poland, canada))
all_sites2 <- all_sites %>% filter(!is.na(lat))
coordinates(all_sites2) <- ~ long + lat

##worldclim
climate <- sapply(1:12, function(mon){
  mon <- ifelse(mon < 10, paste0(0, mon), mon)
  file <- paste0("data/worldclim/wc2.0_10m_tavg_",mon,".tif")
  wclim <- raster::raster(x = file)

  raster::extract(wclim, all_sites2)
})

colnames(climate) <- month.abb

climate <- all_sites %>% filter(!is.na(lat)) %>% 
  bind_cols(as.data.frame(climate)) %>% 
  mutate(summer = rowMeans(data.frame(Jun, Jul, Aug)))

cor(climate$Jun, climate$Aug, use = "pair")
cor(climate$summer, climate$Aug, use = "pair")

## ---- check_worldClim
Jan <- raster::raster(x = "data/worldclim/wc2.0_10m_tavg_01.tif")
Janr <- raster::as.data.frame(Jan, xy = TRUE) %>% filter(x < -51, x> -96, y > 46, y < 63)

library(mapdata)
mp <- map_data("worldHires", region = "Canada")
ggplot(Janr, aes(x = x, y = y, fill = wc2.0_10m_tavg_01)) + 
  geom_raster() + 
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0,0)) + 
  labs(fill = "Jan") +
  geom_point(data = filter(climate, source == "L06"), aes(x = long, y = lat), shape = 1, inherit.aes = FALSE, col = 2)+ 
geom_point(data = canada06, aes(x = long, y = lat), shape = 1, inherit.aes = FALSE, col = 3) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = NA, colour = "black")

Aug <- raster::raster(x = "data/worldclim/wc2.0_10m_tavg_08.tif")
Augr <- raster::as.data.frame(Aug, xy = TRUE) %>% filter(x < -61, x > -95, y > 60, y < 84)

ggplot(Augr, aes(x = x, y = y, fill = wc2.0_10m_tavg_08)) + 
  geom_raster() + 
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0,0)) + 
  labs(fill = "Aug") +
  geom_point(data = filter(climate, source == "L2008"), aes(x = long, y = lat), shape = 1, inherit.aes = FALSE, colour = "red") +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = NA, colour = "black")



## ---- check_vs_LT15_temp
sites %>% left_join(climate) %$% cor(Aug, env, use = "pair") 
sites %>% left_join(climate) %>%  
  mutate(LT15 = env) %>% 
  ggplot(aes(x = LT15, y = Aug, colour = source))+ 
  geom_point() + 
  geom_smooth(aes(group = 1)) +
  geom_abline() #weird in arctic / one site mislocated in HudsonBay littoral

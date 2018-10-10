## correlation in space

## ---- correlation_in_space
zabinskie_calibration_climate <- function(zabinskie_excel_file, sites){
  #Poland 
  poland <- read_excel(zabinskie_excel_file, sheet = "Poland-Canada Lakes", skip = 1) %>% 
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
             long = long_deg + long_min / 60 + long_sec/3600) %>%
    select(Lake, lat, long) %>% 
    mutate(country = "Poland") %>% 
    mutate(Lake = if_else(Lake == "KIE_W", "KIE-W", Lake)) %>%  # match format in other tables
    semi_join(sites)#remove low count & extra 2 sites
  
  # mp <- map_data(map = "world", region = "Poland")
  # 
  # ggplot(poland, aes(x = long, y = lat)) +
  #   geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  #   geom_point()
  
  #Canada part 
  canada0 <- read_excel(zabinskie_excel_file, sheet = "Poland-Canada Lakes", skip = 9) %>% 
    select(Lake = LAKE, `Latitude/Longitude`, Location)
    
  
  canada1 <- canada0 %>%
    mutate(`Latitude/Longitude`  = if_else(Lake == "NorthLake", "81° 21’ N, 69° 32'W", `Latitude/Longitude` )) %>% #set unknown location of NorthLake to that of UML
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
  
  
  # mp <- map_data(map = "world", region = "Canada")
  # 
  # ggplot(canada1, aes(x = long, y = lat)) +
  #   geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80") + 
  #   geom_point()
  
  #canada06
  canada06 <- read_csv("data/zabinskie/CCIN12504_20151119_JOPL-D-14-00075_location-version3-2015.csv") %>% 
    filter(AUTHOR == "Larocque") %>%
    mutate(NAME = paste0("Lake", substring(NAME, 3, 4))) %>% 
    select(Lake = NAME, lat = LAT, long = LONG)
  
  # ggplot(canada06, aes(x = long, y = lat, label = Lake)) +
  #   geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80", inherit.aes = FALSE) + 
  #   geom_point() +
  #   ggrepel::geom_text_repel(size = 2, min.segment.length = unit(0.1, "lines")) +
  #   coord_quickmap() + 
  #   labs(x = "", y = "") +
  #   xlim(-80, -75)
  
  # sites %>% 
  #   left_join(canada06) %>% 
  #   mutate(temp = env) %>% 
  #   filter(source == "L06") %>% 
  #   ggplot(aes(x = lat, y = temp)) + 
  #   geom_point()#looks good
  
  #southern canada08 lakes - very approximate locations
  c1 <- filter(canada06, Lake == "Lake1") 
  minLat <- 42 + 32/60
    
  canada08S <- canada0 %>% 
    filter(grepl("^Lac [A-H]$", Lake)) %>% 
    select(Lake) %>% 
    mutate(
      long = pull(c1, long),
      lat = seq(pull(c1, lat), minLat, length = 9)[1:8])
  
  #env[grepl("^Lac [A-H]$", sites$Lake)]
  
  
  
  canada <- bind_rows(canada1, canada06, canada08S) %>% 
    mutate(country = "Canada")
  
  # ggplot(canada, aes(x = long, y = lat, label = Lake)) +
  #   geom_map(map = mp, data = mp, aes(map_id = region), fill = "grey80", inherit.aes = FALSE) + 
  #   geom_point()+
  #   ggrepel::geom_label_repel(size = 2)
  
  all_sites <- sites %>% 
    left_join(bind_rows(poland, canada))
  all_sites2 <- all_sites %>% filter(!is.na(lat))
  coordinates(all_sites2) <- ~ long + lat
  
  ##worldclim
  #check worldclim data exists
  if(!file.exists("data/worldclim/wc2.0_10m_tavg_01.tif")){
    stop("data/worldclim/wc2.0_10m_tavg_01.tif", " Missing. Download and unzip from http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_10m_tavg.zip")
  }
  
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
  
  return(climate)
}

# cor(climate$Jun, climate$Aug, use = "pair")
# cor(climate$summer, climate$Aug, use = "pair")
# 
# ## ---- check_worldClim
# Jan <- raster::raster(x = "data/worldclim/wc2.0_10m_tavg_01.tif")
# Janr <- raster::as.data.frame(Jan, xy = TRUE) %>% filter(x < -51, x> -96, y > 46, y < 63)
# 
# library(mapdata)
# mp <- map_data("worldHires", region = "Canada")
# ggplot(Janr, aes(x = x, y = y, fill = wc2.0_10m_tavg_01)) + 
#   geom_raster() + 
#   scale_x_continuous(expand = c(0, 0))+ 
#   scale_y_continuous(expand = c(0,0)) + 
#   labs(fill = "Jan") +
#   geom_point(data = filter(climate, source == "L06"), aes(x = long, y = lat), shape = 1, inherit.aes = FALSE, col = 2)+ 
# geom_point(data = canada06, aes(x = long, y = lat), shape = 1, inherit.aes = FALSE, col = 3) +
#   geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = NA, colour = "black")
# 
# Aug <- raster::raster(x = "data/worldclim/wc2.0_10m_tavg_08.tif")
# Augr <- raster::as.data.frame(Aug, xy = TRUE) %>% filter(x < -70, x > -85, y > 40, y < 64)
# 
# ggplot(Augr, aes(x = x, y = y, fill = wc2.0_10m_tavg_08)) + 
#   geom_raster() + 
#   scale_x_continuous(expand = c(0, 0))+ 
#   scale_y_continuous(expand = c(0,0)) + 
#   labs(fill = "Aug") +
# #  geom_point(data = filter(climate, source == "L2008"), aes(x = long, y = lat), shape = 1, inherit.aes = FALSE, colour = "red") +
#   geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = NA, colour = "black") +
#   geom_point(data = canada06, aes(x = long, y = lat), inherit.aes = FALSE)+
#   geom_hline(yintercept = 42.5)
# 
# 
# 
# ## ---- check_vs_LT15_temp
# sites %>% left_join(climate) %$% cor(Aug, env, use = "pair") 
# sites %>% left_join(climate) %>%  
#   mutate(LT15 = env) %>% 
#   ggplot(aes(x = LT15, y = Aug, colour = source))+ 
#   geom_point() + 
#   geom_abline() #weird in arctic 

## ---- regional_composite
zabinskie_regional_composite <- function(){
  read_station <- function(file){
    x <- read.table(file, skip = 5, header = FALSE)
    names(x) <- c("year", month.abb)
    x
  }
  
  #station data
  kanuas <- read_station("data/zabinskie/station_climate/Kaunas_26629.dat")
  warsaw <- read_station("data/zabinskie/station_climate/warsaw_12375.dat")
  vilnius <- read_station("data/zabinskie/station_climate/vilnius26730.dat")
  kalingrad<- read_station("data/zabinskie/station_climate/kalingrad26702.dat")
  
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
  
  return(fat_composite)
}


# fat_composite_as_zab_correct <- fat_composite %>% 
#   ungroup() %>% 
#   left_join(recon %>% 
#               select(-temperature) %>% 
#               mutate(recon_year = year)) %>% 
#   fill(recon_year, .direction = "down") %>% 
#   group_by(recon_year) %>% 
#   summarise_at(vars(one_of(c(month.abb, "summer"))), mean) %>% 
#   arrange(desc(recon_year))


# fat_composite_as_zab_published <- fat_composite %>% 
#   ungroup() %>% 
#   inner_join(recon %>% 
# #              select(-temperature) %>% 
#               mutate(recon_year = year)) %>% 
#   arrange(desc(recon_year))


# ## ---- check
# cor(fat_composite$Aug, fat_composite$Jun)
# cor(fat_composite$Aug, fat_composite$Jul)
# cor(fat_composite$Aug, fat_composite$Sep)
# cor(fat_composite$Aug, fat_composite$summer)

speke_temp <- read.table("data/speke/speke.txt") %>% 
  select(1:2) %>% 
  setNames(c("year", "temp")) %>% 
  mutate(type = rep(c("recon", "inst"), each = 16))   

speke_temp %>% group_by(type) %>% summarise(m = mean(temp))
ggplot(speke_temp, aes(x = year, y = temp, colour = type)) + 
  geom_point() + 
  geom_line() +
  
  scale_x_reverse()

speke_temp2 <- speke_temp %>% 
  mutate(year = round(year)) %>% 
  spread(key = type, value = temp)

speke_temp2 %>% ggplot(aes(x=inst, y = recon)) +
  geom_abline()+
  geom_point() +
  geom_smooth(method = "lm")

speke_temp2 %$% cor.test(inst, recon)
speke_temp2 %$% cor(inst, year)
speke_temp2 %$% cor(recon, year)

speke_temp2 %$% cor(resid(lm(inst ~ year)), 
               resid(lm(recon ~ year)))


##
read_station <- function(file){
  x <- read.table(file, skip = 5, header = FALSE)
  names(x) <- c("year", month.abb)
  x
}

#station data
valley <- read_station("data/speke/valley_t3302.dat")

ggplot(speke_temp2, aes(x = year, y = inst)) + 
  geom_point(colour = "red") +
  geom_line(colour = "red")+
  geom_line(data = valley, mapping = aes(x = year, y = Jul), inherit.aes = FALSE) + 
  geom_smooth(data = valley, mapping = aes(x = year, y = Jul), inherit.aes = FALSE, span = 0.3) + 
  theme(legend.position = "none") +
  xlim(min(valley$year), max(valley$year))
  

# july <- raster::raster("data/worldclim/wc2.0_10m_tavg_07.tif")
# speke_location <- data_frame(long = -(2 + 52/60 + 23/3600), lat = 53 + 20/60 + 19/3600)
# coordinates(speke_location) <- ~ long + lat
# 
# raster::extract(july, speke_location)
# 
# norway_train$Julyt %>% range()



#climate
valley2 <- valley %>% 
  select(year,Jul) %>% 
  mutate(
    smo3 = rollmean(Jul, k = 3, na.pad = TRUE, align = "center"),
    smo5 = rollmean(Jul, k = 5, na.pad = TRUE, align = "center")) %>% 
  select(-Jul) %>% 
  gather(key = smooth, value = Jul, -year)

speke_chron %>% 
  mutate(top = CodeNum, base = lead(CodeNum)) %>% 
  crossing(valley2) %>% 
  filter(year == base) %>% 
  ggplot(aes(x = base, y = Jul, colour = smooth)) + 
  geom_line() + geom_point() +
  geom_line(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) + 
  geom_point(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) +
  geom_line(data = valley, aes(x = year, y = Jul), colour = "grey40")

speke_reported_climate <- speke_chron %>%
  mutate(top = CodeNum, base = lead(CodeNum)) %>%
  crossing(valley) %>%
  filter(year >= base, year < top) %>%
  group_by(base) %>%
  summarise(Jul = mean(Jul)) %>% 
  arrange(desc(base)) %>% 
  bind_cols(speke_pred %>% slice(1:17)) %>% 
  slice(-17) 

speke_reported_climate %>% 
  ggplot(aes(x = base, y = Jul)) + 
  geom_line(colour = "red") + 
  geom_point(colour = "red") +
  geom_line(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) + 
  geom_point(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) +
  geom_line(data = valley, aes(x = year, y = Jul), colour = "grey40")

speke_reported_climate %$% 
  cor.test(Jul, pred) 


library("tidyverse")
library("readxl")

#station data
kanuas <- read.table("data/Kaunas_26629.dat", skip = 5, header = FALSE)
names(kanuas) <- c("year", month.abb)

warsaw <- read.table("data/warsaw_12375.dat", skip = 5, header = FALSE)
names(warsaw) <- c("year", month.abb)

vilnius <- read.table("data/vilnius26730.dat", skip = 5, header = FALSE)
names(vilnius) <- c("year", month.abb)

kalingrad<- read.table("data/kalingrad26702.dat", skip = 5, header = FALSE)
names(kalingrad) <- c("year", month.abb)



#zabinskie instrumental composite
zab <- read.table("data/instrumental.txt")[, 1:2]
names(zab) <- c("year", "Aug")
zab <- zab %>% mutate(year = round(year))

#reconstruction
recon <- read_excel("data/zabinskie2015cit.xls", sheet = "Reconstruction ")
names(recon) <- c("year", "Aug")

##
allStations <- kanuas %>% select(year, Aug) %>% mutate(station = "Kanuas") %>%
  bind_rows(warsaw %>% select(year, Aug) %>% mutate(station = "Warsaw")) %>% 
  bind_rows(vilnius %>% select(year, Aug) %>% mutate(station = "Vilnius")) %>%
  bind_rows(kalingrad %>% select(year, Aug) %>% mutate(station = "Kalingrad")) %>% 
  mutate(Aug = if_else(Aug == -999.9, NA_real_, Aug)) %>% 
  filter(between(year, 1896, 2010))
  

#zabinskie with all stations
zab %>% mutate(station = "Zabinskie") %>% 
  bind_rows(allStations) %>% 
  bind_rows(recon %>%  mutate(station = "Reconstruction")) %>% 
  group_by(station) %>% 
  mutate(Aug = Aug - mean(Aug, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Aug, colour = station))+ 
  geom_path() + 
  geom_point(size = 1) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  labs(x = "Year CE", y = "Temperature anomaly °C", colour = "Station")


composite <- allStations %>% 
  group_by(station) %>% 
  mutate(Aug = Aug - mean(Aug, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarise(Aug = mean(Aug, na.rm = TRUE)) %>% 
  mutate(station = "Composite")

#zabinskie with composite
composite %>% 
  bind_rows(zab %>% mutate(station = "Zabinskie")) %>% 
#  bind_rows(recon %>%  mutate(station = "Reconstruction")) %>% 
  group_by(station) %>% 
  mutate(Aug = Aug - mean(Aug, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Aug, colour = station))+ 
  geom_path() + 
  geom_point(size = 1) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  labs(x = "Year CE", y = "Temperature anomaly °C", colour = "Station")



#mean for multiple years/ single years
composite_as_zab <- zab %>% 
  mutate(year2 = year) %>% 
  full_join(data_frame(year = 1896:2010)) %>% 
  arrange(year) %>% 
  fill(year2, .direction = "down") %>% 
  left_join(composite, by = "year", suffix = c(".Zab", ".composite")) %>% 
  group_by(year2) %>% 
  mutate(meanAug = mean(Aug.composite)) %>% 
  filter(!is.na(Aug.Zab)) %>%  
  ungroup() %>% 
  select(-year2, -station) %>% 
  left_join(recon %>% rename(recon = Aug))
  

composite_as_zab %>% filter(year >= 1939) %>% select(-year) %>% cor
composite_as_zab %>% filter(year < 1939) %>% select(-year) %>% cor

mod1 <- lm(recon~Aug.composite, data = composite_as_zab, subset = year < 1939)
mod2 <- lm(recon~meanAug, data = composite_as_zab, subset = year < 1939)
AIC(mod1, mod2)

sapply(composite_as_zab, sd)

with(composite_as_zab %>% filter(year < 1939), cor.test(recon, Aug.composite))
with(composite_as_zab %>% filter(year < 1939), cor.test(recon, meanAug))

composite_as_zab %>% 
  mutate(Aug.Zab = Aug.Zab - mean(Aug.Zab)) %>% 
  mutate(recon = recon - mean(recon)) %>% 
  select(-recon) %>% 
  gather(key = data, value = Aug, -year) %>% 
  ggplot(aes(x = year, y = Aug, colour = data)) +
  geom_path() +
  geom_point(size = 1) + 
  geom_vline(xintercept = 1939, linetype = "dashed", colour = "grey") +
  scale_x_continuous(expand = c(0.01, 0)) + 
  labs(x = "Year CE", y = "Temperature anomaly °C", colour = "Station")




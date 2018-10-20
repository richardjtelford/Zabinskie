library(tidyverse)
library(magrittr)
speke <- read.table("speke/speke.txt") %>% 
  select(1:2) %>% 
  setNames(c("year", "temp")) %>% 
  mutate(type = rep(c("recon", "inst"), each = 16))   

speke %>% group_by(type) %>% summarise(m = mean(temp))
ggplot(speke, aes(x = year, y = temp, colour = type)) + 
  geom_point() + 
  scale_x_reverse()

speke2 <- speke %>% 
  mutate(year = round(year)) %>% 
  spread(key = type, value = temp)

speke2 %>% ggplot(aes(x=inst, y = recon)) +
  geom_abline()+
  geom_point() +
  geom_smooth(method = "lm")

speke2 %$% cor(inst, recon)
speke2 %$% cor(inst, year)


#correct correlation
#DFeff = DF (1-r1r1')/(1+r1r1') 
r1 <- ar(speke2$inst, order.max = 1)$ar
r2 <- ar(speke2$recon, order.max = 1, aic = FALSE)$ar
DFeff <- (nrow(speke2) - 2) * (1 - r1 * r2) / (1 + r1 * r2) 
r_p(0.62, DFeff + 2)
r_p(0.62, 16)

speke %>% filter(type == "inst") %$% acf(temp)
speke %>% filter(type == "recon") %$% acf(temp)

data_frame(year = speke2$year, diff = c(NA, diff(year))) %>% ggplot(aes(x = year, y = diff)) + geom_point()

##
read_station <- function(file){
  x <- read.table(file, skip = 5, header = FALSE)
  names(x) <- c("year", month.abb)
  x
}

#station data
valley <- read_station("speke/valley_t3302.dat")
bidston <- read_station("speke/bidston_t3316.1.dat")
bidston[bidston == -999.9] <- NA

ggplot(speke2, aes(x = year, y = inst)) + 
  geom_point(colour = "red") +
  geom_line(data = valley, mapping = aes(x = year, y = Jul), inherit.aes = FALSE) + 
  geom_line(data = bidston, mapping = aes(x = year, y = Jul), inherit.aes = FALSE, col= 4) + 
  theme(legend.position = "none") +
  xlim(min(valley$year), max(valley$year))
  

valley %>% filter(between(year, 1989 - 2, 1989 + 2)) %>% 
  summarise(Jul = mean(Jul))
speke2 %>% filter(year == 1989)

b_v <- inner_join(valley, bidston, by = "year", suffix = c(".v", ".b")) %>% 
  select(year, matches("Jul")) 

b_v %$% cor(Jul.v, Jul.b, use = "pair")
b_v %>% filter(!is.na(Jul.b)) %>% select(-year) %>% colMeans()


cet_v <- inner_join(valley, cet, by = "year", suffix = c(".v", ".c")) %>% 
  select(year, matches("Jul"))

cet_v %$% cor(Jul.v, Jul.c, use = "pair")

cet_b <- inner_join(bidston, cet, by = "year", suffix = c(".v", ".b")) %>% 
  select(year, matches("Jul"))

cet_b %$% cor(Jul.v, Jul.b, use = "pair")


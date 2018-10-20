#baker lake

#load packages ####
library("tidyverse")
library("rioja")
library("readxl")
library("countChecker")
library("vegan")

#importdata
#baker_spp <- read_excel("baker/trainingset.xls")
#baker_fos <- read_excel("baker/B1.xls")
#baker_env <- read_excel("baker/env.xls")

baker_spp <- read_excel("baker/thesisMedeiros-w3may7.xls", sheet = "TS-R")
baker_env <- read_excel("baker/thesisMedeiros-w3may7.xls", sheet = "env")
baker_fosR <- read_excel("baker/thesisMedeiros-w3may7.xls", sheet = "core-R")

#fix RS21 to RA19
baker_env <- baker_env %>% 
  mutate(CodeName = recode(CodeName, "RA21"="RA19")) %>% 
  arrange(CodeName)

baker_spp <- baker_spp %>% arrange(X__1)




#countchecker
estimate_n(baker_spp %>% select(-X__1), digits = 7) %>% arrange(est_n)

baker_spp %>%
  percent_checker(digits = 5, site_column = "X__1") %>% 
  select(-one_max, -one_min, -est_min, -est_max) %>% 
  filter(contains == "weird")

estimate_n(baker_fosR %>% select(-Depth), digits = 7) %>% arrange(est_n)

weird_fos <- baker_fosR %>% 
  percent_checker(digits = 5, site_column = "Depth") %>% 
  select(-one_max, -one_min, -est_min, -est_max) %>% 
  filter(contains == "weird")

baker_fos %>% filter(CodeName == "Sam0037") %>% 
  gather(key = species, value = percent, -(CodeNum:Sites2)) %>% 
  filter(percent > 0) %>% 
  arrange(percent) %>% 
  mutate(n = percent / min(percent))


crossing(tot = seq(50,100, 0.5), n = seq(0, 100, 0.5)) %>% 
  filter(n < tot) %>% 
  mutate(percent = n/tot * 100) %>% 
  mutate(delta = abs(percent - weird_fos$percent[1])) %>% 
  arrange(delta) %>% 
  slice(1:10)

## species lists
setdiff(names(baker_fosR), names(baker_spp))
setdiff(names(baker_spp), names(baker_fosR))
identical(sort(baker_spp$X__1), sort(baker_env$CodeName))

select(baker_spp, -X__1) %>% rowSums()

mod <- WAPLS(select(baker_spp, -X__1) %>% sqrt(), baker_env$MSSWT) %>% 
  crossval()

performance(mod) 

pred <- predict(mod, select(baker_fosR, -Depth) %>% sqrt())$fit %>% 
  as_data_frame() %>% 
  mutate(Depth = baker_fosR$Depth)

ggplot(pred, aes(x = Depth, y = Comp02)) +
  geom_point() +
  geom_line() +
  coord_flip() +
  scale_x_reverse()

select(baker_fosR, -Depth) %>% 
  sqrt() %>% 
  decorana() %>% 
  scores(choice = 1) %>% 
#  acf()
  ar()

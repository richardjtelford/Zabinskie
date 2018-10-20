#
library(tidyverse)
library(readr)

an <- read_table(file = "silvaplana/data/homog_mo_SIA.txt", skip = 27) %>% 
  filter(
    between(Month, 4, 11),
    Year < 2004) %>% 
  group_by(Year) %>%
  summarise(meanT = mean(Temperature)) 

an %>% 
  ggplot(aes(x = Year, y = meanT)) +
  geom_line()
  
acf(an$meanT)  

tp <- bind_rows(an =
                  read_table(file = "silvaplana/data/homog_mo_SIA.txt", skip = 27) %>% 
                  filter(
                    between(Month, 4, 11),
                    between(Year, 1952, 2004)) %>% 
                  group_by(Year) %>%
                  summarise(meanT = mean(Temperature)), 
                
                ma = read_table(file = "silvaplana/data/homog_mo_SIA.txt", skip = 27) %>% 
                  filter(
                    between(Month, 5, 8),
                    between(Year , 1954, 2004)) %>% 
                  group_by(Year) %>%
                  summarise(meanT = mean(Temperature)),
                a = read_table(file = "silvaplana/data/homog_mo_SIA.txt", skip = 27) %>% 
                  filter(
                    between(Month, 7, 7),
                    between(Year , 1954, 2004)) %>% 
                  group_by(Year) %>%
                  summarise(meanT = mean(Temperature)),
                .id = "period")

tp %>% group_by(period) %>% summarise(sd =sd(meanT))

tp %>% 
  group_by(period) %>% 
  mutate(meanT = meanT - mean(meanT)) %>% 
  ggplot(aes(x = Year, y = meanT, colour = period)) +
  geom_line()

a <- c(0, 0, 0 , 1, 0, 0, 0)
zoo::rollmean(a, 2, align = "left", na.pad = TRUE) %>% 
  zoo::rollmean(2, align = "right", na.pad = TRUE)

tp <- tp %>% group_by(period) %>% 
  mutate(
    smo = zoo::rollmean(meanT, 2, align = "left", na.pad = TRUE),
    smo = zoo::rollmean(smo, 2, align = "right", na.pad = TRUE))
tp

tp %>% filter(!is.na(smo)) %>% 
  group_by(period) %>% 
  summarise(sd =sd(meanT), sds = sd(smo))
tp %>% filter(period == "an") %>% 
  gather(key = smo, value = temp, meanT, smo) %>% 
  ggplot(aes(x = Year, y = temp, colour = smo)) + geom_line()

library(magrittr)
an <- tp %>% filter(period == "an", !is.na(smo))
an  %$% acf(smo)
an %$% acf(meanT)

library(vegan)


nspp <- 11
spp <- rnorm((nrow(an) + 2) * nspp) %>% 
  matrix(ncol = nspp) %>% 
  as_data_frame() %>% 
  rowid_to_column(var = "depth")

spp  %>% gather(key = species, value = value, -depth) %>% 
  ggplot(aes(x = depth, y = value, colour = species)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~species)

mod <- rda(select(spp, -depth) %>% slice(-c(1, nrow(spp))) ~ meanT, data = an)
eigenvals(mod)[1]/sum(eigenvals(mod))

spps <- spp %>% 
  mutate_at(.vars= vars(-depth), zoo::rollmean, k = 2, align = "left", na.pad = TRUE) %>% 
  mutate_at(.vars = vars(-depth), zoo::rollmean, k = 2, align = "right", na.pad = TRUE)

spp  %>% 
  bind_rows(smo = spps, .id = "smo") %>% 
  gather(key = species, value = value, -depth, -smo) %>% 
  ggplot(aes(x = depth, y = value, colour = smo)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~species)

mods <- rda(select(spps, -depth) %>% slice(-c(1, nrow(spp))) ~ meanT, data = an)
eigenvals(mods)[1]/sum(eigenvals(mods))

loo <- function(x,  formula, mod = lm, ...){
  sapply(1:nrow(x), function(n){
    
    test <- x[n, ]
    train <- x[-n, ]
    
    fit <- mod(formula, data = train, ...)
    predict(fit, newdata = test)
  })
}

pred.cv <- loo(cbind(meanT = an$meanT, select(spps, V1:V6) %>% slice(-c(1, nrow(spp)))), formula = "meanT ~ .")

cor(an$meanT, pred.cv)

pred.cv2 <- loo(cbind(smo = an$smo, select(spps, V1:V6) %>% slice(-c(1, nrow(spp)))), formula = "smo ~ .")

cor(an$smo, pred.cv2)


sims <- plyr::rdply(1000, {
  nspp <- 11
  spp <- rnorm((nrow(an) + 2) * nspp) %>% 
    matrix(ncol = nspp) %>% 
    as_data_frame() %>% 
    rowid_to_column(var = "depth")
  
  mod <- rda(select(spp, -depth) %>% slice(-c(1, nrow(spp))) ~ meanT, data = an)
  
  spps <- spp %>% 
    mutate_at(.vars= vars(-depth), zoo::rollmean, k = 2, align = "left", na.pad = TRUE) %>% 
    mutate_at(.vars = vars(-depth), zoo::rollmean, k = 2, align = "right", na.pad = TRUE)
  
  mods <- rda(select(spps, -depth) %>% slice(-c(1, nrow(spp))) ~ meanT, data = an)
  
  pred.cv <- loo(cbind(meanT = an$meanT, select(spps, V1:V6) %>% slice(-c(1, nrow(spp)))), formula = "meanT ~ .")
  pred.cv2 <- loo(cbind(smo = an$smo, select(spps, V1:V6) %>% slice(-c(1, nrow(spp)))), formula = "smo ~ .")
  
  data_frame(
    raw_e =   eigenvals(mod)[1]/sum(eigenvals(mod)),
    smo_e =  eigenvals(mods)[1]/sum(eigenvals(mods)),

    raw = cor(an$meanT, pred.cv),
             smo = cor(an$smo, pred.cv2)
  )
})

sims %>% 
  select(raw, smo) %>% 
  gather(key = what, value = value) %>% 
  ggplot(aes(x = what, y = value)) +
  geom_violin()

sims %>% 
  select(raw_e, smo_e) %>% 
  gather(key = what, value = value) %>% 
  ggplot(aes(x = what, y = value)) +
  geom_violin()

sims %>%
  gather(key = what, value = value) %>% 
  group_by(what) %>% 
  summarise(q = quantile(value, prob = 0.95))

sims %>% 
  ggplot(aes(x = raw, y = smo)) + geom_point() + geom_abline()

sims %>% mutate(delta = smo - raw) %>% summarise(m = mean(delta > 0))

library(zoo)
library(tidyverse)
library(lubridate)
library(readr)
#running mean correlation inflation
corr <- 0.35
cv <- matrix(c(1, rep(corr, 2), 1), nrow = 2)

nosmo_smo <- plyr::rdply(1000, {
  x <- mvtnorm::rmvnorm(n = 100, mean = c(0, 0), sigma = cv)
  x3 <- rollmean(x, k = 3)
  c(raw = cor(x[, 1], x[, 2]), smoothed = cor(x3[, 1], x3[, 2]))
}, .id = NULL)
  
nosmo_smo %>% 
  gather(key = type, value = correlation) %>% 
  ggplot(aes(x = correlation, fill = type, colour = type)) +
    geom_density(alpha = 0.5) + 
    geom_vline(xintercept = corr)

nosmo_smo %>% 
  ggplot(aes(x = raw, y = smoothed)) +
  geom_point(alpha = 0.5) + 
  geom_abline() + 
  geom_smooth()

lm(smoothed ~ raw, nosmo_smo) %>% summary()

nosmo_smo %>%  
  gather(key = type, value = correlation) %>% 
  group_by(type) %>% 
  summarise(var = var(correlation))

##
silva_met <- read_table("silvaplana/data/homog_mo_SIA.txt", skip = 27) 

#lake ice
ice <- read_csv("silvaplana/data/liag_freeze_thaw_table.csv")

ice %>% 
  filter(country == "SWITZERLAND") %>% 
  mutate_all(function(x){x[x == -999] <- NA; x}) %>% 
  mutate(date = lubridate::ymd(paste(iceoff_year, iceoff_month, iceoff_day, sep = "-")),
         doy = lubridate::yday(date)) %>% 
  ggplot(aes(x = iceoff_year, y = doy, colour = lakename)) + 
  geom_line() + 
  geom_point()

#cor with LEJ DA SAN MUREZZAN 
ice2 <- ice %>% 
  filter(lakename == "LEJ DA SAN MUREZZAN") %>% 
  mutate(
    date = ymd(paste(iceoff_year, iceoff_month, iceoff_day, sep = "-")),
    doy = yday(date)
  ) %>% 
  select(lakename, Year = iceoff_year, doy) 

#make multiple winter variables
#winter = november to march
#if nov/dec, advance to following year
silva_met2 <- silva_met %>% 
  mutate(Year = if_else(Month > 6, Year + 1L, Year)) %>% 
  filter(Year > min(Year),#partial winter
         Year <= max(ice2$Year)) %>% 
  group_by(Year)
  
#combinations <- lapply(1:6, combn, x = c(11:12, 1:4), simplify = FALSE) %>% do.call(what = "c")

combinations <- plyr::llply(1:12, function(wid){
  zoo::rollapply(1:12, width = wid,  FUN = I) %>% 
    as.matrix() %>% 
    plyr::alply(1, function(mo)c(6:12, 1:5)[mo])
}) %>% 
  do.call(what = "c")

winters <- sapply(combinations, function(com){
  silva_met2 %>% 
    filter(Month %in% com) %>% 
    summarise(winter = mean(Temperature)) %>% 
    pull(winter)
})

colnames(winters) <- sapply(combinations, function(com){
    paste0(".", paste(com, collapse = "."))
  })


ggplot(data_frame(year = unique(silva_met2$Year), oct_may = winters[, ".10.11.12.1.2.3.4.5"], smo105 = rollmean(oct_may, k = 3, fill = NA)), aes(x = year, y = smo105)) + geom_line() + 
geom_line(data = data_frame(year = unique(silva_met2$Year), april = winters[, ".4"], smo4 = rollmean(april, k = 3, fill = NA)), aes(x = year, y = smo4), colour = "red")


cor(winters) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  gather(key = winter, value = correlation, -rowname) %>%
  ggplot(aes(x = rowname, y = winter, fill = correlation)) +
  geom_raster() + 
  theme(axis.title = element_blank())

winter_cor <- plyr::raply(1000, {
  x <- rnorm(nrow(winters))
  max(apply(winters, 2, cor, x = x))
})
mean(winter_cor)
ggplot(data_frame(winter_cor = winter_cor), aes(x = winter_cor)) + 
  geom_density()








ice2 <- ice2 %>%   
  semi_join(silva_met2)
  
ice_cors <- data_frame(
  months = sapply(combinations, paste, collapse = "."), 
  v = apply(winters, 2, cor, pull(ice2, doy))
  )
ice_cors %>% arrange(v)

ice_cors %>% ggplot(aes(x = v)) + geom_density()

ice_cors %>% arrange(v) %>% mutate(n = 1:n(), april = grepl(4, months)) %>% 
  ggplot(aes(x = n, y = v, colour = april)) +
  geom_point()


which.min(ice_cors$v)
cor(winters[, c(".4", ".10.11.12.1.2.3.4.5")])

# create the initial x variable
x1 <- rnorm(100, 15, 5)

# x2, x3, and x4 in a matrix, these will be modified to meet the criteria
x234 <- scale(matrix(rnorm(300), ncol=3 ))

# put all into 1 matrix for simplicity
x1234 <- cbind(scale(x1), x234)

# find the current correlation matrix
c1 <- var(x1234)

# cholesky decomposition to get independence
chol1 <- solve(chol(c1))

newx <-  x1234 %*% chol1 

# check that we have independence and x1 unchanged
zapsmall(cor(newx))
all.equal( x1234[,1], newx[,1] )

# create new correlation structure (zeros can be replaced with other r vals)
newc <- matrix( 
  c(1  , 0.4, 0.5, 0.6, 
    0.4, 1  , 0  , 0  ,
    0.5, 0  , 1  , 0  ,
    0.6, 0  , 0  , 1  ), ncol=4 )

# check that it is positive definite
eigen(newc)

chol2 <- chol(newc)

finalx <- newx %*% chol2 * sd(x1) + mean(x1)

# verify success
mean(x1)
colMeans(finalx)

sd(x1)
apply(finalx, 2, sd)

zapsmall(cor(finalx))
pairs(finalx)

all.equal(x1, finalx[,1])

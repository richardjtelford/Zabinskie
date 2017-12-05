## ---- figure_5

seeb_mds <- metaMDS(sqrt(seeberg_pc), dist = "euclidean")
seeb_mds <- metaMDS(seeberg_pc)

fortify(seeb_mds) %>% 
  filter(Score == "sites") %>% 
  mutate(year = rev(sbs_digitised$year)) %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) + 
    geom_path() +
    geom_point(aes( colour = year > 1950)) +
    coord_equal()
  
decorana(sqrt(seeberg_pc))
sbs_ca <- cca(sqrt(seeberg_pc))

fortify(sbs_ca) %>% 
  filter(Score == "sites") %>% 
  mutate(year = rev(sbs_digitised$year)) %>% 
  ggplot(aes(x = CA1, y = CA2)) + 
  geom_path() +
  geom_point(aes( colour = year > 1950)) +
  coord_equal()


#silvaplana
sil_mds <- metaMDS(sqrt(fos_holocene), dist = "euclidean")
sil_mds <- sqrt(fos_holocene) %>% 
  mutate(year = holocene_year$YearAD > 1950, year = as.numeric(year)) %>% 
  metaMDS(dist = "euclidean")
sil_mds <- fos_holocene %>% 
  mutate(year = holocene_year$YearAD > 1950, year = as.numeric(year) * 1000) %>% 
  metaMDS()
sil_mds <- metaMDS(fos_holocene)

fortify(sil_mds) %>% 
  filter(Score == "sites") %>% 
  mutate(year = holocene_year$YearAD) %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_path() +
  geom_point(aes( colour = year > 1950)) +
  coord_equal()

fos_holocene %>% ggplot(aes(x = Microt, y = Cricoto)) + geom_point()#rogue sample

decorana(sqrt(fos_holocene))
sil_ca <- cca(sqrt(fos_holocene))
screeplot(sil_ca, bstick = TRUE)

sil_ca <- fortify(sil_ca)

sil_ca %>% 
  filter(Score == "sites") %>% 
  mutate(year = holocene_year$YearAD) %>% 
  ggplot(aes(x = CA1, y = CA2)) + 
  geom_path() +
  geom_point(aes( colour = year > 1950)) +
  coord_equal()



## ---- random_optima

wapls.predict <- function(coef, Y)
{
  onesc <- rep(1, ncol(Y))
  onesc[is.na(coef)] <- 0
  coef[is.na(coef)] <- 0
  R <- as.matrix(Y) %*% onesc
  Y %*% coef / R
}

plot(predict(mod, SWAP$spec)$fit[,2], wapls.predict(mod$coefficients[,2], as.matrix(SWAP$spec)))
## random coef

sbs_mat <- as.matrix(seeberg_pc)[ rev(sbs_digitised$year) <= 1980, ]
sbs_jt <- rev(sbs_digitised$july[sbs_digitised$year <= 1980])
rtf <- replicate(10000, {
  coef <- rnorm(n = ncol(seeberg_pc))
  recon <- wapls.predict(coef, sbs_mat)
  cor(recon, sbs_jt)
})

rtf %>% hist()
quantile(rtf, probs = c(0.025, 0.975))
mean(rtf > 0.64)

sbs_mat <- as.matrix(seeberg_pc)[ rev(sbs_digitised$year) <= 1960, ]
sbs_jt <- rev(sbs_digitised$july[sbs_digitised$year <= 1960])
rtf <- replicate(10000, {
  coef <- rnorm(n = ncol(seeberg_pc))
  recon <- wapls.predict(coef, sbs_mat)
  cor(recon, sbs_jt)
})

rtf %>% hist()
quantile(rtf, probs = c(0.025, 0.975))
mean(rtf > 0.71)
mean(rtf > 0.79)

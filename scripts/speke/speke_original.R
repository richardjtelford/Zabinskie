#import data
speke_env <- read_excel("data/speke/temperature.xls")
speke_spp <- read_excel("data/speke/species.xls")
speke_fos <- read_excel("data/speke/core.xls")

speke_chron <- speke_fos %>% select(CodeNum:FullName)

speke_spp <- speke_spp %>% select(-(CodeNum:FullName))
speke_fos <- speke_fos %>% select(-(CodeNum:FullName))

lowCount <- c(13, 111, 134, 139)#outliers, probably not low count

# remove outliers
speke_spp <- speke_spp %>% filter(!speke_env$CodeNum %in% lowCount)
speke_env <- speke_env %>% filter(!CodeNum %in% lowCount)

#remove rare taxa
speke_spp <- speke_spp %>% select_if(~max(.) > 2)


#trasform
speke_fos1 <- decostand(speke_fos, method="hellinger")
speke_spp1 <- decostand(speke_spp, method="hellinger")




#fit model
# speke_mod0 <- WAPLS(speke_spp1, speke_env$July) %>% 
#   crossval()
# performance(speke_mod0)
# 
# data_frame(measured = speke_mod0$x, 
#            fitted = speke_mod0$fitted.values[, "Comp02"],
#            predicted = speke_mod0$predicted[, "Comp02"], 
#            lowc = 1:nrow(speke_spp) %in% lowCount) %>% 
#   ggplot(aes(x = measured, y = predicted, colour = lowc)) + geom_point()


#refit mod
speke_mod <- WAPLS(speke_spp1, speke_env$July) %>% 
  crossval()
performance(speke_mod)
rand.t.test(speke_mod)

data_frame(measured = speke_mod$x, 
           fitted = speke_mod$fitted.values[, "Comp03"],
           predicted = speke_mod$predicted[, "Comp03"]) %>% 
  ggplot(aes(x = measured, y = predicted)) + geom_point()


speke_pred <- data_frame(
  year = speke_chron$CodeNum,
  pred = predict(speke_mod, speke_fos1)$fit[, "Comp03"])

speke_pred %>% ggplot(aes(x = year, y = pred)) + 
  geom_point() + 
  geom_line() + 
  scale_x_reverse() + 
  geom_line(speke_temp2, mapping = aes(y = recon), colour = "red")


#analogue distances
d <- paldist(speke_spp) %>% 
  as.matrix() %>% 
  as.dist() %>% #hist()
  quantile(probs = c(.05, .1))

mat <- MAT(speke_spp, speke_env$July)
speke_pred$dist <- predict(mat, speke_fos)$dist.n[, 1] 

ggplot(speke_pred, aes(x = year, y = dist)) + geom_point() + 
  geom_hline(yintercept = d)

#residual length
rl <- analogue::residLen(speke_spp1, speke_env$July, speke_fos1, method = "cca")

plot(rl)
speke_pred$resLen <- rl$passive

ggplot(speke_pred, aes(x = year, y = resLen)) + geom_point() + 
  geom_hline(yintercept = quantile(rl$train, probs = c(.85, .95))) +
  scale_x_reverse()


#
est_n <- estimate_n(speke_fos, digits = 2) %>% 
  mutate(n = 1:n()) %>% 
  arrange(est_n)

est_n %>% 
  summarise(s = sum(est_n), m = mean(est_n))

percent_checker(speke_fos, digits = 2) %>% 
  select(-one_max, -one_min, -est_max, -est_min)

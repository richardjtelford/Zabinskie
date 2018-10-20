env2 <- env
env2[env2 > 21] <- 21
mod <- WAPLS(sqrt(spp), env)
pred <- predict(mod, sqrt(fos))
mod2 <- WAPLS(sqrt(spp), env2)
pred2 <- predict(mod2, sqrt(fos))

ggplot(data.frame(mod = coef(mod)[, 2], mod2 = coef(mod2)[, 2]), aes(mod, mod2)) + geom_point() + geom_abline() + coord_equal()

performance(crossval(mod))
performance(crossval(mod2))


preds <- chron %>% mutate(mod = pred$fit[, 2], mod2 = pred2$fit[, 2]) 

preds %>% ggplot(aes(mod, mod2)) + geom_point() + geom_abline() + coord_equal()

preds %>% 
  gather(key = what, value = temp, -year) %>% 
  ggplot( aes(year, temp, colour = what)) + geom_line()

preds %>% mutate(delta = mod2 - mod) %>% arrange(desc(abs(delta)))
preds %>% mutate(delta = mod2 - mod) %>% summarise(diff = mean(abs(delta)))

preds %>% full_join(instrumental_temperature) %>% cor()

preds %>% full_join(instrumental_temperature) %>% mutate(delta = mod - old, delta2 = mod2 - old, worse = abs(delta2) > abs(delta)) %>% summarise(s = mean(abs(delta)), s2  =mean(abs(delta2)), worse = mean(worse))

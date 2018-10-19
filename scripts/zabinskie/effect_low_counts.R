# ## ---- count_error
# all_fos <- colSums(fos_counts)
# 
# count_sums <- rowSums(fos_counts)
# 
# all_fos <- spp %>% 
#   slice(nrow(spp)) %>% 
#   select_if(. > 0) %>% 
#   unlist()
# 
# mod <- WAPLS(sqrt(spp), env)
# nrep <- 1000
# size_sd <- map_df(min(count_sums):70, function(size){
#   sim <- rmultinom(nrep, size = size, prob = all_fos)/size * 100
#   sim<- t(sim)
#   data_frame(size = size, sd = sd(predict(mod, sqrt(sim))$fit[, 2]))
# })
# 
# mod_nls <- nls(sd ~ a + b/size^c, data = size_sd, start = list(a = 0, b = 5, c = 0.5))
# all_pred <- data.frame(size = min(count_sums):70, sd = predict(mod_nls))
# 
# count_error_plot <- size_sd %>% ggplot(aes(x = size, y = sd)) + 
#   geom_point() + 
#   geom_line(data = all_pred, colour = "blue") +  
#   ylim(0, NA) +
#   labs(x = "Count sum", y = "Standard deviation °C")
# 
# est_error <- predict(mod_nls, newdata = data_frame(size = rowSums(fos_counts))) %>%
#   mean()
# 
# # est_error <- predict(mod_nls, newdata = data_frame(size = rowSums(fos_counts)))^2 %>%
# #   mean() %>% sqrt() # should probably be do like this but diff is small and this is more pessimistic

## ---- fossil_residual_sd
# 
# 1 - (1 - pnorm(1)) * 2
# 1 - (1 - pnorm(1.96)) * 2
# 
# 1 - (1 - pnorm(1.38)) * 2
# 1.3/1.38
# 
# mean(abs(rnorm(n = 10000, mean = 0, sd = 1.3/1.38)) < 1.3) 

fos_residuals_sd <- function(recon, instrumental_temperature){
  recon %>% 
    full_join(instrumental_temperature) %>% 
    mutate(resid = temperature - old) %>% 
    summarise(sd  = sd(resid))
  
}
# ## ---- count_residual
# recon %>% 
#   mutate(count = rowSums(fos_counts)) %>% 
#   full_join(zab) %>% 
#   mutate(resid = temperature - Aug) %$% 
#   cor.test(abs(resid), count)

# ## ---- large_counts
# 
# size_sd2 <- map_df(seq(5, 200, 5), function(size){
#   sim <- rmultinom(1000, size = size, prob = all_fos)/size * 100
#   sim<- t(sim)
#   data_frame(size = size, sd = sd(predict(mod, sqrt(sim))$fit[, 2]))
# })
# 
# mod_nls2 <- nls(sd ~ a + b/size^c, data = size_sd2, start = list(a = 0, b = 5, c = 1))
# mod_nls2
# all_pred2 <- data.frame(size = seq(5, 200, 5), sd = predict(mod_nls2))
# 
# size_sd2 %>% ggplot(aes(x = size, y = sd)) +
#   geom_point() + 
#   geom_line(data = all_pred2, colour = "blue") +
#   ylim(0, NA)
# 
# size_sd2 %>% mutate(pred = all_pred2$sd, resid = pred - sd) %>% 
#   ggplot(aes(x = size, y = resid)) + 
#   geom_point() # not a perfect fit - small ~quadratic term needed
# 
# 
# data_frame(spp = names(all_fos), abun = all_fos) %>% 
#   arrange(desc(abun)) %>%mutate(n = 1:n()) %>%
#   mutate(n = 1:n()) %>%
#   ggplot(aes(x = n, y = abun)) + geom_col() + scale_y_log10()
# 

## ---- count_error2
count_error <- function(spp, env, sites, estimated_countsum, fos_counts)  {
  spp100 <- spp %>% #use sites with apparent counts of more than 100
    bind_cols(sites %>% select(Lake)) %>%
    filter(estimated_countsum$est_n >= 100) %>%
    gather(key = taxon, value = perc, -Lake) %>% 
    filter(perc > 0) %>% 
    group_by(Lake) 
  
  count_sums <- rowSums(fos_counts)
  
  nrep <- 500

  spp100_size_sd <- spp100 %>% do({
    size_sd <- map_df(count_sums, function(size){
      p <- setNames(.$perc, .$taxon) 
      sim <- rmultinom(nrep, size = size, prob = p)/size * 100
      sim<- t(sim)
      focal <- sites$Lake == .$Lake[1]
      mod <- WAPLS(sqrt(spp[-focal, ]), env[-focal])
      data_frame(size = size, sd = sd(predict(mod, sqrt(sim))$fit[, 2]))
    })
    size_sd
  })
  spp100_size_sd %>% ungroup %>% summarise(m = mean(sd)) %>% pull(m)
}



# ## ---- no_singletons
# nrep <- 10000
# no_singletons <- spp100 %>% do({
#   p <- setNames(.$perc, .$taxon)
#   sim30 <- rmultinom(nrep, size = 30, prob = p)
#   sim50 <- rmultinom(nrep, size = 50, prob = p)
#   data_frame(
#     mean30 = mean(colSums(sim30 == 1) > 0),
#     mean50 = mean(colSums(sim50 == 1) > 0)
#   )
# })
# 
# no_singletons %>% ggplot(aes(x = mean30 * 100)) + geom_histogram(center = 0)
# no_singletons %>% ggplot(aes(x = mean50 * 100)) + geom_histogram(center = 0)
# no_singletons %>% filter(mean30 < 0.98)
# no_singletons %>% filter(mean50 < 0.98)
# spp100 %>% semi_join(no_singletons %>% filter(mean50 < 0.98)) %>% arrange(Lake) %>% print(n = Inf)
# 
# spp100 %>% 
#   mutate(none = Lake %in% (filter(no_singletons, mean50 < 0.98) %>% pull(Lake))) %>% 
#   ggplot(aes(x = n, y = perc, group = Lake, colour  = none)) + 
#   geom_line(show.legend = FALSE) + 
#   scale_y_log10()
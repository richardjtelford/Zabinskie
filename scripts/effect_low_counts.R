## ---- count_error
library("nlme")

all_fos <- colSums(fos_counts)
count_sums <- rowSums(fos_counts)

mod <- WAPLS(sqrt(spp), env)
nrep <- 1000
size_sd <- map_df(min(count_sums):80, function(size){
  sim <- rmultinom(nrep, size = size, prob = all_fos)/size * 100
  sim<- t(sim)
  data_frame(size = size, sd = sd(predict(mod, sqrt(sim))$fit[, 2]))
})

mod_nls <- nls(sd ~ a + b/size^c, data = size_sd, start = list(a = 0, b = 5, c = 1))
all_pred <- data.frame(size = min(count_sums):80, sd = predict(mod_nls))

count_error_plot <- size_sd %>% ggplot(aes(x = size, y = sd)) + 
  geom_point() + 
  geom_line(data = all_pred, colour = "blue") +  
  ylim(0, NA) +
  labs(x = "Count sum", y = "Standard deviation °C")

est_error <- predict(mod_nls, newdata = data_frame(size = rowSums(fos_counts))) %>%
  mean()

# est_error <- predict(mod_nls, newdata = data_frame(size = rowSums(fos_counts)))^2 %>%
#   mean() %>% sqrt() # should probably be do like this but diff is small and this is more pessimistic

## ---- fossil_residual_sd
# 
# 1 - (1 - pnorm(1)) * 2
# 1 - (1 - pnorm(1.96)) * 2
# 
# 1 - (1 - pnorm(1.38)) * 2
# 1.3/1.38
# 
# mean(abs(rnorm(n = 10000, mean = 0, sd = 1.3/1.38)) < 1.3) 

zab <- read.table("data/instrumental.txt") %>% 
  select(1:2) %>% 
  set_names(c("year", "Aug")) %>% 
  mutate(year = round(year))

fos_sd <- recon %>% 
  full_join(zab) %>% 
  mutate(resid = temperature - Aug) %>% 
  summarise(sd  = sd(resid))

## ---- count_residual
recon %>% 
  mutate(count = rowSums(fos_counts)) %>% 
  full_join(zab) %>% 
  mutate(resid = temperature - Aug) %$% 
  cor.test(abs(resid), count)

## ---- large_counts

size_sd2 <- map_df(seq(10, 2000, 10), function(size){
  sim <- rmultinom(1000, size = size, prob = all_fos)/size * 100
  sim<- t(sim)
  data_frame(size = size, sd = sd(predict(mod, sqrt(sim))$fit[, 2]))
})

mod_nls2 <- nls(sd ~ a + b/size^c, data = size_sd2, start = list(a = 0, b = 5, c = 1))
mod_nls2
all_pred2 <- data.frame(size = seq(10, 2000, 10), sd = predict(mod_nls2))

size_sd2 %>% ggplot(aes(x = size, y = sd)) +
  geom_point() + 
  geom_line(data = all_pred2, colour = "blue") +
  ylim(0, NA)

size_sd2 %>% mutate(pred = all_pred2$sd, resid = pred - sd) %>% 
  ggplot(aes(x = size, y = resid)) + 
  geom_point() # not a perfect fit - small ~quadratic term needed






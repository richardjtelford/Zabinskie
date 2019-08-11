
N <-seq(5, 100, 5)
nrep <- 1000
cors <- plyr::ldply(N, function(n){ 
  tibble(n = n, cor = replicate(nrep, cor(rnorm(n), rnorm(n))))
  })

s100 <- replicate(nrep, {
  df <- tibble(x = rnorm(110), y = rnorm(110)) %>% 
   # zoo::rollmean(k = 3, align = "left", na.pad = TRUE) %>% 
    zoo::rollmean(k = 3, align = "right", na.pad = TRUE)
   df <- df[5:104, ]
    #df <- df[5:94, ]
  cor(df[,"x"], df[, "y"])  
})

set.seed(42)
df <- tibble(x = rnorm(120), y = rnorm(120)) %>% 
  #zoo::rollmean(k = 3, align = "left", na.pad = TRUE) %>% 
  zoo::rollmean(k = 3, align = "right", na.pad = TRUE) %>% 
  as_tibble() %>% 
  slice(5:104) %>%# cor()
  rowid_to_column("pos") 

  df %>% gather(key = key, value = value, -pos) %>% 
  ggplot(aes(x = pos, y = value, colour = key)) + geom_line()

cors %>%
  ggplot(aes(x = n, y = cor, group = n)) + 
  geom_violin() +
  geom_violin(data = tibble(cor = s100, n = 110), fill = "red")


cors %>% 
  group_by(n) %>% 
  summarise(sd = sd(cor)) %>% 
  ggplot(aes(x = n, y = sd)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = sd(s100), colour = "red")


cors %>% 
  group_by(n) %>% 
  summarise(q95 = quantile(cor, prob = 0.95)) %>% 
  ggplot(aes(x = n, y = q95)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = quantile(s100, prob = 0.95), colour = "red")

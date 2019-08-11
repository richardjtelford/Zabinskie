r_p <- function(r, n, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  df <- n - 2
  STAT <- r/sqrt((1 - r^2)/df)
  PVAL <- switch(alternative, 
                 less = pt(STAT, df), 
                 greater = pt(STAT, df, lower.tail = FALSE), 
                 two.sided = 2 * min(pt(STAT, df), pt(STAT, df, lower.tail = FALSE)))
  return(PVAL)
}




res <- plyr::rdply(100, {
  r = 0.3
  
  #simulate white noise climate and proxy
  x <- MASS::mvrnorm(101, mu = c(0, 0), 
                     Sigma = cbind(c(1, r), c(r, 1)), 
                     empirical = FALSE) %>% 
    as_tibble() %>% 
    set_names(c("climate", "proxy"))
  
  
  #age-depth error
  x <- x %>% 
    mutate(proxy = proxy[c(1:50, 52:101, NA)]) %>% 
    slice(1:100)
  
  xs <- x %>% zoo::rollmean(k = 3, na.pad = TRUE) %>% 
    as_tibble()
  
  xst <- xs[c(FALSE, TRUE, FALSE), ]
  
  r1 <- ar(na.omit(xs$climate), order.max = 1)$ar
  r2 <- ar(na.omit(xs$proxy), order.max = 1, aic = FALSE)$ar
  DFeff <- (length(na.omit(xs$climate)) - 2) * (1 - r1 * r2) / (1 + r1 * r2) 
  
  tibble(
    expected = r,
    raw = with(x, cor(climate, proxy, use = "pair")),
    smoothed = with(xs, cor(climate, proxy, use = "pair")),
    thinned = with(xst, cor(climate, proxy, use = "pair")), 
    rawp = r_p(raw, nrow(x)),
    smoothedp = r_p(smoothed, DFeff + 2),
    thinnedp = r_p(thinned, nrow(xst))
    
  )
})

res %>% select(matches("p$")) %>% 
  gather(key = method, value = correlation) %>% 
  ggplot(aes(x = method, y = correlation)) + 
  geom_violin() + 
  geom_hline(yintercept = 0.05, colour = "grey50", linetype = "dashed") +
  scale_y_log10()

res %>% select(-matches("p$")) %>% 
  gather(key = method, value = correlation, -.n, -expected) %>% 
  ggplot(aes(x = method, y = correlation)) + 
  geom_violin()


quantile(res$rawp)
quantile(res$smoothedp)
quantile(res$thinnedp)

cor(xs$climate[1:50], xs$proxy[1:50], use = "pair")
cor(xs$climate[-(1:50)], xs$proxy[-(1:50)], use = "pair")
cor(x$climate[1:50], x$proxy[1:50], use = "pair")
cor(x$climate[-(1:50)], x$proxy[-(1:50)], use = "pair")

bind_cols(x, xs) %>% 
  mutate(d = 1:100) %>% 
  ggplot(aes(x = d)) +
  geom_line(aes(y = climate, colour = "raw")) +
  geom_line(aes(y = climate1, colour = "smo")) 

bind_cols(x, xs) %>% 
  mutate(d = 1:100) %>% 
  ggplot(aes(x = d)) +
  geom_line(aes(y = proxy, colour = "raw")) +
  geom_line(aes(y = proxy1, colour = "smo")) 



## --- check_on_JoPL_recon
#resolution
recon_jopl %>%  mutate(resolution = lag(Year) - Year) %>% 
  ggplot(aes(x = Year, y = resolution)) +
  geom_point()

## --- check_on_Holocene
#resolution
recon_holocene %>%  mutate(resolution = lag(Year) - Year) %>% 
  ggplot(aes(x = Year, y = resolution)) +
  geom_point()

#noccur
colSums(fos_holocene > 0)
mean(colSums(fos_holocene > 0) < 3)
#max abun
sapply(fos_holocene, max)

## ---- stratigraphy
fos_holocene %>% select_if(function(x){sum(x > 0) >= 10}) %>% 
  as.data.frame() %>%  
  strat.plot(yvar = recon_holocene$Year, scale.percent = TRUE, plot.line = FALSE, cex.xlabel = .7)

## ---- estimated_count
#assume min count == 1

min_pc <- apply(fos_holocene, 1, function(x)min(x[x>0]))
min_count <- 100/min_pc

lowcount <- mean(min_count < 30)

low_count_plot <- ggplot(recon_holocene, aes(x = Year, y = min_count)) + 
  geom_hline(yintercept = 30, colour = "red") +
  geom_point() +
  labs(x = "Year CE", y = "Estimated count")


## ---- select sample
fos_holocene %>% filter(recon_holocene$Year == 2001) %>% select_if(function(x){sum(x) > 0})

## ---- ordination
decorana(sqrt(fos_holocene))

silv_ca <- cca(sqrt(fos_holocene[, colSums(fos_holocene > 0) > 2]))
eigenvals(silv_ca)[1:2]/sum(eigenvals(silv_ca))

plot(silv_ca)


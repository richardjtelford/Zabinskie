## --- check_on_silvaplana2009

#estimated count Sum (assume min count == 1)

min_pc <- apply(silv2009, 1, function(x)min(x[x>0]))
min_count <- 100/min_pc

mean(min_count < 30)

ggplot(recon_silv2009, aes(x = Year, y = min_count)) + 
  geom_point() +
  geom_hline(yintercept = 30, colour = "red") +
  labs(y = "Estimated count")

ggplot(recon_silv2009, aes(x = min_count)) + 
  geom_histogram() +
  geom_vline(xintercept = 30)

## ordination
decorana(sqrt(silv2009))

silv_ca <- cca(sqrt(silv2009))
eigenvals(silv_ca)[1:2]/sum(eigenvals(silv_ca))




